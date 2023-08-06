;;; org-roam-slipbox.el --- Teach org-roam how to handle multiple slipboxes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A 'slipbox' is a distinct folder of notes related to a specific topic or
;; context that should be differentiated from other notes in your Zettelkasten.
;; Broadly speaking, they represent mutually exclusive categories of notes.
;;
;; For example, you might maintain separate slipboxes for Evergreen Notes, notes
;; to do with your job or specific clients, and your dailies.

;; When searching notes, or using dynamic blocks to build lists of notes[1],
;; it's useful to have a tag corresponding to the slipbox to enable filtering.
;; This package hooks into org-roam's indexing so that a slipbox tag is
;; automatically applied when the note is indexed, saving you from having to add
;; the tag yourself. It also provides a function to 'refile' from one slipbox to
;; another[2] in a structured way.
;;
;; The slipbox tag is computed based on the name of the folder:
;;
;;   `org-roam-directory' -> tagged with `org-roam-slipbox-default'
;;      |
;;      |--- dailies  -> tagged with :dailies:
;;      |--- outlines -> tagged with :outlines:
;;      \--- work     -> tagged with :work:
;;
;; [1]: see `org-roam-dblocks'
;;
;; [2]: see `org-roam-slipbox-refile'.
;;

;;; Installation:

;;    (use-package org-roam-slipbox
;;      :after org-roam
;;      :demand t
;;      :config
;;      (org-roam-slipbox-buffer-identification-mode +1)
;;      (org-roam-slipbox-tag-mode +1))
;;
;; After enabling the mode, run `C-u M-x org-roam-db-sync' to rebuild your notes
;; index. Thereafter, the slipbox tag will automatically be applied at
;; indexing-time.

;;; Code:

(require 'f)
(require 'magit)
(require 'org-capture-detect)
(require 'org-roam)
(require 'org-roam-review)
(require 'org-roam-rewrite)

(defgroup org-roam-slipbox nil
  "Teach org-roam how to interpret multiple slipboxes."
  :group 'productivity
  :prefix "org-roam-slipbox-")

(defcustom org-roam-slipbox-default "notes"
  "A 'default' slipbox tag to apply for nodes not in a slipbox.

Nodes at the top-level of the org-roam directory will have this
tag applied."
  :group 'org-roam-slipbox
  :type '(choice (string :tag "Tag")
                 (const :tag "None" nil)))

(defcustom org-roam-slipbox-after-refile-hook nil
  "Hook run after a node is refiled via `org-roam-slipbox-refile'."
  :group 'org-roam-slipbox
  :type 'hook)

(defcustom org-roam-slipbox-mode-line-separator " > "
  "The separator string between the slipbox name and the node title."
  :group 'org-roam-slipbox
  :type 'string)

(defface org-roam-slipbox-name
  '((t
     (:inherit font-lock-string-face)))
  "Face for references to slipboxes."
  :group 'org-roam-slipbox)

(defface org-roam-slipbox-mode-line-separator
  '((t
     (:inherit comment)))
  "Face for the separator in the mode line string."
  :group 'org-roam-slipbox)

(defcustom org-roam-slipbox-use-git-p t
  "Whether to update git when modifying nodes in slipboxes."
  :group 'org-roam-slipbox
  :type 'boolean)



(defun org-roam-slipbox--sanitize-tag (str)
  (s-replace-regexp (rx (not (any alnum "_@#%"))) "_" str))

(defun org-roam-slipbox-from-file (file)
  (condition-case nil
      (let ((dir (directory-file-name (file-name-directory file))))
        (if (equal dir org-roam-directory)
            org-roam-slipbox-default
          (org-roam-slipbox--sanitize-tag (file-name-nondirectory dir))))
    (error org-roam-slipbox-default)))

(cl-defmethod org-roam-node-slipbox ((node org-roam-node))
  "Return the slipbox a NODE belongs to.

See also: `org-roam-slipbox-default'."
  (org-roam-slipbox-from-file
   ;; HACK: Work around org-roam-node-file being
   ;; nil during capture sequence.
   (if org-roam-capture--node
       (expand-file-name ".placeholder")
     (org-roam-node-file node))))

;; NOTE: Cannot use autoload magic comment directly on a defmethod.

;;;###autoload
(autoload 'org-roam-node-slipbox "org-roam-slipbox")

(defun org-roam-slipbox--rename-file-without-git (from to)
  "Move file FROM to TO, updating the file's buffer if open.

Adapted from `magit-file-rename', but with the git actions stripped out."
  (rename-file from to)
  (when-let* ((buf (get-file-buffer from)))
    (with-current-buffer buf
      (let ((buffer-read-only buffer-read-only))
        (set-visited-file-name to nil t)))))

(defun org-roam-slipbox--rename-file-with-magit (from to)
  (let ((repo-a (magit-toplevel (file-name-directory from)))
        (repo-b (magit-toplevel (file-name-directory to))))

    ;; Ensure the file is tracked by git.
    (magit-call-git "add" (magit-convert-filename-for-git from))

    (if (equal repo-a repo-b)
        (magit-file-rename from to)
      (let ((default-directory repo-b))
        (org-roam-slipbox--rename-file-without-git from to)
        (magit-call-git "add" (magit-convert-filename-for-git to))))))

(defun org-roam-slipbox--read (&optional current-slipbox)
  (let ((slipboxes (seq-difference (f-directories org-roam-directory)
                                   (list current-slipbox))))
    (completing-read "Slipbox: " slipboxes nil t)))

;;;###autoload
(defun org-roam-slipbox-refile (node slipbox)
  "Move NODE into SLIPBOX."
  (interactive (let* ((node (org-roam-node-at-point t))
                      (current-slipbox (org-roam-node-slipbox node)))
                 (list node (org-roam-slipbox--read current-slipbox))))

  (let ((current-slipbox (org-roam-node-slipbox node))
        dest)
    (cond
     ((zerop (org-roam-node-level node))
      (let ((file (org-roam-node-file node)))
        (setq dest (file-name-concat org-roam-directory slipbox (file-name-nondirectory file)))
        (if org-roam-slipbox-use-git-p
            (org-roam-slipbox--rename-file-with-magit file dest)
          (org-roam-slipbox--rename-file-without-git file dest))
        (org-roam-db-sync)))
     (t
      (let ((new-file (file-name-nondirectory (org-roam-rewrite--new-filename-from-capture-template node))))
        (setq dest (file-name-concat org-roam-directory slipbox new-file))
        (org-roam-rewrite-extract node dest))))

    (run-hooks 'org-roam-slipbox-after-refile-hook)

    (message (concat "Refiled from "
                     (propertize current-slipbox 'face 'org-roam-slipbox-name)
                     " to "
                     (propertize slipbox 'face 'org-roam-slipbox-name)))))

(defun org-roam-slipbox--ad-append-slipbox-tag (&optional _tags-only)
  (when-let* ((slipbox (ignore-errors (org-roam-slipbox-from-file (buffer-file-name)))))
    (add-to-list 'org-file-tags
                 ;; File-level properties should always have this text property,
                 ;; otherwise org shows the tag in the agenda, for instance.
                 (propertize slipbox 'inherited t))))

;;;###autoload
(define-minor-mode org-roam-slipbox-tag-mode
  "Automatically add a node's slipbox as a tag."
  :global t
  (cond
   (org-roam-slipbox-tag-mode
    (advice-add 'org-set-regexps-and-options :after #'org-roam-slipbox--ad-append-slipbox-tag))
   (t
    (advice-remove 'org-set-regexps-and-options #'org-roam-slipbox--ad-append-slipbox-tag))))



(defvar-local org-roam-slipbox--original-buffer-identification nil
  "Stores the original value of `mode-line-buffer-identification'.

This means titles can be restored if
`org-roam-slipbox-buffer-identification-mode' is toggled.")

;;;###autoload
(define-minor-mode org-roam-slipbox-buffer-identification-mode
  "Display the slipbox and node title as the buffer name."
  :global t
  (cond
   (org-roam-slipbox-buffer-identification-mode
    (add-hook 'org-mode-hook #'org-roam-slipbox--set-up-buffer-identification-mode)
    (add-hook 'org-roam-rewrite-node-renamed-hook #'org-roam-slipbox-update-buffer-identification)
    (add-hook 'org-roam-slipbox-after-refile-hook #'org-roam-slipbox-update-buffer-identification)

    (when (derived-mode-p 'org-mode)
      (org-roam-slipbox--set-up-buffer-identification-mode)))
   (t
    (remove-hook 'org-mode-hook #'org-roam-slipbox--set-up-buffer-identification-mode)
    (remove-hook 'org-roam-rewrite-node-renamed-hook #'org-roam-slipbox-update-buffer-identification)
    (remove-hook 'org-roam-slipbox-after-refile-hook #'org-roam-slipbox-update-buffer-identification)

    ;; Restore default buffer identification settings.
    (dolist (buf (seq-filter (lambda (it) (with-current-buffer it (derived-mode-p 'org-mode)))
                             (buffer-list)))
      (with-current-buffer buf
        (org-roam-slipbox-update-buffer-identification))))))

(defun org-roam-slipbox--set-up-buffer-identification-mode ()
  ;; Save the default buffer identification settings.
  (setq org-roam-slipbox--original-buffer-identification mode-line-buffer-identification)

  (unless (or org-inhibit-startup (org-capture-detect))
    (org-roam-slipbox-update-buffer-identification)
    (add-hook 'after-save-hook #'org-roam-slipbox-update-buffer-identification nil t)))

(defun org-roam-slipbox-update-buffer-identification ()
  (cond
   (org-roam-slipbox-buffer-identification-mode
    (when-let* ((node
                 (ignore-errors (save-excursion
                                  (goto-char (point-min))
                                  (org-roam-node-at-point)))))
      (setq-local mode-line-buffer-identification
                  (concat (propertize (org-roam-node-slipbox node) 'face 'org-roam-slipbox-name)
                          (propertize org-roam-slipbox-mode-line-separator 'face 'org-roam-slipbox-mode-line-separator)
                          (propertize (org-roam-node-title node) 'face 'mode-line-highlight 'help-echo (buffer-file-name))))))
   (t
    (setq-local mode-line-buffer-identification org-roam-slipbox--original-buffer-identification))))



;;;###autoload
(defun org-roam-slipbox-list-notes (slipbox)
  "List nodes belonging to SLIPBOX."
  (interactive (list (org-roam-slipbox--read)))
  (display-buffer
   (org-roam-review-create-buffer
    :title (concat "Notes for slipbox: " (propertize slipbox 'face 'org-roam-slipbox-name))
    :instructions "The nodes below are sorted by slipbox"
    :group-on (lambda (it)
                (or (org-roam-review--maturity-header it)
                    (cons "Others" 4)))
    :nodes
    (lambda ()
      (seq-filter (lambda (node)
                    (seq-contains-p (org-roam-node-tags node) slipbox))
                  (org-roam-node-list)))
    :sort #'org-roam-review-sort-by-title-case-insensitive)))

(provide 'org-roam-slipbox)

;;; org-roam-slipbox.el ends here
