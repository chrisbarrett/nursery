;;; org-roam-rewrite.el --- Commands for rewriting org-roam nodes and their links  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Homepage: https://github.com/chrisbarrett/nursery

;; Author: Chris Barrett <chris+emacs@walrus.cool>

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

;; Provides commands for rewriting links and removing nodes in a structured way,
;; to reduce the likelihood of leaving broken links in your org-roam files.

;;; Code:

(require 'dash)
(require 'org)
(require 'org-roam)
(require 'plisty)

(require 'org-transclusion nil t)

(defgroup org-roam-rewrite nil
  "Commands for rewriting org-roam nodes and their links."
  :group 'productivity
  :prefix "org-roam-rewrite-")

(defcustom org-roam-rewrite-extract-excluded-tags '("ATTACH")
  "Tags that should not be propagated when extracting nodes."
  :group 'org-roam-rewrite
  :type '(repeat string))

(defcustom org-roam-rewrite-confirm-extraction-path-p nil
  "Whether to confirm the path to extract to with `org-roam-rewrite-extract'."
  :group 'org-roam-rewrite
  :type 'boolean)

(defcustom org-roam-rewrite-insert-link-after-extraction-p t
  "Whether to insert a link to nodes extracted with `org-roam-rewrite-extract'."
  :group 'org-roam-rewrite
  :type 'boolean)

(defcustom org-roam-rewrite-rename-without-confirmation-p nil
  "Whether to ask for confirmation before updating links to renamed nodes."
  :group 'org-roam-rewrite
  :type 'boolean)

(defcustom org-roam-rewrite-backlink-transformer #'org-roam-rewrite-backlink-transformer
  "Function to transform link data used to populate links in rewrites.

It should take a single argument, a plist conforming to
`org-roam-rewrite-backlink-transformer-args', and return a plist
conforming to `org-roam-rewrite-backlink-transformer-result'."
  :group 'org-roam-rewrite
  :type 'function)

(defcustom org-roam-rewrite-backlink-modified-functions nil
  "Hook function run after a backlink is modified.

It is called with the renamed link at point, and is passed two arguments:

1. a plist containing information about the link before and after
   the rename, but before the link transformer has possibly made
   modifications. It conforms to
   `org-roam-rewrite-backlink-transformer-args'.

2. a plist containing the data used to construct the new link. It
   conforms to `org-roam-rewrite-backlink-transformer-result'."
  :group 'org-roam-rewrite
  :type 'hook)

(defcustom org-roam-rewrite-backlinks-modified-in-file-functions nil
  "Hook run after processing backlinks in a file caused modifications.

It is called with a single argument: the file name that was modified."
  :group 'org-roam-rewrite
  :type 'hook)

(defcustom org-roam-rewrite-node-extracted-hook nil
  "Hook run after a node has been extracted successfully to a new file.

It is called with the new node as the current buffer."
  :group 'org-roam-rewrite
  :type 'hook)

(defcustom org-roam-rewrite-node-removed-functions nil
  "Hook functions run after a node has been removed.

It is called with a plist, containing the following attributes
from the original node: :title, :level, :file, :id"
  :group 'org-roam-rewrite
  :type 'hook)

(defcustom org-roam-rewrite-node-renamed-hook nil
  "Hook executed after renaming a node.

It is called with the renamed node as the current buffer."
  :group 'org-roam-rewrite
  :type 'hook)



(defun org-roam-rewrite--set-title-keyword (text)
  (org-with-wide-buffer
   (goto-char (point-min))
   (save-match-data
     (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol))
     (replace-match text t nil nil 1))))

(defun org-roam-rewrite--set-file-tags (tags)
  (org-with-wide-buffer
   (goto-char (point-min))
   (unless (search-forward-regexp (rx bol "#+filetags:" (group (* nonl))) nil t)
     (cond ((search-forward-regexp (rx bol "#+title:"))
            (goto-char (line-end-position))
            (insert "\n#+filetags:"))
           (t
            (insert "#+filetags:\n"))))

   (let ((formatted (if tags
                        (format ":%s:" (string-join tags ":"))
                      "")))
     (save-match-data
       (goto-char (point-min))
       (when (search-forward-regexp (rx bol "#+filetags:" (group (* nonl))))
         (replace-region-contents (match-beginning 1) (match-end 1)
                                  (lambda ()
                                    (concat " " formatted))))))))

(defun org-roam-rewrite--file-tags ()
  (save-match-data
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (search-forward-regexp (rx bol "#+filetags:" (group (+ nonl)))
                                  nil
                                  t)
       (split-string (string-trim (substring-no-properties (match-string 1))) ":" t)))))



(plisty-define org-roam-rewrite-backlink-transformer-args
  :required (:prev-node :new-node
             :prev-id :prev-desc
             :new-id :new-desc))

(plisty-define org-roam-rewrite-backlink-transformer-result
  :required (:id :desc))

(defun org-roam-rewrite-backlink-transformer (args-plist)
  (cl-labels ((normalise
               (str)
               (replace-regexp-in-string (rx (+ (any space "\n"))) ""
                                         (downcase str))))
    (-let* (((&plist :prev-node :new-id :new-desc :prev-desc)
             args-plist)

            (norm-titles (cons (normalise (org-roam-node-title prev-node))
                               (seq-map 'normalise (org-roam-node-aliases prev-node))))

            (desc-customised-p
             (not (seq-contains-p norm-titles (normalise prev-desc))))

            (updated-desc
             (if desc-customised-p prev-desc new-desc)))

      (list :id new-id :desc updated-desc))))

(defun org-roam-rewrite--parse-link-at-point ()
  (save-match-data
    (when (looking-at org-link-any-re)
      (-let* ((beg (match-beginning 0))
              (end (match-end 0))
              (str (buffer-substring-no-properties beg end))
              (((_link (&plist :path id) desc)) (org-element-parse-secondary-string str '(link))))
        (list :beg beg :end end :id id :desc (substring-no-properties desc))))))

(defun org-roam-rewrite--edit-backlinks (prev-node new-node new-desc)
  (let* ((backlinks-by-file
          (seq-group-by (-compose #'org-roam-node-file #'org-roam-backlink-source-node)
                        (org-roam-backlinks-get prev-node))))
    (pcase-dolist (`(,file . ,backlinks) backlinks-by-file)
      (with-temp-buffer
        (let ((modified-p))

          (insert-file-contents file)
          (dolist (backlink (seq-sort-by #'org-roam-backlink-point #'> backlinks))
            (goto-char (org-roam-backlink-point backlink))
            (-when-let* (((&plist :beg :end :id prev-id :desc prev-desc)
                          (org-roam-rewrite--parse-link-at-point))

                         (transformer-args
                          (org-roam-rewrite-backlink-transformer-args-create
                           :prev-node prev-node
                           :new-node new-node
                           :prev-id prev-id
                           :prev-desc prev-desc
                           :new-id (org-roam-node-id new-node)
                           :new-desc new-desc))
                         (transformed
                          (org-roam-rewrite-backlink-transformer-result-assert
                           (funcall org-roam-rewrite-backlink-transformer transformer-args)))

                         ((&plist :desc new-desc :id new-id) transformed))

              (replace-region-contents beg end (lambda ()
                                                 (org-link-make-string (concat "id:" new-id) new-desc)))
              (setq modified-p t)
              (run-hook-with-args 'org-roam-rewrite-backlink-modified-functions transformer-args transformed)))

          (write-region (point-min) (point-max) file)
          (when modified-p
            (run-hook-with-args 'org-roam-rewrite-backlinks-modified-in-file-functions file)))))

    (pcase-dolist (`(,file . ,_) backlinks-by-file)
      (when-let* ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (revert-buffer t t)))))

  ;; Tell org-roam that files changed behind its back.
  (org-roam-db-sync))

(defun org-roam-rewrite--update-node-title (node new-title)
  (org-id-goto (org-roam-node-id node))
  (cond ((equal 0 (org-roam-node-level node))
         (org-roam-rewrite--set-title-keyword new-title))
        ((looking-at org-complex-heading-regexp)
         (replace-match new-title t t nil 4)))
  (save-buffer))

(defun org-roam-rewrite--delete-node-kill-buffer (node)
  (let ((level (org-roam-node-level node))
        (file (org-roam-node-file node))
        (id (org-roam-node-id node)))
    (cond
     ((zerop level)
      (when-let* ((buf (find-buffer-visiting file)))
        (kill-buffer buf))
      (delete-file file))
     (t
      (let ((buffer-visiting-p (find-buffer-visiting file)))
        (org-with-point-at (org-roam-node-marker node)
          (goto-char (point-min))
          (when (search-forward-regexp (rx-to-string `(and
                                                       bol
                                                       (* space) ":ID:"
                                                       (* space)
                                                       ,id)))
            (let ((message-log-max))
              (org-cut-subtree)))
          (save-buffer)
          (unless buffer-visiting-p
            (kill-buffer))))))
    (run-hook-with-args 'org-roam-rewrite-node-removed-functions
                        (list :title (org-roam-node-title node) :id id :file file :level level))))

(defun org-roam-rewrite--node-formatted-title (node &optional default)
  (if org-roam-node-formatter
      (funcall org-roam-node-formatter node)
    (or default
        (org-roam-node-title node))))

;;;###autoload
(defun org-roam-rewrite-rename (node new-title)
  "Change the title of a node and update links to match.

NODE is the node to update.

NEW-TITLE is the new title to use. All backlinks will have their
descriptions updated according to the behaviour of the function
bound to variable `org-roam-rewrite-backlink-transformer'."
  (interactive (let ((node (or (org-roam-node-at-point) (org-roam-node-read))))
                 (list node (read-string "New title: " (org-roam-node-title node)))))
  (org-roam-node-visit node)
  (org-save-all-org-buffers)
  (org-roam-rewrite--update-node-title node new-title)
  (let* ((node-id (org-roam-node-id node))
         ;; Get an updated node with the new title.
         (updated-node (org-roam-node-from-id node-id))
         (backlinks (org-roam-backlinks-get node)))
    (cond
     ((null backlinks)
      (message "Renamed. No backlinks to update."))
     (t
      (cond ((or org-roam-rewrite-rename-without-confirmation-p
                 (y-or-n-p (format "Modify %s backlink description%s? "
                                   (length backlinks)
                                   (if (= 1 (length backlinks)) "" "s"))))

             (let ((new-desc (org-roam-rewrite--node-formatted-title updated-node new-title)))
               (org-roam-rewrite--edit-backlinks node updated-node new-desc))

             (message "Rewrote %s link%s to node."
                      (length backlinks)
                      (if (= 1 (length backlinks)) "" "s")))
            (t
             (message "Rename completed.")))))
    (run-hooks 'org-roam-rewrite-node-renamed-hook)))

;;;###autoload
(defun org-roam-rewrite-remove (from to link-desc)
  "Redirect links from one node to a replacement node.

Optionally, delete the original node after all links are
redirected.

FROM is the node which will be unlinked.

TO is the node to change those references to point to.

LINK-DESC is the description to use for the updated links."
  (interactive (let* ((from (org-roam-node-at-point t))
                      (backlinks (progn
                                   (org-save-all-org-buffers)
                                   (org-roam-backlinks-get from))))
                 (if (zerop (length backlinks))
                     (list from nil nil)
                   (let* ((to (org-roam-node-read nil (lambda (it) (not (equal from it))) nil t "Rewrite to: "))
                          (desc (read-string "Link description: " (org-roam-rewrite--node-formatted-title to))))
                     (list from to desc)))))
  (let ((backlinks (org-roam-backlinks-get from)))
    (cond
     ((null backlinks)
      (when (y-or-n-p "No links found. Delete node? ")
        (org-roam-rewrite--delete-node-kill-buffer from)))

     ((or (null to) (null link-desc))
      (user-error "Must provide a node to redirect existing links to"))

     ((y-or-n-p (format "Rewriting %s link%s from \"%s\" -> \"%s\". Continue? "
                        (length backlinks)
                        (if (= 1 (length backlinks)) "" "s")
                        (org-roam-node-title from)
                        link-desc))
      (org-roam-rewrite--edit-backlinks from to link-desc)
      (when (y-or-n-p "Rewrite completed. Delete node? ")
        (org-roam-rewrite--delete-node-kill-buffer from)))
     (t
      (user-error "Rewrite aborted")))))

(defmacro org-roam-rewrite--when-transclusions (&rest body)
  (declare (indent 0))
  `(when (bound-and-true-p org-transclusion-mode)
     ,@body))

;;;###autoload
(defun org-roam-rewrite-inline (src-node dest-node)
  "Inline the contents of one org-roam node into another, removing the original.

SRC-NODE is the node to be removed.

DEST-NODE is the node that will be added to."
  (interactive
   (let* ((suggested-title (-some->> (org-roam-node-at-point) (org-roam-node-title)))
          (src (org-roam-node-read suggested-title nil nil t "Source: "))
          (dest (org-roam-node-read nil (lambda (node)
                                          (and
                                           (not (equal (org-roam-node-id node) (org-roam-node-id src)))
                                           (zerop (org-roam-node-level node))
                                           (not (seq-contains-p (org-roam-node-tags node) "dailies"))))
                                    nil t "Destination: ")))
     (list src dest)))

  (let* ((org-inhibit-startup t)
         (src-buffer (find-file-noselect (org-roam-node-file src-node)))
         (content
          (with-current-buffer src-buffer
            (org-with-wide-buffer
             (org-roam-rewrite--when-transclusions
               (org-transclusion-remove-all))
             (goto-char (point-min))
             (org-roam-end-of-meta-data t)
             (buffer-substring (point) (point-max))))))
    (find-file (org-roam-node-file dest-node))
    (org-with-wide-buffer
     (org-roam-rewrite--when-transclusions
       (org-transclusion-remove-all))
     (goto-char (point-max))
     (delete-blank-lines)
     (insert "\n\n")
     (insert (format "* %s\n" (org-roam-node-title src-node)))
     (org-set-property "ID" (org-roam-node-id src-node))
     (save-restriction
       (narrow-to-region (point) (point-max))
       (insert content)
       (org-map-entries 'org-do-demote)
       (goto-char (point-min))
       (while (search-forward-regexp (rx bol "#+transclude:") nil t)
         (org-roam-rewrite--when-transclusions
           (org-transclusion-add))
         (org-roam-rewrite--when-transclusions
           (org-transclusion-promote-subtree)))))
    (delete-file (org-roam-node-file src-node))
    (save-buffer)
    (org-roam-rewrite--when-transclusions
      (org-transclusion-add-all))
    (when (buffer-live-p src-buffer)
      (kill-buffer src-buffer)))

  (org-roam-node-visit dest-node)
  (message "Inlined node successfully"))

(defun org-roam-rewrite--ensure-node-for-headline-at-point ()
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (org-id-get-create)
    (save-buffer)
    (org-roam-db-update-file)
    (org-roam-node-at-point t)))

(defun org-roam-rewrite--new-filename-from-capture-template (node)
  (unwind-protect
      (progn
        (setq org-capture-plist nil)
        (org-roam-format-template
         (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
         (lambda (key default-val)
           (let ((fn (intern key))
                 (node-fn (intern (concat "org-roam-node-" key))))
             (cond
              ((fboundp fn)
               (funcall fn node))
              ((fboundp node-fn)
               (funcall node-fn node))
              (t
               (read-from-minibuffer (format "%s: " key) default-val)))))))
    (setq org-capture-plist nil)))

;;;###autoload
(defun org-roam-rewrite-extract (node dest)
  "Extract NODE to a new file at DEST.

Note that NODE must be a headline, not at the top-level of the
file. If NODE is at the top-level an error is signalled.

If called interactively, ensure the headline at point has an ID
before extracting.

This is a rough reimplementation of `org-roam-extract-subtree',
but it handles file titles, tags and transclusions better."
  (interactive (let* ((node (org-roam-rewrite--ensure-node-for-headline-at-point))
                      (template (org-roam-rewrite--new-filename-from-capture-template node))
                      (relpath (file-name-as-directory org-roam-directory))
                      (dest (expand-file-name
                             (if org-roam-rewrite-confirm-extraction-path-p
                                 (read-file-name "Extract node to: " relpath  template nil template)
                               template)
                             org-roam-directory)))
                 (list node dest)))

  (cl-assert (org-roam-node-level node) t)
  (cl-assert (not (zerop (org-roam-node-level node))) t)

  (with-current-buffer (find-file-noselect (org-roam-node-file node))
    (org-roam-rewrite--when-transclusions
      (org-transclusion-remove-all))

    ;; Use underlying org-mode machinery to go to the ID in the buffer. We can't
    ;; use org-roam-node-marker because updates aren't reliable.
    (org-with-point-at (org-id-find (org-roam-node-id node) t)
      (let ((tags (org-get-tags))
            (save-silently t)
            (dest-buf (find-file-noselect dest))
            extraction-succeeded-p)
        (unwind-protect
            (atomic-change-group
              ;; Extract from source buffer
              (org-cut-subtree)
              (save-buffer)
              (org-roam-db-update-file)
              (when org-roam-rewrite-insert-link-after-extraction-p
                (insert (org-link-make-string (format "id:%s" (org-roam-node-id node))
                                              (org-link-display-format (org-roam-rewrite--node-formatted-title node))))
                (newline))
              (org-roam-rewrite--when-transclusions
                (org-transclusion-add-all))

              ;; Insert into dest buffer
              (with-current-buffer dest-buf
                (org-paste-subtree)
                (while (> (org-current-level) 1) (org-promote-subtree))

                ;; `org-roam-promote-entire-buffer' expects an indexed node to
                ;; exist, and the file must exist on-disk for indexing to succeed.
                (let ((before-save-hook)
                      (after-save-hook
                       (lambda ()
                         (org-id-add-location (org-roam-node-id node) dest))))
                  (save-buffer))
                (org-roam-promote-entire-buffer)

                (let ((tags (-difference (-union (org-roam-rewrite--file-tags) tags)
                                         org-roam-rewrite-extract-excluded-tags)))
                  (org-roam-rewrite--set-file-tags tags)
                  (org-roam-rewrite--when-transclusions
                    (org-transclusion-add-all)))
                (save-buffer))

              (setq extraction-succeeded-p t))

          (unless extraction-succeeded-p
            (message "Extraction failed")
            (with-current-buffer dest-buf
              (let ((kill-buffer-query-functions))
                (set-buffer-modified-p nil)
                (kill-buffer dest-buf))
              (when (file-exists-p dest)
                (delete-file dest)))))

        (save-buffer)
        (with-current-buffer dest-buf
          (run-hooks 'org-roam-capture-new-node-hook 'org-roam-rewrite-node-extracted-hook))))))

(provide 'org-roam-rewrite)

;;; org-roam-rewrite.el ends here
