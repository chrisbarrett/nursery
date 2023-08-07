;;; org-roam-gc.el --- Clean up empty roam files -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;; Homepage: https://github.com/chrisbarrett/nursery

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

;; Simple package that deletes dailies files that don't have anything in them.

;; Example configuration:
;;
;;     (use-package org-roam-gc
;;       :after org-roam
;;       :demand t
;;       :hook (org-mode . org-roam-gc-automatically))

;;; Code:

(require 'org-roam)
(require 'org-roam-dailies)

(defconst org-roam-gc-prompt-before-deleting-p nil
  "Whether to prompt before removing files when run interactively.")

(defconst org-roam-gc-debug nil
  "Whether to output extra messages for debugging purposes.")

(defun org-roam-gc--empty-content-p (buf)
  (with-current-buffer buf
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (equal 'property-drawer (org-element-type (org-element-at-point)))
          (org-forward-element))
        (while (and (equal 'keyword (org-element-type (org-element-at-point)))
                    (ignore-errors
                      (org-forward-element)
                      t)))
        (or (eobp)
            (string-blank-p (buffer-substring (1+ (point)) (point-max))))))))

(defun org-roam-gc--empty-file-content-p (file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-roam-gc--empty-content-p (current-buffer))))

(defun org-roam-gc-dailies-files ()
  (require 'org-roam)
  (require 'org-roam-dailies)
  (let ((path (expand-file-name org-roam-dailies-directory org-roam-directory)))
    (seq-filter #'file-regular-p (directory-files path t))))

(defmacro org-roam-gc--log (msg &rest args)
  `(when org-roam-gc-debug
     (message (concat "org-roam-gc: " ,msg) ,@args)
     nil))

(defun org-roam-gc--file-editing-p (file)
  (when-let* ((buf (find-buffer-visiting file)))
    (or (buffer-modified-p buf)
        (get-buffer-window-list buf))))

(defun org-roam-gc--remove-file (file confirm-p)
  (let ((file (expand-file-name file)))
    (cond
     ((org-roam-gc--file-editing-p file)
      (org-roam-gc--log "Skipping open file: %s" file))

     (t
      (org-roam-gc--log "Removing file: %s" file)
      (with-current-buffer (find-file file)
        (when (or (not confirm-p)
                  (y-or-n-p (format "Delete file `%s'? " (abbreviate-file-name file))))
          (kill-buffer)
          (delete-file file)))
      t))))

(defun org-roam-gc (&optional interactive)
  "Delete empty org-roam dailies.

Optional arg INTERACTIVE determines whether to query before
removing files."
  (interactive "p")
  (let ((count
         (thread-last (org-roam-gc-dailies-files)
                      (seq-filter #'org-roam-gc--empty-file-content-p)
                      (seq-filter (lambda (file)
                                    (org-roam-gc--remove-file file (and interactive
                                                                        org-roam-gc-prompt-before-deleting-p))))
                      (length))))
    (cond
     (interactive
      (message "Deleted %s file%s" count (if (eq 1 count) "" "s")))
     ((< 0 count)
      (message "org-roam-gc deleted %s file%s" count (if (eq 1 count) "" "s"))))))

(defun org-roam-gc--maybe-remove-this-file ()
  (when-let* ((file (buffer-file-name)))
    (cond
     ((not (derived-mode-p 'org-mode))
      (org-roam-gc--log "Skipping non-org file: %s" file))
     ((and (org-roam-gc--empty-content-p (current-buffer))
           (org-roam-dailies--daily-note-p))
      (org-roam-gc--log "Removing file: %s" file)
      (delete-file (buffer-file-name)))
     (t
      (org-roam-gc--log "Skipping file: %s" file)))))

;;;###autoload
(defun org-roam-gc-automatically ()
  (add-hook 'kill-buffer-hook #'org-roam-gc--maybe-remove-this-file nil t))

(provide 'org-roam-gc)

;;; org-roam-gc.el ends here
