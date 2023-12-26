;;; org-format.el --- Auto-format org buffers.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Adapted from: https://emacs.stackexchange.com/a/28268

;; Example configuration:
;;
;;   (use-package org-format
;;     :hook (org-mode . org-format-on-save-mode))

;;; Code:

(require 'org)
(require 'org-capture-detect)
(require 'thingatpt)

(defgroup org-format nil
  "Automatically format org buffers on save."
  :group 'productivity
  :prefix "org-format-")

(defcustom org-format-blank-lines-before-subheadings 1
  "Number of blank lines between a heading and preceding content.

Only applies to subheadings."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-first-heading 1
  "Number of blank lines between a heading and preceding content.

Only applies to first level-1 heading in the document, and
supercedes the setting for
`org-format-blank-lines-before-level-1-headings'."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-level-1-headings 1
  "Number of blank lines between a heading and preceding content.

Only applies to level-1 headings in the document."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-content 0
  "Number of blank lines after the heading line and any property drawers."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-meta 0
  "Number of blank lines between headers and subsequent planning & drawers."
  :group 'org-format
  :type 'integer)

(defcustom org-format-align-all-tables t
  "Whether to align tables on save."
  :group 'org-format
  :type 'boolean)



(defun org-format--ensure-empty-lines (n)
  (save-excursion
    (goto-char (line-beginning-position))
    (unless (bobp)
      (forward-char -1)
      (let ((start (point)))
        (when (search-backward-regexp (rx (not (any space "\n"))))
          (ignore-errors
            (forward-char 1)
            (delete-region (point) start))))
      (insert (make-string n ?\n)))))

(defun org-format--in-archived-heading-p ()
  (save-excursion
    (when (org-before-first-heading-p)
      (org-forward-heading-same-level 1))
    (let ((tags (org-get-tags)))
      (seq-contains-p tags org-archive-tag))))

(defun org-format--delete-blank-lines ()
  "Modified version of `delete-blank-lines'."
  (beginning-of-line)
  (when (looking-at "[ \t]*$")
    (delete-region (point)
                   (if (re-search-backward "[^ \t\n]" nil t)
                       (progn (forward-line 1) (point))
                     (point-min))))
  ;; Handle the special case where point is followed by newline and eob.
  ;; Delete the line, leaving point at eob.
  (when (looking-at "^[ \t]*\n\\'")
    (delete-region (point) (point-max))))

(defun org-format--headings (scope)
  (let ((seen-first-heading-p))
    (org-map-entries (lambda ()
                       ;; Widen so we can see space preceding the current
                       ;; headline.
                       (org-with-wide-buffer
                        (let* ((level (car (org-heading-components)))
                               (headline-spacing (cond
                                                  ((and (equal 1 level) (not seen-first-heading-p))
                                                   (setq seen-first-heading-p t)
                                                   org-format-blank-lines-before-first-heading)
                                                  ((equal 1 level)
                                                   org-format-blank-lines-before-level-1-headings)
                                                  (t
                                                   org-format-blank-lines-before-subheadings))))
                          (org-format--ensure-empty-lines headline-spacing)))

                       (unless (and (fboundp 'org-transclusion-within-transclusion-p)
                                    (org-transclusion-within-transclusion-p))
                         (forward-line 1)
                         (org-format--delete-blank-lines)
                         (org-format--ensure-empty-lines org-format-blank-lines-before-meta)
                         (org-end-of-meta-data t)
                         (org-format--ensure-empty-lines org-format-blank-lines-before-content)))
                     t
                     scope)))

(defun org-format--transclusions ()
  (while (search-forward-regexp (rx bol "#+transclude:") nil t)
    (save-excursion
      (unless (search-forward ":only-content" (line-end-position) t)
        (goto-char (line-beginning-position))
        (org-format--ensure-empty-lines org-format-blank-lines-before-subheadings)))))

;;;###autoload
(defun org-format-buffer ()
  "Format the current `org-mode' buffer."
  (interactive)
  (unless (org-capture-detect)
    (let ((scope (when (org-format--in-archived-heading-p)
                   ;; archive files can be enormous--just format the heading at
                   ;; point after archiving.
                   'tree)))
      (org-with-wide-buffer

       (when org-format-align-all-tables
         (org-table-map-tables #'org-table-align t))

       (org-format--headings scope)

       ;; Clean up trailing whitespace.
       (goto-char (point-max))
       (org-format--delete-blank-lines)

       ;; Format transcluded headings as if they were really there.
       (goto-char (point-min))
       (org-format--transclusions)))))

;; NB: Set this higher than the default to avoid interfering with things like
;; org-transclusion, etc.
(defvar org-format-on-save-mode-hook-depth 95)

;;;###autoload
(define-minor-mode org-format-on-save-mode
  "Minor mode to enable formatting on buffer save in `org-mode'."
  :lighter nil
  (cond
   (org-format-on-save-mode
    (add-hook 'before-save-hook 'org-format-buffer org-format-on-save-mode-hook-depth t))
   (t
    (remove-hook 'before-save-hook 'org-format-buffer t))))

(provide 'org-format)

;;; org-format.el ends here
