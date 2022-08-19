;;; org-roam-consult.el --- Search org-roam nodes with consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

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

;; This package exposes a command, `org-roam-consult', which is a version
;; of`consult-ripgrep' that shows the titles of org files rather than their
;; filepath. This is desirable when searching org-roam files, since filenames
;; may not correspond to a note's title.

;; Example configuration:
;;
;;     (use-package org-roam-consult
;;       :commands (org-roam-consult))

;;; Code:

(require 'consult)
(require 'org)
(require 'org-roam)
(require 'memoize)
(require 'pcre2el)

(defgroup org-roam-consult nil
  "Search org-roam nodes with consult."
  :group 'productivity
  :prefix "org-roam-consult-")


(defface org-roam-consult-highlight
  `((t (:inherit highlight)))
  "Face for hits for a search term."
  :group 'org-roam-consult)

(defvar org-roam-consult-title-search-byte-limit 1024
  "The max number of bytes to look at when trying to find a roam node's title.")



(defun org-roam-consult--replace-links-in-string (str)
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))

      ;; Replace links with their descriptions.
      (save-excursion
        (while (search-forward-regexp org-link-bracket-re nil t)
          (replace-match (match-string 2))))

      ;; Best-effort processing for remaining line-wrapped links
      (save-excursion
        (while (search-forward-regexp (rx "[[" (+? nonl) "][" (group (+? nonl)) (? "]")) nil t)
          (replace-match (match-string 1))))

      (buffer-substring (point-min) (point-max)))))

(defun org-roam-consult--candidate-group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (let* ((line (substring cand (1+ (length (get-text-property 0 'consult--grep-file cand)))))
         (filename (get-text-property 0 'consult--grep-file cand)))
    (if transform
        (org-roam-consult--replace-links-in-string line)
      (org-roam-consult--format-group-title filename))))

(defun org-roam-consult--lookup-title (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file org-roam-directory) nil nil org-roam-consult-title-search-byte-limit)
    (goto-char (point-min))
    (if (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol))
        (match-string 1)
      file)))

(defun org-roam-consult--format-group-title (file)
  (let ((title (org-roam-consult--lookup-title file))
        (dir (-some->> (file-name-directory file) (string-remove-prefix "/") (string-remove-suffix "/"))))
    (if (or (null dir) (string-blank-p dir))
        title
      (format "%s > %s" dir title))))

(ignore-errors
  (memoize 'org-roam-consult--format-group-title 60))

;; HACK: brutal copy-pasta to tweak two expressions in `consult--grep-format' to
;; make outputs more readable.
(defun org-roam-consult--format-results (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command argument builder."
  (let ((highlight))
    (lambda (action)
      (cond
       ((stringp action)
        (setq highlight (plist-get (funcall builder action) :highlight))
        (funcall async action))
       ((consp action)
        (let (result)
          (save-match-data
            (dolist (str action)
              (when (and (string-match consult--grep-match-regexp str)
                         ;; Filter out empty context lines
                         (or (/= (aref str (match-beginning 3)) ?-)
                             (/= (match-end 0) (length str))))
                (let* ((file (match-string 1 str))
                       (line (format "%4s" (match-string 2 str)))
                       (ctx (= (aref str (match-beginning 3)) ?-))
                       (sep (if ctx "-" " "))
                       (content (substring str (match-end 0)))
                       (file-len (length file))
                       (line-len (length line)))
                  (when (> (length content) consult-grep-max-columns)
                    (setq content (substring content 0 consult-grep-max-columns)))
                  (when highlight
                    (funcall highlight content))
                  (setq str (concat file sep line sep content))
                  ;; Store file name in order to avoid allocations in `consult--grep-group'
                  (add-text-properties 0 file-len `(face consult-file consult--grep-file ,file) str)
                  (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                  (when ctx
                    (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append str))
                  (push str result)))))
          (funcall async (nreverse result))))
       (t (funcall async action))))))

;;;###autoload
(defun org-roam-consult (&optional initial)
  "Search for regexp with rg in `org-roam-directory' with INITIAL input."
  (interactive)
  (let* ((default-directory org-roam-directory)
         (read-process-output-max (max read-process-output-max (* 1024 1024))))
    (consult--read
     (consult--async-command #'consult--ripgrep-builder
       (org-roam-consult--format-results #'consult--ripgrep-builder)
       :file-handler t)
     :prompt "Search Roam: "
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :initial (consult--async-split-initial initial)
     :add-history (consult--async-split-thingatpt 'symbol)
     :require-match t
     :category 'consult-grep
     :group #'org-roam-consult--candidate-group
     :history '(:input consult--grep-history)
     :sort nil)))

(provide 'org-roam-consult)

;;; org-roam-consult.el ends here
