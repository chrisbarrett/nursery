;;; org-roam-search.el --- A search interface that works better with org-roam  -*- lexical-binding: t; -*-

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

;; Org-roam works best when your nodes are divided into many files, but this
;; makes the org-search functionality unsuitable. ripgrep does a better job, but
;; has the problem that it shows the raw filenames instead of the node title.

;; This implementation of search aims to surface matched text along with the
;; title of the relevant node.

;; This package exposes two commands, `org-roam-search' and
;; `org-roam-search-tags', that search your org-roam nodes with ripgrep and
;; display a buffer of matching nodes.

;; Example configuration:
;;
;;     (use-package org-roam-search
;;       :commands (org-roam-search)
;;       :general
;;       (:keymaps 'org-roam-mode-map :states '(normal motion)
;;        "s" 'org-roam-search))

;;; Code:

(require 'async)
(require 'dash)
(require 'magit-diff)
(require 'org-roam)
(require 'org-roam-review)
(require 'pcre2el)

(defgroup org-roam-search nil
  "Node search interface for org-roam."
  :group 'productivity
  :prefix "org-roam-search-")

(defcustom org-roam-search-ripgrep-program "rg"
  "Path to the ripgrep program for searching notes."
  :group 'org-roam-search
  :type 'string)

(defcustom org-roam-search-ripgrep-extra-flags '("--follow" "--smart-case" "--no-messages")
  "Extra flags to apply when searching via ripgrep."
  :group 'org-roam-search
  :type '(list string))

(defcustom org-roam-search-ignored-tags nil
  "A list of tags for nodes that should never be included in search results.

For instance, you might want never want to see dailies in your
search results. If you tagged them with a tag in this list they
would be excluded."
  :group 'org-roam-search
  :type '(list string))

(defvar org-roam-search-buffer-name "*org-roam-search*")
(defvar org-roam-search-tags-buffer-name "*org-roam-search-tags*")

(defface org-roam-search-highlight
  '((t
     (:inherit magit-diff-added-highlight)))
  "Face for highlighted results in the search buffer."
  :group 'org-roam-search)

(defface org-roam-search-query
  '((t
     (:inherit font-lock-string-face)))
  "Face for the search query in the header line."
  :group 'org-roam-search)




(defun org-roam-search--highlight-matches (regexp)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let ((transpiled-regexp (pcre-to-elisp regexp)))
        (while (search-forward-regexp transpiled-regexp nil t)
          (unless (seq-intersection (face-at-point nil t) '(magit-section-heading org-roam-review-instructions))
            (let ((overlay (make-overlay (let ((pt (match-beginning 0)))
                                           (goto-char pt)
                                           (min pt (or (car (save-match-data (bounds-of-thing-at-point 'word)))
                                                       (line-end-position))))
                                         (let ((pt (match-end 0)))
                                           (goto-char pt)
                                           (max pt (or (cdr (save-match-data (bounds-of-thing-at-point 'word)))
                                                       (line-beginning-position)))))))
              (overlay-put overlay 'face 'org-roam-search-highlight))))))))

(defun org-roam-search--match-previews (search-regexp node)
  (let ((hits))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents (org-roam-node-file node))
        (let ((org-inhibit-startup t))
          (org-mode))
        (goto-char (point-min))
        (org-roam-end-of-meta-data t)
        (while (search-forward-regexp search-regexp nil t)
          (let ((hit (list :pos (match-beginning 0)
                           :olp (ignore-errors (org-get-outline-path t t))
                           :preview
                           ;; Extracted from implementation of
                           ;; `org-roam-preview-get-contents'
                           (let ((s (funcall org-roam-preview-function)))
                             (dolist (fn org-roam-preview-postprocess-functions)
                               (setq s (funcall fn s)))
                             (org-roam-fontify-like-in-org-mode s)))))
            (push hit hits)))))
    (->> (nreverse hits)
         ;; Take the first hit from each outline
         (seq-group-by (lambda (it) (plist-get it :olp)))
         (ht-from-alist)
         (ht-map (lambda (_key values) (car values))))))

(defun org-roam-search-make-insert-preview-fn (search-regexp)
  (lambda (node)
    (let ((hits-in-file (org-roam-search--match-previews search-regexp node)))
      (cond
       (hits-in-file
        (--each-indexed hits-in-file
          (magit-insert-section section (org-roam-preview-section)
            (-let [(&plist :olp :preview :pos) it]
              (when (and olp (< 1 (length olp)))
                (let ((start (point))
                      (heading (propertize (string-join olp " > ") 'face 'org-roam-title)))
                  (insert heading)
                  (fill-region start (point))
                  (insert "\n")))
              (insert preview)
              (oset section file (org-roam-node-file node))
              (oset section point pos)
              (insert "\n\n")))))
       ((string-match-p search-regexp (org-roam-node-title node))
        (insert (propertize "(Matched title)" 'font-lock-face 'font-lock-comment-face))
        (insert "\n\n"))
       (t
        (magit-cancel-section))))))

(defvar org-roam-search-view-query-history nil)

(defun org-roam-search--ripgrep-for-nodes (query)
  (let ((reporter (make-progress-reporter "Searching nodes"))
        (files (ht-create))
        (ripgrep-args (append org-roam-search-ripgrep-extra-flags (list "--json" query org-roam-directory))))
    (async-wait
     (apply 'async-start-process "ripgrep" org-roam-search-ripgrep-program
            (lambda (_)
              (goto-char (point-min))
              (while (not (eobp))
                (progress-reporter-update reporter)
                (-when-let* ((line (buffer-substring (line-beginning-position) (line-end-position)))

                             ((parsed &as &plist :type)
                              (json-parse-string line :object-type 'plist))

                             ((&plist :data (&plist :path (&plist :text file) :absolute_offset pos))
                              (when (equal "match" type)
                                parsed))
                             (file (expand-file-name file org-roam-directory)))
                  (puthash file file files))
                (forward-line)))
            ripgrep-args))
    (progress-reporter-done reporter)
    (seq-filter (lambda (node)
                  (and (ht-get files (org-roam-node-file node))
                       (null (seq-intersection (org-roam-node-tags node)
                                               org-roam-search-ignored-tags))))
                (org-roam-node-list))))

;;;###autoload
(defun org-roam-search (query)
  "Search `org-roam-directory' for nodes matching a query.

QUERY is a PRCE regexp string that will be passed to ripgrep."
  (interactive (list
                (let* ((default (car org-roam-search-view-query-history))
                       (prompt (format "Search Roam%s: " (if default (format " (default \"%s\")" default) "")))
                       (input (string-trim (read-string prompt nil 'org-roam-search-view-query-history org-roam-search-view-query-history))))
                  (if (and (string-match-p (rx "|") input)
                           (not (string-prefix-p "(" input)))
                      (format "(%s)" input)
                    input))))
  (let ((nodes (org-roam-search--ripgrep-for-nodes query)))
    (display-buffer
     (org-roam-review-create-buffer
      :title (concat "Search Results: " (propertize query 'face 'org-roam-search-query))
      :placeholder "No search results"
      :buffer-name org-roam-search-buffer-name
      :nodes
      (lambda ()
        (seq-remove #'org-roam-review-node-ignored-p nodes))
      :render
      (-lambda ((&plist :nodes :placeholder :root-section))
        (cond
         ((null nodes)
          (insert placeholder)
          (newline))
         (t
          (pcase-dolist (`(,_file . ,group) (seq-group-by #'org-roam-node-file nodes))
            (when-let* ((top-node (-max-by (-on #'< #'org-roam-node-level)
                                           group) )
                        (node-id (org-roam-node-id top-node)))
              (magit-insert-section section (org-roam-node-section node-id t)
                (magit-insert-heading
                  (concat (funcall org-roam-review-title-formatter top-node)
                          " "
                          (when-let* ((mat (org-roam-review-node-maturity top-node)))
                            (alist-get mat org-roam-review-maturity-emoji-alist nil nil #'equal))))
                (oset section parent root-section)
                (oset section node top-node)
                (oset section washer
                      (lambda ()
                        (org-roam-review-insert-preview top-node)
                        (org-roam-search--highlight-matches query)
                        (magit-section-maybe-remove-visibility-indicator section))))))
          (org-roam-search--highlight-matches query))))))))

;;;###autoload
(defun org-roam-search-tags (query)
  "Search `org-roam-directory' for nodes matching a tags query.

QUERY is an `org-tags-filter'."
  (interactive (list (org-tags-filter-read "Search by tags filter (+/-): ")))
  (org-roam-review-modify-tags query t)
  (display-buffer
   (org-roam-review-create-buffer
    :title "Tag Search Results"
    :instructions "The list below contains nodes matching the given tags."
    :placeholder "No search results"
    :buffer-name org-roam-search-tags-buffer-name
    :sort #'org-roam-review-sort-by-title-case-insensitive)))

(provide 'org-roam-search)

;;; org-roam-search.el ends here
