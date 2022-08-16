;;; org-tags-filter.el --- Implements reading & parsing of a tags filter structure.  -*- lexical-binding: t; -*-

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

;; Defines a data structure to represent a filter against an org-roam-node's
;; tags, and provides functions to read and evaluate these filters.

;;; Code:

(require 'dash)
(require 'plisty)

(plisty-define org-tags-filter
  :optional (:required :forbidden))

(defun org-tags-filter-parse (input)
  ;; (org-tags-filter-parse nil)
  ;; (org-tags-filter-parse "")
  ;; (org-tags-filter-parse "hello there")
  ;; (org-tags-filter-parse "-hello there +obi +wan")
  ;; (org-tags-filter-parse '(-hello there "+obi" "+wan"))
  (-let* ((tokens
           (cond
            ((null input) nil)
            ((stringp input)
             (split-string input " " t))
            ((symbolp input)
             (list (symbol-name input)))
            ((listp input)
             (seq-map (lambda (it) (format "%s" it)) input))
            (t
             (error "Cannot parse as note filter: %s" input))))
          ((forbidden required) (-separate (lambda (it) (string-prefix-p "-" it)) tokens)))
    (org-tags-filter-create :forbidden (seq-map (lambda (it) (string-remove-prefix "-" it))
                                                forbidden)
                            :required (seq-map (lambda (it) (string-remove-prefix "+" it))
                                               required))))

(defun org-tags-filter-pp (tags-filter)
  (string-join (append
                (seq-map (lambda (it) (concat "-" it)) (org-tags-filter-forbidden tags-filter))
                (org-tags-filter-required tags-filter)) " "))

(defvar org-tags-filter-last-value nil)

(defun org-tags-filter-read (&optional prompt)
  (let* ((current-filter (org-tags-filter-pp org-tags-filter-last-value))
         (input (read-string (or prompt "Tags filter (+/-): ")
                             (unless  (string-blank-p current-filter)
                               (concat current-filter " "))
                             'org-roam-review-tags)))
    (org-tags-filter-parse input)))

(provide 'org-tags-filter)

;;; org-tags-filter.el ends here
