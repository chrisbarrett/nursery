;;; org-roam-lazy-previews.el --- Make previews in org-roam buffer lazy for better performance  -*- lexical-binding: t; -*-

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

;; Changes previews in the org-roam buffer to be lazily computed, improving
;; responsiveness in buffers with many backlinks and reflinks.
;;
;; Example configuration:
;;
;;   (use-package org-roam-lazy-previews
;;     :after org-roam
;;     :demand t)

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'org)
(require 'org-roam)
(require 'subr-x)

(autoload 'org-roam-review-insert-preview "org-roam-review")

(defgroup org-roam-lazy-previews nil
  "Change org-roam node previews to be lazy for performance."
  :group 'productivity
  :prefix "org-roam-lazy-previews-")

(defcustom org-roam-lazy-previews-title-formatter #'org-roam-lazy-previews-custom-title-formatter
  "A function to format a node's title for the backlinks buffer.

It is passed the same arguments as
`org-roam-node-insert-section', and should return a string."
  :group 'org-roam-lazy-previews
  :type 'function)

(cl-defun org-roam-lazy-previews-custom-title-formatter (&key source-node properties &allow-other-keys)
  (let* ((outline (when-let* ((outline (plist-get properties :outline)))
                    (mapconcat #'org-link-display-format outline " > ")))
         (title (org-roam-node-title source-node)))
    (concat (propertize title 'font-lock-face 'org-roam-title)
            (when (and outline (not (equal title outline)))
              (format " > %s" (propertize outline 'font-lock-face 'org-roam-olp))))))

(define-advice org-roam-node-insert-section (:override (&rest args) lazy-previews)
  (cl-destructuring-bind (&key source-node point &allow-other-keys) args
    (magit-insert-section section (org-roam-node-section (org-roam-node-id source-node) t)
      (magit-insert-heading (apply org-roam-lazy-previews-title-formatter args))
      (oset section node source-node)
      ;; KLUDGE: Mofified macro-expansion of `magit-insert-section-body' that
      ;; avoids unsetting the parent section's keymap.
      (oset section washer
            (lambda ()
              (org-roam-review-insert-preview source-node :point point)
              (magit-section-maybe-remove-visibility-indicator section))))))

(provide 'org-roam-lazy-previews)

;;; org-roam-lazy-previews.el ends here
