;;; org-roam-refill-previews.el --- Refill org-roam backlink previews  -*- lexical-binding: t; -*-

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

;; Refills previews displayed in the org-roam buffer to aid reabability in
;; narrow window layouts.

;; Note that this only works when preview sections are lazily computed, or the
;; buffer is refreshed.

;; Example configuration:
;;
;;   (use-package org-roam-refill-previews
;;     :after org-roam
;;     :demand t
;;     :config
;;     (add-hook 'org-roam-preview-postprocess-functions #'org-roam-refill-previews))

;;; Code:

(require 'org)
(require 'org-roam)
(require 'seq)
(require 'subr-x)

(defgroup org-roam-refill-previews nil
  "Fill previews in the org-roam backlinks buffer."
  :group 'productivity
  :prefix "org-roam-refill-previews-")


(defcustom org-roam-refill-previews-justify-p t
  "Whether to justify preview text."
  :group 'org-roam-refill-previews
  :type 'boolean)

;;; Code:
(defun org-roam-refill-previews--window-width ()
  (if-let* ((win (seq-find
                  (lambda (it)
                    (with-selected-window it
                      (derived-mode-p 'org-roam-mode)))
                  (window-list))))
      (window-width win)
    fill-column))

(defun org-roam-refill-previews (preview-str)
  "Refill PREVIEW-STR to fit the backlinks window.

Expected to be appended to `org-roam-preview-postprocess-functions'."
  (let ((fill-column (org-roam-refill-previews--window-width)))
    (with-temp-buffer
      (insert (org-fontify-like-in-org-mode preview-str))
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (while (not (bobp))
        (ignore-errors
          (org-fill-element (when org-roam-refill-previews-justify-p 'justify)))
        (org-backward-paragraph))
      (buffer-string))))

(provide 'org-roam-refill-previews)

;;; org-roam-refill-previews.el ends here
