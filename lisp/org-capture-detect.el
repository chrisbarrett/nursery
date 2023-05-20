;;; org-capture-detect.el --- Detect whether we're currently in an org-capture context  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Barrett

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
;;; Code:

(require 'org-capture)

(defvar org-capture-detect--in-org-capture-p nil)

(defun org-capture-detect ()
  (or (bound-and-true-p org-capture-mode)
      org-capture-detect--in-org-capture-p))

(define-advice org-capture (:around (fn &rest args) detect-capture)
  (let ((org-capture-detect--in-org-capture-p t))
    (apply fn args)))

(provide 'org-capture-detect)

;;; org-capture-detect.el ends here
