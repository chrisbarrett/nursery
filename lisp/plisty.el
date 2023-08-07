;;; plisty.el --- Utilities for working with plists  -*- lexical-binding: t; -*-

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

;; Dinky little utility library for defining simple schemas for plists.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)
(require 'ht)

(defun plisty-keys (plist)
  (seq-map #'car (seq-partition plist 2)))

(defun plisty-pick (key-or-keys plist)
  (let ((keys (flatten-list (list key-or-keys)))
        (ht (ht-from-plist plist)))
    (ht-reject! (lambda (key _v) (not (seq-contains-p keys key)))
                ht)
    (ht-to-plist ht)))

(defun plisty-omit (key-or-keys plist)
  (let ((keys (flatten-list (list key-or-keys)))
        (ht (ht-from-plist plist)))
    (ht-reject! (lambda (key _v) (seq-contains-p keys key))
                ht)
    (ht-to-plist ht)))

(defun plisty-map-keys (fn plist)
  (let ((pairs (ht-map (lambda (k v)
                         (list (funcall fn k) v))
                       (ht-from-plist plist))))
    (apply 'append pairs)))

(defun plisty-merge (p1 p2)
  "Merge two plists, such that keys in P2 override duplicates in P1."
  (let* ((h1 (ht-from-plist p1))
         (h2 (ht-from-plist p2))
         (merged (ht-merge h1 h2)))
    (ht-to-plist merged)))

(defun plisty-p (obj)
  "Return t if OBJ is a list and appears to be a plist with keyword keys."
  (and (listp obj)
       (cl-evenp (length obj))
       (seq-every-p #'keywordp (plisty-keys obj))))

(defun plisty-equal (p1 p2)
  "Test whether two plists P1 & P2 are structurally equal.

Values are compared using `equal', except directly nested plists,
which are compared using `plist-equal' recursively."
  (cl-assert (plisty-p p1) t)
  (cl-assert (plisty-p p2) t)
  (catch 'not-equal
    (when (equal (length p1) (length p2))
      (dolist (key (plisty-keys p1))
        (let ((v1 (plist-get p1 key))
              (v2 (plist-get p2 key)))
          (cond
           ((equal v1 v2))
           ((and (plisty-p v1) (plisty-p v2) (plisty-equal v1 v2)))
           (t
            (throw 'not-equal nil)))))
      t)))

(defun plisty--pred-name-for-type (type)
  (intern (format "%s-p" type)))

(defmacro plisty-define-predicate (type required-keys all-keys)
  (cl-assert (symbolp type))
  (cl-assert (listp required-keys))
  (cl-assert (seq-every-p #'keywordp required-keys))
  (cl-assert (seq-every-p #'keywordp all-keys))
  `(defun ,(plisty--pred-name-for-type type) (value &optional strict)
     (when (listp value)
       (let ((required-keys ',required-keys)
             (all-keys ',all-keys))
         (let ((keys (plisty-keys value)))
           (and (null (seq-difference required-keys keys))
                (seq-every-p (lambda (key)
                               (plist-get value key))
                             required-keys)
                (if strict
                    (null (seq-difference keys all-keys))
                  t)))))))

(defun plisty--validator-for-type (type)
  (intern (format "%s-assert" type)))

(defmacro plisty-define-validator (type required-keys all-keys)
  (cl-assert (symbolp type))
  (cl-assert (listp required-keys))
  (cl-assert (seq-every-p #'keywordp required-keys))
  (cl-assert (seq-every-p #'keywordp all-keys))
  `(defun ,(plisty--validator-for-type type) (value &optional strict)
     (cl-assert (listp value) t "Expected a plist" )
     (let ((required-keys ',required-keys)
           (all-keys ',all-keys)
           (keys (plisty-keys value)))
       (cl-assert (null (seq-difference required-keys keys)) t "Missing required keys: %s" (seq-difference required-keys keys))
       (cl-assert (seq-every-p (lambda (key)
                                 (plist-get value key))
                               required-keys) t
                               "Illegal values for required keys: %s" (seq-filter (lambda (key)
                                                                                    (null (plist-get value key)))
                                                                                  required-keys))
       (when strict
         (cl-assert (null (seq-difference keys all-keys))
                    t
                    "Unexpected additional keys: %s"
                    (seq-difference keys all-keys)))
       value)))

(defun plisty--pred-accessor-name (type keyword)
  (intern (format "%s-%s" type (string-remove-prefix ":" (symbol-name keyword)))))

(defmacro plisty-define-getter (type key)
  (cl-assert (symbolp type))
  (cl-assert (keywordp key))
  (let ((validator (plisty--validator-for-type type)))
    `(defun ,(plisty--pred-accessor-name type key) (,type)
       ,(format "Lookup `%s' in a plist of type `%s'." key type)
       (when (fboundp ',validator)
         (,validator ,type))
       (plist-get ,type ,key))))

(defun plisty--format-create-fn-arglist (required optional)
  (if (or required optional)
      (format "\n\n\(fn &key %s)"
              (string-join (append (seq-map (lambda (it) (upcase (string-remove-prefix ":" (symbol-name it)))) required)
                                   (seq-map (lambda (it) (format "[%s]" (upcase (string-remove-prefix ":" (symbol-name it))))) optional))
                           " "))
    ""))

(defmacro plisty-define-create (type required optional)
  (cl-assert (symbolp type))
  (cl-assert (listp required))
  (cl-assert (listp optional))
  `(defun ,(intern (format "%s-create" type)) (&rest attrs)
     ,(format "Construct a value of type `%s'.%s"
              type (plisty--format-create-fn-arglist required optional))
     (,(plisty--validator-for-type type) attrs)
     (plisty-pick ',(-union required optional) attrs)))

(cl-defmacro plisty-define (type &key required optional)
  (declare (indent 1))
  (cl-assert (symbolp type))
  (cl-assert (listp required))
  (cl-assert (listp optional))
  (cl-assert (null (seq-intersection required optional)))
  (let ((keys (-union required optional)))
    `(progn
       (plisty-define-predicate ,type ,required ,keys)
       (plisty-define-validator ,type ,required ,keys)
       (plisty-define-create ,type ,required ,optional)
       ,@(seq-map (lambda (it) `(plisty-define-getter ,type ,it))
                  keys))))

(provide 'plisty)

;;; plisty.el ends here
