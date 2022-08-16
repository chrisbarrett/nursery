;;; plist.el --- Utilities for working with plists  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (ht "2.4"))

;; Author: Chris Barrett <chris@walrus.cool>

;; Homepage: https://github.com/chrisbarrett/nursery

;; Version: 0.0.1-pre

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

;; Dinky little utilitity library for parsing plists around with basic
;; structural validation.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)
(require 'ht)

(defun plist-keys (plist)
  (seq-map #'car (seq-partition plist 2)))

(defun plist-pick (key-or-keys plist)
  (let ((keys (flatten-list (list key-or-keys)))
        (ht (ht-from-plist plist)))
    (ht-reject! (lambda (key _v) (not (seq-contains-p keys key)))
                ht)
    (ht-to-plist ht)))

(defun plist-omit (key-or-keys plist)
  (let ((keys (flatten-list (list key-or-keys)))
        (ht (ht-from-plist plist)))
    (ht-reject! (lambda (key _v) (seq-contains-p keys key))
                ht)
    (ht-to-plist ht)))

(defun plist-map-keys (fn plist)
  (let ((pairs (ht-map (lambda (k v)
                         (list (funcall fn k) v))
                       (ht-from-plist plist))))
    (apply 'append pairs)))

(defun plist-merge (p1 p2)
  "Merge two plists, such that keys in P2 override duplicates in P1."
  (let* ((h1 (ht-from-plist p1))
         (h2 (ht-from-plist p2))
         (merged (ht-merge h1 h2)))
    (ht-to-plist merged)))

(defun plist-p (obj)
  "Return t if OBJ is a list and appears to be a plist with keyword keys."
  (and (listp obj)
       (cl-evenp (length obj))
       (seq-every-p #'keywordp (plist-keys obj))))

(defun plist-equal (p1 p2)
  "Test whether two plists P1 & P2 are structurally equal.

Values are compared using `equal', except directly nested plists,
which are compared using `plist-equal' recursively."
  (cl-assert (plist-p p1) t)
  (cl-assert (plist-p p2) t)
  (catch 'not-equal
    (when (equal (length p1) (length p2))
      (dolist (key (plist-keys p1))
        (let ((v1 (plist-get p1 key))
              (v2 (plist-get p2 key)))
          (cond
           ((equal v1 v2))
           ((and (plist-p v1) (plist-p v2) (plist-equal v1 v2)))
           (t
            (throw 'not-equal nil)))))
      t)))

(defun plist--pred-name-for-type (type)
  (intern (format "%s-p" type)))

(defmacro plist-define-predicate (type required-keys all-keys)
  (cl-assert (symbolp type))
  (cl-assert (listp required-keys))
  (cl-assert (seq-every-p #'keywordp required-keys))
  (cl-assert (seq-every-p #'keywordp all-keys))
  `(defun ,(plist--pred-name-for-type type) (value &optional strict)
     (when (listp value)
       (let ((required-keys ',required-keys)
             (all-keys ',all-keys))
         (let ((keys (plist-keys value)))
           (and (null (seq-difference required-keys keys))
                (seq-every-p (lambda (key)
                               (plist-get value key))
                             required-keys)
                (if strict
                    (null (seq-difference keys all-keys))
                  t)))))))

(defun plist--validator-for-type (type)
  (intern (format "%s-assert" type)))

(defmacro plist-define-validator (type required-keys all-keys)
  (cl-assert (symbolp type))
  (cl-assert (listp required-keys))
  (cl-assert (seq-every-p #'keywordp required-keys))
  (cl-assert (seq-every-p #'keywordp all-keys))
  `(defsubst ,(plist--validator-for-type type) (value &optional strict)
     (cl-assert (listp value) t "Expected a plist" )
     (let ((required-keys ',required-keys)
           (all-keys ',all-keys)
           (keys (plist-keys value)))
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
                    (seq-difference keys all-keys))))))

(defun plist--pred-accessor-name (type keyword)
  (intern (format "%s-%s" type (string-remove-prefix ":" (symbol-name keyword)))))

(defmacro plist-define-getter (type key)
  (cl-assert (symbolp type))
  (cl-assert (keywordp key))
  (let ((validator (plist--validator-for-type type)))
    `(defun ,(plist--pred-accessor-name type key) (,type)
       ,(format "Lookup `%s' in a plist of type `%s'." key type)
       (when (fboundp ',validator)
         (,validator ,type))
       (plist-get ,type ,key))))

(defun plist--format-create-fn-arglist (required optional)
  (if (or required optional)
      (format "\n\n\(fn &key %s)"
              (string-join (append (seq-map (lambda (it) (upcase (string-remove-prefix ":" (symbol-name it)))) required)
                                   (seq-map (lambda (it) (format "[%s]" (upcase (string-remove-prefix ":" (symbol-name it))))) optional))
                           " "))
    ""))

(defmacro plist-define-create (type required optional)
  (cl-assert (symbolp type))
  (cl-assert (listp required))
  (cl-assert (listp optional))
  `(defun ,(intern (format "%s-create" type)) (&rest attrs)
     ,(format "Construct a value of type `%s'.%s"
              type (plist--format-create-fn-arglist required optional))
     (,(plist--validator-for-type type) attrs)
     (plist-pick ',(-union required optional) attrs)))

(cl-defmacro plist-define (type &key required optional)
  (declare (indent 1))
  (cl-assert (symbolp type))
  (cl-assert (listp required))
  (cl-assert (listp optional))
  (cl-assert (null (seq-intersection required optional)))
  (let ((keys (-union required optional)))
    `(progn
       (plist-define-predicate ,type ,required ,keys)
       (plist-define-validator ,type ,required ,keys)
       (plist-define-create ,type ,required ,optional)
       ,@(seq-map (lambda (it) `(plist-define-getter ,type ,it))
                  keys))))

(provide 'plist)

;;; plist.el ends here
