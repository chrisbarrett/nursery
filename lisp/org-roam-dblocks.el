;;; org-roam-dblocks.el --- Defines dynamic block types for org-roam  -*- lexical-binding: t; -*-

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

;; Defines dynamic block types for use with org-roam.
;;
;; Example configuration:
;;
;;    (use-package org-roam-dblocks
;;      :hook (org-mode . org-roam-dblocks-autoupdate-mode))

;; The dblock types defined are:
;;
;; - "backlinks": lists the backlinks for this node, with optional filter
;;   criteria.
;;
;;     E.g., in my TV Tropes note I have:
;;
;;     #+BEGIN: backlinks :match trope$
;;     - [[id:...][Advanced Ancient Humans Trope]]
;;     - [[id:...][Bizarre Alien Biology Trope]]
;;     - [[id:...][Evil Brit Trope]]
;;     - [[id:...][Humans Are Bastards Trope]]
;;     - [[id:...][Lost Superweapon Trope]]
;;     - [[id:...][Mega-Corporations Trope]]
;;     - [[id:...][One-Man-Army Trope]]
;;     - [[id:...][Precursor Alien Civilisation Trope]]
;;     - [[id:...][Scary Dogmatic Aliens Trope]]
;;     - [[id:...][Sealed Evil in a Can Trope]]
;;     #+END:
;;
;; - "notes": lists org-roam notes based on filter criteria.
;;
;;     E.g. A block that collects open questions in my Zettelkasten:
;;
;;     #+BEGIN: notes :match (rx "?" eos) :tags (-answered -snooze -outline)
;;     - [[id:...][Are Alien and Blade Runner in the same universe?]]
;;     - [[id:...][Can attention span be increased through training?]]
;;     - [[id:...][Is there research supporting the claimed benefits of the Pomodoro Technique?]]
;;     #+END:

;; Options:
;;
;; - :only-missing, when non-nil, excludes links already in the node from the
;;   block.
;;
;;   As a use-case, you might use this to search for nodes that haven't been
;;   worked into the text of the current node.
;;
;; - :include-same-file, when non-nil, includes backlinks from nodes within the
;;   same file.

;; Implemented filters:
;;
;; - :match, which matches note titles (case-insensitively).

;;     A match filter must be an `rx' form or regexp string. String
;;     double-quotes may be safely omitted for regexps that are just a single
;;     alphanumeric word.
;;
;;     Examples:
;;     - foo, "foo", (rx "foo")
;;     - "foo bar", (rx "foo bar")
;;     - "[?]$", (rx "?" eol)
;;
;;     If the match contains a capture group, the text in that group is used for
;;     the link description. E.g., to extract the text after "Prefix - ", you'd
;;     write one of:

;;     - :match "^Prefix - \\(.+\\)"
;;     - :match (rx bol "Prefix - " (group (+ nonl)))
;;
;; - :tags, which matches the note's headline and file tags.
;;
;;     A tags filter must be a single tag (double-quotes optional) or a list of
;;     tags. Each tag may be preceded by a minus sign to indicate a forbidden tag,
;;     or a plus symbol to indicate a required tag. Tags are interpreted to be
;;     required if neither +/- is specified.
;;
;;     Examples of tags matches:
;;     - required: foo, "foo", +foo, "+foo"
;;     - forbidden: -foo, "-foo"
;;     - multiple tags (and-ed together): (foo "+bar" -baz)
;;
;; - :filter, and its logical opposite :remove, provide a generic way to decide
;;   which nodes to include.
;;
;;      A filter can be a symbol, which is interpreted to be a function name, a
;;      lambda expression, or a bare S-expression.
;;
;;      When a function or lambda expression is provided, it will be called on
;;      each node to decide whether to include that node in results. The given
;;      function should accept a single argument, which is an `org-roam-node'.
;;
;;      Otherwise, the form is interpreted to be an 'anaphoric' S-expression,
;;      where the symbol `it' is bound to an `org-roam-node', before being
;;      evaluated.
;;
;;      Examples:
;;      - my-predicate
;;      - (lambda (node) (zerop (org-roam-node-level node)))
;;      - (zerop (org-roam-node-level it))
;;
;;      For convenience, the slots on an org-roam node are bound within an
;;      anaphoric predicate. This allows you to rewrite:
;;
;;        (zerop (org-roam-node-level it))
;;
;;      As the more convenient:
;;
;;        (zerop level)
;;
;;      If :filter and :remove are both provided, they are logically and-ed.
;;
;; - :forbidden-ids, a list of node IDs (strings) that should always be excluded
;;   from results.

;; Keeping blocks up-to-date:
;;
;; These dynamic blocks can optionally be updated when opening and saving
;; buffers. To do this, enable `org-roam-dblocks-autoupdate-mode'.
;;
;; The autoupdate can be customised using `org-roam-dblocks-auto-refresh-tags'
;; so that it only runs in files/headings with specific tags. This is useful if
;; you want to have both index-style cards and stable canned searches.
;;

;;; Code:

(require 'dash)
(require 'org-tags-filter)
(require 'plisty)
(require 's)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-roam))

(defgroup org-roam-dblocks nil
  "Adds support for a dynamic block of org-roam backlinks to `org-mode'."
  :group 'productivity
  :prefix "org-roam-dblocks-")

(defcustom org-roam-dblocks-auto-refresh-tags nil
  "A list of tags (as strings) or nil.

If non-nil, only org-roam nodes with the specified tags have
their blocks updated automatically."
  :group 'org-roam-dblocks
  :type '(choice (const nil)
          (repeat :tag "Tag" (string))))

(defcustom org-roam-dblocks-autoupdate-silently-p t
  "Whether to suppress messages during the dblock update process."
  :group 'org-roam-dblocks
  :type 'boolean)

(defconst org-roam-dblocks-names '("notes" "backlinks"))



(plisty-define org-roam-dblocks-link
  :required (:id :desc))

(defun org-roam-dblocks--make-list-item-formatter (params)
  (let* ((indent (org-roam-dblocks-args-indent params))
         (prefix (concat (make-string indent ?\ ) "- ")))
    (lambda (link)
      (concat prefix
              (org-link-make-string (concat "id:" (org-roam-dblocks-link-id link))
                                    (org-roam-dblocks-link-desc link))))))

(defalias 'org-roam-dblocks--link-sorting
  (-on #'string-lessp (-compose #'downcase #'org-roam-dblocks-link-desc)))

(plisty-define org-roam-dblocks-args
  :optional (:id :match :tags :only-missing :include-same-file
             :name :indentation-column :content :forbidden-ids
             :filter :remove :indent))

(defun org-roam-dblocks--make-link-formatter (params)
  (let ((regexp-parser (when-let* ((matcher (org-roam-dblocks-args-match params)))
                         (org-roam-dblocks--parse-regexp-form matcher))))
    (lambda (node)
      (let ((title (org-roam-node-title node)))
        (org-roam-dblocks-link-create :id (org-roam-node-id node)
                                      :desc (or (when regexp-parser
                                                  (cadr (s-match regexp-parser title)))
                                                title))))))

(defun org-roam-dblocks--parse-regexp-form (form)
  ;;; Quick tests:
  ;; (org-roam-dblocks--parse-regexp-form nil)
  ;; (org-roam-dblocks--parse-regexp-form 'hi)
  ;; (org-roam-dblocks--parse-regexp-form "hi")
  ;; (org-roam-dblocks--parse-regexp-form '(rx bol "hi" eol))
  (cond
   ((null form) nil)
   ((stringp form)
    (unless (zerop (length form))
      form))
   ((symbolp form)
    (symbol-name form))
   (t
    (pcase form
      (`(rx . ,args)
       (rx-to-string (cons 'and args)
                     t))))))

(defconst org-roam-dblocks--node-slot-symbols
  '(file file-title file-hash file-atime file-mtime
    id level point todo priority scheduled deadline title properties olp
    tags aliases refs)
  "A list of slots names on org-roam-nodes.

This list is used to create lexical bindings in anaphoric
predicates.")

(defun org-roam-dblocks--bindings-for-lexical-scope (node)
  (cons `(it . ,node)
        (seq-map (lambda (sym)
                   (let ((slot-accessor (intern (format "org-roam-node-%s" sym))))
                     (cons sym (funcall slot-accessor node))))
                 org-roam-dblocks--node-slot-symbols)))

(defun org-roam-dblocks--parse-filter-fn (keyword form)
  ;; Quick tests:
  ;; (org-roam-dblocks--parse-filter-fn :foo nil)
  ;; (org-roam-dblocks--parse-filter-fn :foo t)
  ;; (org-roam-dblocks--parse-filter-fn :foo 'ignore)
  ;; (org-roam-dblocks--parse-filter-fn :foo (lambda (node) node))
  ;; (org-roam-dblocks--parse-filter-fn :foo '(lambda (node) node))
  ;; (org-roam-dblocks--parse-filter-fn :foo 'it)
  ;; (org-roam-dblocks--parse-filter-fn :foo '(equal it 0))
  (cl-macrolet ((lambda-with-error-handling (binding &rest body)
                  `(lambda ,binding
                     (condition-case-unless-debug err
                         (progn ,@body)
                       (error
                        (error "Error evaluating %s form: %s"
                               keyword
                               (error-message-string err)))))))
    (cond
     ((null form)
      nil)
     ((functionp form)
      (lambda-with-error-handling (node)
                                  (funcall form node)))
     (t
      (lambda-with-error-handling (node)
                                  (eval form (org-roam-dblocks--bindings-for-lexical-scope node)))))))

(defun org-roam-dblocks--compile-filter-fns (params)
  ;; Quick tests:
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:filter (equal "foo" it))) "foo")
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:filter (equal "foo" it))) "bar")
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:remove (equal "foo" it))) "foo")
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:remove (equal "foo" it))) "bar")
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:filter stringp :remove stringp)) "foo")
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:filter stringp :remove integerp)) "foo")
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:filter stringp :remove integerp)) 0)
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:filter integerp :remove stringp)) "foo")
  ;; (funcall (org-roam-dblocks--compile-filter-fns '(:filter integerp :remove stringp)) 1)
  (pcase-exhaustive
      (cons (org-roam-dblocks--parse-filter-fn :filter (org-roam-dblocks-args-filter params))
            (org-roam-dblocks--parse-filter-fn :remove (org-roam-dblocks-args-remove params)))

    (`(nil     . nil)     (-const t))
    (`(,filter . nil)     filter)
    (`(nil     . ,remove) (-not remove))
    (`(,filter . ,remove) (-andfn filter (-not remove)))))

(defun org-roam-dblocks--eval-regexp-predicate (node match)
  (or (null match)
      (string-match-p match (org-roam-node-title node))))

(defun org-roam-dblocks--eval-tags-predicate (node tags-filter)
  (let* ((tags (org-roam-node-tags node))
         (forbidden-tags (org-tags-filter-forbidden tags-filter))
         (required-tags (org-tags-filter-required tags-filter)))
    (not (or (seq-intersection tags forbidden-tags)
             (seq-difference required-tags tags)))))

(defun org-roam-dblocks--compiled-predicates (params)
  (let ((tags (org-tags-filter-parse (org-roam-dblocks-args-tags params)))
        (match (org-roam-dblocks--parse-regexp-form (org-roam-dblocks-args-match params)))
        (predicate (org-roam-dblocks--compile-filter-fns params))
        (file-for-id (org-roam-node-file (org-roam-node-from-id (org-roam-dblocks-args-id params)))))
    (lambda (node)
      (when (and (not (seq-contains-p (org-roam-dblocks-args-forbidden-ids params)
                                      (org-roam-node-id node)))
                 (or (org-roam-dblocks-args-include-same-file params)
                     (not (equal file-for-id (org-roam-node-file node))))
                 (org-roam-dblocks--eval-regexp-predicate node match)
                 (org-roam-dblocks--eval-tags-predicate node tags)
                 (funcall predicate node))
        node))))

(defun org-roam-dblocks--links-not-in-dblocks (node)
  (let ((forward-links (org-roam-db-query [:select :distinct [pos dest]
                                           :from links
                                           :where (and (= type "id") (= source $s1))]
                                          (org-roam-node-id node)))
        (not-in-block (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents (org-roam-node-file node))
      (let ((org-inhibit-startup t)
            (org-inhibit-logging t))
        (delay-mode-hooks (org-mode)))

      (pcase-dolist (`(,pos ,id) forward-links)
        (goto-char pos)
        (let ((block-args (cadr (org-element-lineage (org-element-at-point) '(dynamic-block)))))
          (unless (or block-args
                      (seq-contains-p org-roam-dblocks-names (plist-get block-args :block-name)))
            (puthash id t not-in-block)))))
    (hash-table-keys not-in-block)))

(defun org-roam-dblocks--compute-forbidden-ids (params)
  (append (list (org-roam-dblocks-args-id params))
          (org-roam-dblocks-args-forbidden-ids params)
          (when (org-roam-dblocks-args-only-missing params)
            (when-let* ((node (org-roam-node-from-id (org-roam-dblocks-args-id params))))
              (org-roam-dblocks--links-not-in-dblocks node)))))


;; HACK: To avoid dirtying the buffer when blocks haven't changed, we actually
;; compute the data to insert earlier, at the phase where org would normally
;; blindly clear out the block's content. We then check whether the block
;; content needs to be updated.

(define-advice org-prepare-dblock (:around (fn &rest args) org-roam-dblocks-dirty-checks)
  "Advice to hack org's dblock update flow for the dblock types we define.

FN is the advised function, and ARGS are its arguments.

Populates `org-roam-dblocks--content' and ensures the buffer
stays unchanged if there's no difference between the new content
and old content."
  (unless (looking-at org-dblock-start-re)
    (user-error "Not at a dynamic block"))
  (let ((name (match-string-no-properties 1)))
    (if (not (member name org-roam-dblocks-names))
        ;; Defer to default implementation for any dblocks we don't define in
        ;; this file..
        (apply fn args)
      (let* ((indent (save-excursion
                       (goto-char (match-beginning 0))
                       (back-to-indentation)
                       (current-column)))
             (node-id (ignore-errors
                        (save-match-data
                          (org-roam-node-id (org-roam-node-at-point)))))
             (params (append (list :name name)
                             (read (concat "(" (match-string 3) ")"))
                             (list :id node-id :indent indent)))
             (content-start (match-end 0))
             (content-end (if (re-search-forward org-dblock-end-re nil t)
                              (1- (match-beginning 0))
                            (error "Dynamic block not terminated")))
             (current-content (buffer-substring-no-properties content-start content-end))
             (updated-content
              (condition-case-unless-debug err
                  (pcase-exhaustive name
                    ("notes" (org-roam-dblocks-format-notes params))
                    ("backlinks" (org-roam-dblocks-format-backlinks params)))
                (error
                 (error-message-string err))))

             (content-changed-p (not (equal current-content
                                            updated-content)))
             (params (append params (list :new-content (when content-changed-p updated-content)))))

        ;; Only clear the block if the content should change.
        (when content-changed-p
          (delete-region content-start content-end)
          (goto-char content-start))

        params))))

;;;###autoload
(defun org-roam-dblocks--write-content (params)
  (when-let* ((new-content (plist-get params :new-content)))
    (insert "\n")
    (insert new-content)))


;;; Backlinks dblock type

(defun org-roam-dblocks-format-backlinks (params)
  (org-roam-dblocks-args-assert params t)

  (setf (plist-get params :forbidden-ids)
        (org-roam-dblocks--compute-forbidden-ids params))

  (if-let* ((id (org-roam-dblocks-args-id params))
            (node (if id (org-roam-node-from-id id) (org-roam-node-at-point t)))
            (lines (->> (org-roam-backlinks-get node :unique t)
                        (-keep (-compose (org-roam-dblocks--compiled-predicates params) #'org-roam-backlink-source-node))
                        (seq-map (org-roam-dblocks--make-link-formatter params))
                        (seq-sort 'org-roam-dblocks--link-sorting)
                        (seq-map (org-roam-dblocks--make-list-item-formatter params)))))
      (string-join lines  "\n")
    ""))

;;;###autoload
(defalias 'org-dblock-write:backlinks #'org-roam-dblocks--write-content)

;;;###autoload
(defun org-insert-dblock:backlinks ()
  "Insert a dynamic block backlinks at point."
  (interactive)
  (atomic-change-group
    (org-create-dblock (list :name "backlinks")))
  (org-update-dblock))

(org-dynamic-block-define "backlinks" #'org-insert-dblock:backlinks)


;;; Roam notes search dblock type

(defun org-roam-dblocks-format-notes (params)
  (org-roam-dblocks-args-assert params t)
  (cl-assert (or (org-roam-dblocks-args-match params)
                 (org-roam-dblocks-args-tags params)
                 (org-roam-dblocks-args-filter params)
                 (org-roam-dblocks-args-remove params))
             t "Must provide at least one of :tags, :match, :filter or :remove")

  (setf (plist-get params :forbidden-ids)
        (org-roam-dblocks--compute-forbidden-ids params))

  (let ((lines (->> (org-roam-node-list)
                    (-keep (org-roam-dblocks--compiled-predicates params))
                    (seq-map (org-roam-dblocks--make-link-formatter params))
                    (seq-sort #'org-roam-dblocks--link-sorting)
                    (seq-map (org-roam-dblocks--make-list-item-formatter params)))))
    (string-join lines "\n")))

;;;###autoload
(defalias 'org-dblock-write:notes #'org-roam-dblocks--write-content)

(defun org-roam-dblocks--read-tags-filter-for-dblock-args ()
  (let* ((tags-filter (org-tags-filter-read))
         (unpacked (append (seq-map (lambda (it) (concat "-" it)) (org-tags-filter-forbidden tags-filter))
                           (org-tags-filter-required tags-filter))))
    (if (equal 1 (length unpacked))
        (car unpacked)
      unpacked)))

;;;###autoload
(defun org-insert-dblock:notes ()
  "Insert a dynamic block org-roam notes at point."
  (interactive)
  (let ((args (pcase-exhaustive (completing-read "Query Type: " '("Title Regexp Match" "Tags Filter"))
                ("Title Regexp Match"
                 (list :match (read-string "Match title (regexp): ")))
                ("Tags Filter"
                 (list :tags (org-roam-dblocks--read-tags-filter-for-dblock-args))))))
    (atomic-change-group
      (org-create-dblock (append '(:name "notes") args))))
  (org-update-dblock))


(org-dynamic-block-define "notes" #'org-insert-dblock:notes)



(defun org-roam-dblocks--update-block-at-point-p ()
  (when (derived-mode-p 'org-mode)
    (or (null org-roam-dblocks-auto-refresh-tags)
        (seq-intersection org-roam-dblocks-auto-refresh-tags
                          (append org-file-tags (org-get-tags))))))

(defun org-roam-dblocks--update-blocks ()
  (let ((message-log-max (if org-roam-dblocks-autoupdate-silently-p nil message-log-max)))
    (org-map-dblocks
     (lambda ()
       (when (org-roam-dblocks--update-block-at-point-p)
         (pcase (org-element-at-point)
           (`(dynamic-block ,plist)
            (when (member (plist-get plist :block-name) org-roam-dblocks-names)
              (org-update-dblock)))))))))



;;;###autoload
(define-minor-mode org-roam-dblocks-autoupdate-mode
  "Automatically update org-roam-dblocks blocks on open and save."
  :init-value nil
  (cond
   (org-roam-dblocks-autoupdate-mode
    (org-roam-dblocks--update-blocks)
    (when (and (buffer-file-name) (buffer-modified-p))
      (let ((message-log-max (if org-roam-dblocks-autoupdate-silently-p nil message-log-max)))
        (save-buffer)))
    (add-hook 'before-save-hook #'org-roam-dblocks--update-blocks nil t))
   (t
    (remove-hook 'before-save-hook #'org-roam-dblocks--update-blocks))))

(provide 'org-roam-dblocks)

;;; org-roam-dblocks.el ends here
