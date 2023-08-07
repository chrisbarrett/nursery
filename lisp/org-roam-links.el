;;; org-roam-links.el --- Buffer showing links in an org-roam node -*- lexical-binding: t; -*-

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

;; org-roam comes with a backlanks buffer that shows previous of other locations
;; in the Zettlekasten linking here. This is useful for most cases, but it would
;; be useful to see another level of links deep to find unexpected node
;; connections.

;;; Code:

(require 'dash)
(require 'org-roam-review)
(require 'plisty)

(plisty-define org-roam-links-graph
  :required (:nodes :tree))

(defun org-roam-links--forward-links (node)
  (-keep (-compose #'org-roam-node-from-id #'car)
         (org-roam-db-query [:select :distinct [dest] :from links :where (= source $s1)]
                            (org-roam-node-id node))))

(defun org-roam-links-graph (node depth)
  "Return the linked nodes for NODE, and their links, up to DEPTH."
  (when (cl-plusp depth)
    (let ((links
           (->>  (append (org-roam-links--forward-links node)
                         (seq-map #'org-roam-backlink-source-node (org-roam-backlinks-get node)))
                 (seq-remove #'org-roam-review-node-ignored-p)
                 (-uniq)))
          (nodes (ht-create))
          (tree (ht-create)))
      (dolist (node links)
        (let ((id (org-roam-node-id node))
              (children (org-roam-links-graph node (1- depth))))
          (puthash id node nodes)
          (-when-let ((&plist :nodes child-nodes) children)
            (setq nodes (ht-merge nodes child-nodes)))
          (puthash (org-roam-node-id node) (plist-get children :tree) tree)))
      (org-roam-links-graph-create :nodes nodes :tree tree))))

(defconst org-roam-links-max-title-length 50)

(defalias 'org-roam-links--graph-sorting
  (-compose 'downcase #'org-roam-node-title #'org-roam-node-from-id #'car))

;;;###autoload
(cl-defun org-roam-links (&optional (max-depth 2))
  "Show Evergreen Note links for the current buffer.

When called interactively, prompt the user for MAX-DEPTH."
  (interactive (when current-prefix-arg (list (read-number "Depth: " 2))))
  (-let* ((start-node (or (org-roam-node-at-point)
                          (let ((node (org-roam-node-read)))
                            (org-roam-node-visit node)
                            node)))
          (title (org-roam-node-title start-node))
          (short-title (substring title 0 (min (length title) org-roam-links-max-title-length)))
          (short-title (if (equal title short-title) title (concat short-title "…")))
          graph)
    (display-buffer
     (org-roam-review-create-buffer
      :title (format "Links for “%s\”" short-title)
      :instructions "Below is the graph of links to and from the current node."
      :placeholder "No linked nodes"
      :buffer-name "*org-roam-links*"
      :nodes
      (lambda ()
        (setq graph (org-roam-links-graph start-node max-depth))
        (seq-remove #'org-roam-review-node-ignored-p
                    (ht-values (org-roam-links-graph-nodes graph))))
      :render
      (-lambda ((&plist :root-section))
        (let ((seen-ids (ht-create))
              (nodes (org-roam-links-graph-nodes graph))
              (start-node-id (org-roam-node-id start-node)))
          (cl-labels ((render-at-depth
                       (tree depth)
                       (let ((sorted-nodes (seq-sort-by 'org-roam-links--graph-sorting #'string< (ht-to-alist tree))))
                         (pcase-dolist (`(,id . ,children) sorted-nodes)
                           (when-let* ((node (ht-get nodes id)))
                             (magit-insert-section section (org-roam-preview-section)
                               (oset section parent root-section)
                               (oset section point (org-roam-node-point node))
                               (oset section file (org-roam-node-file node))
                               (if (equal id start-node-id)
                                   (magit-cancel-section)
                                 (let* ((seen-p (gethash id seen-ids))
                                        (heading (funcall org-roam-review-title-formatter node)))
                                   (magit-insert-heading (org-roam-review-indent-string heading depth))
                                   (unless seen-p
                                     (puthash id t seen-ids)
                                     (when children
                                       (render-at-depth children (1+ depth))))))))))))

            (render-at-depth (org-roam-links-graph-tree graph) 0))))))))

(provide 'org-roam-links)

;;; org-roam-links.el ends here
