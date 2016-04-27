;;; zippers.el --- Functional zippers
;;; Commentary:
;;
;; Ported from Daniel Fernandes Martins' cl-zipper
;; https://github.com/danielfm/cl-zipper
;;
;; Original license:
;;
;; Copyright (c) 2010-2012, Daniel Fernandes Martins
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:
;;
;;     1. Redistributions of source code must retain the above copyright notice,
;;        this list of conditions and the following disclaimer.
;;
;;     2. Redistributions in binary form must reproduce the above copyright
;;        notice, this list of conditions and the following disclaimer in the
;;        documentation and/or other materials provided with the distribution.
;;
;;     3. Neither the name of Daniel Fernandes Martins nor the names of
;;        its contributors may be used to endorse or promote products derived
;;        from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; Code:

(eval-when-compile
  (require 'cl))

(defstruct loc
  "A location consists of a distinguished tree, the current focus of attention
and its path, representing its surrounding context."
  (left "Nodes on the left of this location.")
  (ppath "Path to this location from the root.")
  (right "Nodes on the right of this location."))


(defun zipper (lst)
  "Creates a zipper for the list lst."
  (list lst nil))

(defmacro with-loc (loc &rest body)
  "Binds the variables 'loc-tree' and 'loc-path' for the tree and path of this
loc, respectively."
  `(let ((loc-tree (car ,loc))
         (loc-path (cadr ,loc)))
     loc-tree loc-path
     ,@body))

(defun root-node (loc)
  "Zips all the way up and returns the root node, reflecting any changes."
  (let ((loc-up (go-up loc)))
    (if loc-up
        (root-node loc-up)
      (car loc))))

(defun go-down (loc)
  "Returns the loc of the leftmost child of the node at this loc, or nil if no
children."
  (with-loc loc
            (when (consp loc-tree)
              (list (car loc-tree)
                    (make-loc :left nil
                              :ppath loc-path
                              :right (cdr loc-tree))))))

(defun go-right (loc)
  "Returns the loc of the right sibling of the node at this loc, or nil."
  (with-loc loc
            (when loc-path
              (let ((left (loc-left loc-path))
                    (right (loc-right loc-path)))
                (when right
                  (list (car right)
                        (make-loc :left (cons loc-tree left)
                                  :ppath (loc-ppath loc-path)
                                  :right (cdr right))))))))

(defun rights (loc)
  "Returns a list of the right siblings of this loc."
  (with-loc loc
            (when loc-path
              (loc-right loc-path))))

(defun rightmost (loc)
  "Returns the loc of the rightmost sibling of the node at this loc, or self."
  (with-loc loc
            (if loc-path
                (let ((right (loc-right loc-path)))
                  (list (car (last right))
                        (make-loc :left (cons (car (butlast right)) (list loc-tree))
                                  :ppath (loc-ppath loc-path)
                                  :right nil)))
              loc)))

(defun go-left (loc)
  "Returns the loc of the left sibling of the node at this loc, or nil."
  (with-loc loc
            (when loc-path
              (let ((left (loc-left loc-path))
                    (right (loc-right loc-path)))
                (when left
                  (list (car left)
                        (make-loc :left (cdr left)
                                  :ppath (loc-ppath loc-path)
                                  :right (cons loc-tree right))))))))

(defun lefts (loc)
  "Returns a list of the left siblings of this loc."
  (with-loc loc
            (when loc-path
              (loc-left loc-path))))

(defun leftmost (loc)
  "Returns the loc of the leftmost sibling of the node at this loc, or self."
  (with-loc loc
            (if loc-path
                (let ((left (loc-left loc-path)))
                  (list (car (last left))
                        (make-loc :left nil
                                  :ppath (loc-ppath loc-path)
                                  :right (cons (car (butlast left)) (list loc-tree)))))
              loc)))

(defun go-up (loc)
  "Returns the loc of the parent node of the node at this loc, or nil if at
the top."
  (with-loc loc
            (when loc-path
              (list (concatenate 'list (reverse (loc-left loc-path))
                                 (when loc-tree
                                   (cons loc-tree (loc-right loc-path))))
                    (loc-ppath loc-path)))))

(defun go-next (loc)
  "Moves to the next loc in the hierarchy, depth-first. When reaching the end,
returns nil."
  (with-loc loc
            (or (and (consp loc-tree) (go-down loc))
                (go-right loc)
                (go-next-up loc))))

(defun go-next-up (loc)
  "Moves up in hierarchy until a right node is found or the root is reached."
  (let ((uloc (go-up loc)))
    (when uloc
      (or (go-right uloc) (go-next-up uloc)))))

(defun go-prev (loc)
  "Moves to the previous loc in the hierarchy, depth-first. If already at the
root, returns nil."
  (let ((lloc (go-left loc)))
    (if lloc
        (go-prev-up lloc)
      (go-up loc))))

(defun go-prev-up (loc)
  "Moves up in the hierarchy until a left node is found or the root is reached."
  (with-loc loc
            (let ((dloc (and (consp loc-tree) (go-down loc))))
              (if dloc
                  (go-prev-up (rightmost dloc))
                loc))))

(defun path (loc)
  "Returns the list of nodes leading to this loc."
  (let ((loc-up (go-up loc)))
    (when loc-up
      (append (path loc-up) (list (car loc-up))))))

(defun change-node (loc tree)
  "Replaces the node at this loc, whithout moving."
  (when loc
    (list tree (cadr loc))))

(defun edit-node (loc func &rest args)
  "Replaces the node at this loc with the value of (func node args)."
  (change-node loc (apply func (car loc) args)))

(defun insert-right (loc tree)
  "Inserts the item as the right sibling of the node at this loc, without
moving."
  (with-loc loc
            (when loc-path
              (list loc-tree
                    (make-loc :left (loc-left loc-path)
                              :ppath (loc-ppath loc-path)
                              :right (cons tree (loc-right loc-path)))))))

(defun insert-left (loc tree)
  "Inserts the item as the left sibling of the node at this loc, without
moving."
  (with-loc loc
            (when loc-path
              (list loc-tree
                    (make-loc :left (cons tree (loc-left loc-path))
                              :ppath (loc-ppath loc-path)
                              :right (loc-right loc-path))))))

(defun insert-down (loc tree)
  "Inserts the item as the leftmost child of the node at this loc, without
moving."
  (with-loc loc
            (when (consp loc-tree)
              (list (cons tree loc-tree)
                    loc-path))))

(defun append-down (loc tree)
  "Inserts the item as the rightmost child of the node at this loc, without
moving."
  (with-loc loc
            (when (consp loc-tree)
              (list (concatenate 'list loc-tree (list tree))
                    loc-path))))

(defun remove-node (loc)
  "Removes the node at loc, returning the loc that would have preceded it in a
depth-first walk."
  (with-loc loc
            (when (and loc loc-path)
              (cond ((loc-right loc-path) (replace-by-right loc-path))
                    ((loc-left loc-path) (replace-by-left loc-path))))))

(defun replace-by-right (loc-path)
  "Replaces the current node at loc by the node at the right."
  (when loc-path
    (let ((right (loc-right loc-path)))
      (list (car right)
            (make-loc :left (loc-left loc-path)
                      :ppath (loc-ppath loc-path)
                      :right (cdr right))))))

(defun replace-by-left (loc-path)
  "Replaces the current node at loc by the node at the left."
  (when loc-path
    (let ((left (loc-left loc-path)))
      (list (car left)
            (make-loc :left (cdr left)
                      :ppath (loc-ppath loc-path)
                      :right (loc-right loc-path))))))

(provide 'zippers)
;;; zippers.el ends here
