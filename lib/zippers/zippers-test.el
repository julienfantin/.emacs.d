;;; zippers-test.el -- Zippers test-suite
;;; Commentary:
;;
;; Ported from Daniel Fernandes Martins' cl-zipper
;; https://github.com/danielfm/cl-zipper
;;
;; Original license:
;;
;; Copyright (c) 2010-2012, Daniel Fernandes Martins
;; All rights reserved.
;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:
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

(require 'zippers)

(defvar *tree* '(/ (+ (* a 2) (- b 4))))

(defun -> (tree &rest path)
  "Navigates the given path on tree, where each element in path should a keyword
that represents the desired direction, i.e., :down, :up, :left, :right."
  (cl-labels ((nav (loc rest)
                   (if rest
                       (nav (case (car rest)
                              (:down (go-down loc))
                              (:up (go-up loc))
                              (:left (go-left loc))
                              (:right (go-right loc))
                              (otherwise loc))
                            (cdr rest))
                     loc)))
    (nav (zipper tree) path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ZIPPER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-zipper ()
  (let ((loc (-> *tree*)))
    (should (equal 2 (length loc)))
    (should (equal *tree* (car loc)))
    (should (null (cadr loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ROOT-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ert-deftest test-root-node ()
  (let ((loc (root-node (-> *tree* :down :right :down :right :right))))
    (should (equal 2 (length loc)))
    (should (equal *tree* loc))))

(ert-deftest test-root-node-at-root ()
  (let ((loc (root-node (-> *tree*))))
    (should (equal 2 (length loc)))
    (should (equal *tree* loc))))

(ert-deftest test-root-node-at-nil ()
  (should (null (root-node nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-LEFT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-go-left ()
  (let* ((loc (-> *tree* :down :right :down :right :right))
         (new-loc-1 (go-left loc))
         (new-loc-2 (go-left new-loc-1)))
    (should (equal '(* a 2) (car new-loc-1)))
    (should (equal '(+) (lefts new-loc-1)))
    (should (equal '((- b 4)) (rights new-loc-1)))

    (should (equal '+ (car new-loc-2)))
    (should (null (lefts new-loc-2)))
    (should (equal '((* a 2) (- b 4)) (rights new-loc-2)))))

(ert-deftest test-go-left-at-root ()
  (should (null (go-left (-> *tree*)))))

(ert-deftest test-go-left-at-nil ()
  (should (null (go-left nil))))

(ert-deftest test-go-left-at-end ()
  (let ((loc (-> *tree* :down)))
    (should (equal '/ (car loc)))
    (should (null (lefts loc)))
    (should (null (go-left loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-RIGHT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-go-right ()
  (let* ((loc (-> *tree* :down :right :down))
         (new-loc-1 (go-right loc))
         (new-loc-2 (go-right new-loc-1)))
    (should (equal '(* a 2) (car new-loc-1)))
    (should (equal '(+) (lefts new-loc-1)))
    (should (equal '((- b 4)) (rights new-loc-1)))

    (should (equal '(- b 4) (car new-loc-2)))
    (should (equal '((* a 2) +) (lefts new-loc-2)))
    (should (null (rights new-loc-2)))))

(ert-deftest test-go-right-at-root ()
  (should (null (go-right (-> *tree*)))))

(ert-deftest test-go-right-at-nil ()
  (should (null (go-right nil))))

(ert-deftest test-go-right-at-end ()
  (let ((loc (rightmost (-> *tree* :down :right :down))))
    (should (equal '(- b 4) (car loc)))
    (should (null (rights loc)))
    (should (null (go-right loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-go-up ()
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (new-loc-1 (go-up loc))
         (new-loc-2 (go-up new-loc-1)))
    (should (equal '(* a 2) (car new-loc-1)))
    (should (equal '(+) (lefts new-loc-1)))
    (should (equal '((- b 4)) (rights new-loc-1)))

    (should (equal '(+ (* a 2) (- b 4)) (car new-loc-2)))
    (should (equal '(/) (lefts new-loc-2)))
    (should (null (rights new-loc-2)))))

(ert-deftest test-go-up-at-root ()
  (should (null (go-up (-> *tree*)))))

(ert-deftest test-go-up-at-nil ()
  (should (null (go-up nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-go-down ()
  (let* ((loc (-> *tree* :down :right))
         (new-loc-1 (go-down loc))
         (new-loc-2 (go-down (go-right new-loc-1))))
    (should (equal '+ (car new-loc-1)))
    (should (null (lefts new-loc-1)))
    (should (equal '((* a 2) (- b 4)) (rights new-loc-1)))

    (should (equal '* (car new-loc-2)))
    (should (null (lefts new-loc-2)))
    (should (equal '(a 2) (rights new-loc-2)))))

(ert-deftest test-go-down-at-end ()
  (should (null (go-down (-> *tree* :down :right :down :right :down)))))

(ert-deftest test-go-down-at-nil ()
  (should (null (go-down nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LEFTMOST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-leftmost ()
  (let ((loc (leftmost (-> *tree* :down :right :down :right :right))))
    (should (equal '+ (car loc)))
    (should (null (lefts loc)))
    (should (equal '((* a 2) (- b 4)) (rights loc)))))

(ert-deftest test-leftmost-at-nil ()
  (should (null (leftmost nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RIGHTMOST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-rightmost ()
  (let ((loc (rightmost (-> *tree* :down :right :down))))
    (should (equal '(- b 4) (car loc)))
    (should (null (rights loc)))
    (should (equal '((* a 2) +) (lefts loc)))))

(ert-deftest test-rightmost-at-nil ()
  (should (null (rightmost nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-NEXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-go-next ()
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (next-loc (go-next loc)))
    (should (equal '* (car loc)))
    (should (equal 'a (car next-loc)))
    (should (equal '(*) (lefts next-loc)))
    (should (equal '(2) (rights next-loc)))))

(ert-deftest test-go-next-at-root ()
  (let* ((loc (-> *tree*))
         (next-loc (go-next loc)))
    (should (equal *tree* (car loc)))
    (should (equal '/ (car next-loc)))
    (should (equal '() (lefts next-loc)))
    (should (equal '((+ (* a 2) (- b 4))) (rights next-loc)))))

(ert-deftest test-go-next-at-rightmost ()
  (let* ((loc (-> *tree* :down :right :down :right :down :right :right))
         (next-loc (go-next loc)))
    (should (equal 2 (car loc)))
    (should (equal '(- b 4) (car next-loc)))
    (should (equal '((* a 2) +) (lefts next-loc)))
    (should (equal '() (rights next-loc)))))

(ert-deftest test-go-next-at-nested-rightmost ()
  (let* ((loc (-> '(+ (- 1 (* 2 3)) 4) :down :right :down :right :right :down :right :right))
         (next-loc (go-next loc)))
    (should (equal 3 (car loc)))
    (should (equal 4 (car
                      next-loc)))
    (should (equal '((- 1 (* 2 3)) +) (lefts next-loc)))
    (should (equal '() (rights next-loc)))))

(ert-deftest test-go-next-at-last ()
  (let* ((loc (-> *tree* :down :right :down :right :right :down :right :right))
         (next-loc (go-next loc)))
    (should (equal 4 (car loc)))
    (should (null next-loc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO-PREV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-go-prev ()
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (prev-loc (go-prev loc)))
    (should (equal 'a (car loc)))
    (should (equal '* (car prev-loc)))
    (should (equal '() (lefts prev-loc)))
    (should (equal '(a 2) (rights prev-loc)))))

(ert-deftest test-go-prev-at-root ()
  (let* ((loc (-> *tree*))
         (prev-loc (go-prev loc)))
    (should (null prev-loc))))

(ert-deftest test-go-prev-at-leftmost ()
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (prev-loc (go-prev loc)))
    (should (equal '* (car loc)))
    (should (equal '(* a 2) (car prev-loc)))))

(ert-deftest test-go-prev-at-rightmost ()
  (let* ((loc (-> *tree* :down :right :down :right :right))
         (prev-loc (go-prev loc)))
    (should (equal '(- b 4) (car loc)))
    (should (equal 2 (car prev-loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-path ()
  (let ((loc (-> *tree* :down :right :down :right :down)))
    (should (equal '((/ (+ (* a 2) (- b 4)))
                     (+ (* a 2) (- b 4))
                     (* a 2)) (path loc)))))

(ert-deftest test-path-at-root ()
  (should (null (path (-> *tree*)))))

(ert-deftest test-path-at-nil ()
  (should (null (path nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHANGE-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-change-node-atom ()
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (change-node loc 'x)))
    (should (equal 'a (car loc)))
    (should (equal 'x (car new-loc)))
    (should (equal '(*) (lefts new-loc)))
    (should (equal '(2) (rights new-loc)))
    (should (equal '(/ (+ (* x 2) (- b 4))) (root-node new-loc)))))

(ert-deftest test-change-node-subtree ()
  (let* ((loc (-> *tree* :down :right :down :right))
         (new-loc (change-node loc '(+ a a))))
    (should (equal '(* a 2) (car loc)))
    (should (equal '(+ a a) (car new-loc)))
    (should (equal '(+) (lefts new-loc)))
    (should (equal '((- b 4)) (rights new-loc)))
    (should (equal '(/ (+ (+ a a) (- b 4))) (root-node new-loc)))))

(ert-deftest test-change-node-null ()
  (should (null (change-node nil 'x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EDIT-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-edit-node ()
  (cl-labels ((fn (node arg-1 arg-2 &optional (arg-3 3))
                  (should (equal '(* a 2) node))
                  (should (equal 1 arg-1))
                  (should (equal 2 arg-2))
                  (should (equal 3 arg-3))
                  '(- b)))
    (let* ((loc (-> *tree* :down :right :down :right))
           (new-loc (edit-node loc #'fn 1 2)))
      (should (equal '(* a 2) (car loc)))
      (should (equal '(- b) (car new-loc)))
      (should (equal '(+) (lefts new-loc)))
      (should (equal '((- b 4)) (rights new-loc)))
      (should (equal '(/ (+ (- b) (- b 4))) (root-node new-loc))))))

(ert-deftest test-edit-node-null ()
  (should (null (edit-node nil #'identity))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSERT-DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-insert-down ()
  (let* ((loc (-> *tree* :down :right :down :right))
         (new-loc (insert-down loc '-)))
    (should (equal '(* a 2) (car loc)))
    (should (equal '(- * a 2) (car new-loc)))
    (should (equal '(+) (lefts new-loc)))
    (should (equal '((- b 4)) (rights new-loc)))
    (should (equal '(/ (+ (- * a 2) (- b 4))) (root-node new-loc)))))

(ert-deftest test-insert-down-at-atom ()
  (let ((loc (-> *tree* :down :right :down :right :down)))
    (should (null (insert-down loc '-)))))

(ert-deftest test-insert-down-at-nil ()
  (should (null (insert-down nil '-))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APPEND-DOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-append-down ()
  (let* ((loc (-> *tree* :down :right :down :right))
         (new-loc (append-down loc 'b)))
    (should (equal '(* a 2) (car loc)))
    (should (equal '(* a 2 b) (car new-loc)))
    (should (equal '(+) (lefts new-loc)))
    (should (equal '((- b 4)) (rights new-loc)))
    (should (equal '(/ (+ (* a 2 b) (- b 4))) (root-node new-loc)))))

(ert-deftest test-append-down-at-atom ()
  (let ((loc (-> *tree* :down :right :down :right :down)))
    (should (null (append-down loc 'b)))))

(ert-deftest test-append-down-at-nil ()
  (should (null (append-down nil 'b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSERT-LEFT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-insert-left ()
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (insert-left loc 'b)))
    (should (equal 'a (car new-loc)))
    (should (equal '(*) (lefts loc)))
    (should (equal '(b *) (lefts new-loc)))
    (should (equal '(/ (+ (* b a 2) (- b 4))) (root-node new-loc)))))

(ert-deftest test-insert-left-at-nil ()
  (should (null (insert-left nil 'b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INSERT-RIGHT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-insert-right ()
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (insert-right loc 'b)))
    (should (equal 'a (car new-loc)))
    (should (equal '(2) (rights loc)))
    (should (equal '(b 2) (rights new-loc)))
    (should (equal '(/ (+ (* a b 2) (- b 4))) (root-node new-loc)))))

(ert-deftest test-insert-right-at-nil ()
  (should (null (insert-right nil 'b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REMOVE-NODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-remove-node ()
  (let* ((loc (-> *tree* :down :right :down :right :down :right))
         (new-loc (remove-node loc)))
    (should (equal 'a (car loc)))
    (should (equal '2 (car new-loc)))
    (should (equal '(*) (lefts new-loc)))
    (should (null (rights new-loc)))
    (should (equal '(/ (+ (* 2) (- b 4))) (root-node new-loc)))))

(ert-deftest test-remove-node-at-leftmost-node ()
  (let* ((loc (-> *tree* :down :right :down :right :down))
         (new-loc (remove-node loc)))
    (should (equal '* (car loc)))
    (should (equal 'a (car new-loc)))
    (should (null (lefts new-loc)))
    (should (equal '(2) (rights new-loc)))
    (should (equal '(/ (+ (a 2) (- b 4))) (root-node new-loc)))))

(ert-deftest test-remove-node-at-rightmost-node ()
  (let* ((loc (rightmost (-> *tree* :down :right :down :right :down)))
         (new-loc (remove-node loc)))
    (should (equal '2 (car loc)))
    (should (equal 'a (car new-loc)))
    (should (equal '(*) (lefts new-loc)))
    (should (null (rights new-loc)))
    (should (equal '(/ (+ (* a) (- b 4))) (root-node new-loc)))))

(ert-deftest test-remove-node-at-nil ()
  (should (null (remove-node nil))))

(provide 'zippers-test)
;;; zippers-test.el ends here
