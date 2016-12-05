;;; chroma.el --- Eieio color interface -*- lexical-binding: t ; -*-
;; Copyright (C) 2015 Free Software Foundation, Inc.
;; Author:
;; URL:
;; Package-Version:
;; Keywords: convenience, lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;;; Commentary:
;; Usage
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'seq)
(require 'subr-x)
(require 'eieio)
(require 'color)


;; * Object system
;; ** Abstract type & Generic methods

(defclass chroma () ((:abstract t)))

(cl-defgeneric chroma-to-hex (c))
(cl-defgeneric chroma-to-rgb (c))
(cl-defgeneric chroma-to-hsl (c))
(cl-defgeneric chroma-to-lab (c))
(cl-defgeneric chroma-to-hsv (c))
(cl-defgeneric chroma-to-hcl (c))

(cl-defgeneric chroma-to-string (c))
(cl-defgeneric chroma-to-list (c))

(cl-defgeneric chroma-clone (c))

;; ** Color types and constructors

(defun chroma--norm (r x)
  "Divide by 'R' unless 'X' is a float."
  (if (integerp x) (/ (float x) (float r)) x))

(defun chroma--munge-slots (keys integer-ratios slots)
  (cl-typecase (car slots)
    (null
     (error "Can't initialize color with nil"))
    (keyword
     (cl-mapcan
      #'list
      (cl-remove-if-not  #'keywordp slots)
      (cl-mapcar #'chroma--norm integer-ratios (cl-remove-if #'keywordp slots))))
    (number
     (cl-mapcan
      (lambda (slot r x) (list slot (chroma--norm r x)))
      keys integer-ratios slots))
    (list
     (chroma--munge-slots keys integer-ratios (car slots)))))

;; *** Hexadecimal

(defclass chroma-hex (chroma)
  ((hex :initarg :hex :type string))
  (:documentation "Hex chroma object"))

;; *** RGB

(defclass chroma-rgb (chroma)
  ((r :initarg :r :type float :initform 0.0)
   (g :initarg :g :type float :initform 0.0)
   (b :initarg :b :type float :initform 0.0))
  (:documentation "RGB chroma object"))

(cl-defmethod initialize-instance :around
  ((c chroma-rgb) &optional slots)
  (thread-last slots
    (chroma--munge-slots '(:r :g :b) (make-list 3 255.0))
    (cl-call-next-method c)))

;; *** HSL

(defclass chroma-hsl (chroma)
  ((h :initarg :h :type float :initform 0.0)
   (s :initarg :s :type float :initform 0.0)
   (l :initarg :l :type float :initform 0.0))
  (:documentation "HSL chroma object"))

(cl-defmethod initialize-instance :around
  ((c chroma-hsl) &optional slots)
  (thread-last slots
    (chroma--munge-slots '(:h :s :l) '(360.0 100.0 100.0))
    (cl-call-next-method c)))

(cl-defmethod chroma-clone ((c chroma-hsl))
  (with-slots (h s l) c
    (make-instance 'chroma-hsl :h h :s s :l l)))

(cl-defmethod chroma-clone ((c chroma-rgb))
  (with-slots (r g b) c
    (make-instance 'chroma-rgb :r r :g g :b b)))

;; *** Cielab

(defclass chroma-cielab (chroma)
  ((l :initarg :l :type float :initform 0.0)
   (a :initarg :a :type float :initform 0.0)
   (b :initarg :b :type float :initform 0.0))
  (:documentation "Cielab chroma object"))

;; ** To Conversions

(defun fpct (f)
  (if (and (floatp f) (< f 1.0))
      (* f 100.0)
    f))

;; *** Hexadecimal

(cl-defmethod chroma-to-hex ((c chroma-hex))
  c)

(cl-defmethod chroma-to-hex ((c chroma-rgb))
  (with-slots (r g b) c
    (chroma-hex :hex (color-rgb-to-hex r g b))))

(cl-defmethod chroma-to-hex ((c chroma-hsl))
  (with-slots (r g b) (chroma-to-rgb c)
    (chroma-hex :hex (color-rgb-to-hex r g b))))

;; *** RGB

(cl-defmethod chroma-to-rgb ((c chroma-rgb)) c)

(cl-defmethod chroma-to-rgb ((c chroma-hex))
  (with-slots (hex) c
    (seq-let (r g b) (color-name-to-rgb hex)
      (chroma-rgb :r r :g g :b b))))

(cl-defmethod chroma-to-rgb ((c chroma-hsl))
  (with-slots (h s l) c
    (seq-let (r g b) (color-hsl-to-rgb h s l)
      (chroma-rgb :r r :g g :b b))))

;; *** HSL

(cl-defmethod chroma-to-hsl ((c chroma-hsl)) c)

(cl-defmethod chroma-to-hsl ((c chroma-hex))
  (with-slots (hex) c
    (seq-let (h s l) (apply #'color-rgb-to-hsl (color-name-to-rgb hex))
      (chroma-hsl :h h :s s :l l))))

(cl-defmethod chroma-to-hsl ((c chroma-rgb))
  (with-slots (r g b) c
    (seq-let (h s l) (color-rgb-to-hsl r g b)
      (chroma-hsl :h h :s s :l l))))

;; *** Cielab

(cl-defmethod chroma-to-cielab ((c chroma-rgb))
  (with-slots (r g b) c
    (seq-let (l a b) (color-srgb-to-lab r g b)
      (make-instance 'chroma-cielab :l l :a a :b b))))

(cl-defmethod chroma-to-cielab ((c chroma-hex))
  (chroma-to-cielab (chroma-to-rgb c)))

;; *** String

(cl-defmethod chroma-to-string ((c t)) c)

(cl-defmethod chroma-to-string ((c chroma-hex))
             (oref c hex))

(cl-defmethod chroma-to-string ((c chroma-rgb))
  (with-slots (r g b) c
    (color-rgb-to-hex r g b)))

(cl-defmethod chroma-to-string ((c chroma-hsl))
  (with-slots (h s l) c
    (apply #'color-rgb-to-hex (color-hsl-to-rgb h s l))))

(cl-defmethod chroma-to-string ((c chroma-cielab))
  (with-slots (l a b) c
    (apply #'color-rgb-to-hex (color-lab-to-srgb l a b))))

;; *** List

(cl-defmethod chroma-to-list ((c chroma-rgb))
  (with-slots (r g b) c (list r g b)))

(cl-defmethod chroma-to-list ((c chroma-hsl))
  (with-slots (h s l) c (list h s l)))

(cl-defmethod chroma-to-list ((c chroma-cielab))
  (with-slots (l a b) c (list l a b)))


;; * Functions

(defun chroma-lighten (c pct)
  (with-slots (h s l) (chroma-to-hsl c)
    (seq-let (h* s* l*) (color-lighten-hsl h s l (fpct pct))
      (chroma-hsl :h h* :s s* :l l*))))

(defun chroma-darken (c pct)
  (with-slots (h s l) (chroma-to-hsl c)
    (seq-let (h* s* l*) (color-darken-hsl h s l (fpct pct))
      (chroma-hsl :h h* :s s* :l l*))))

(defun chroma-saturate (c pct)
  (with-slots (h s l) (chroma-to-hsl c)
    (seq-let (h* s* l*) (color-saturate-hsl h s l (fpct pct))
      (chroma-hsl :h h* :s s* :l l*))))

(defun chroma-desaturate (c pct)
  (with-slots (h s l) (chroma-to-hsl c)
    (seq-let (h* s* l*) (color-desaturate-hsl h s l (fpct pct))
      (chroma-hsl :h h* :s s* :l l*))))

(defun chroma-gradient (from to n)
  (with-slots (r g b) (chroma-to-rgb from)
    (seq-let (r* g* b*) (chroma-to-rgb to)
      (color-gradient (list r g b) (list r* g* b*) n))))

(defun chroma--blend (ratio x y)
  (+ (* x (- 1 ratio)) (* y ratio)))

(defun chroma-blend (from to pct)
  (chroma-rgb
   (cl-mapcar
    (apply-partially #'chroma--blend (/ (fpct pct) 100.0))
    (chroma-to-list (chroma-to-rgb from))
    (chroma-to-list (chroma-to-rgb to)))))


;; * Color theory

(defun chroma--iterate (c n fn &rest args)
  (let ((value c))
    (dotimes (_ n value)
      (setq value (apply fn value args)))))

(defun chroma--iterate* (c n fn &rest args)
  (let ((acc (list c))
        (value c))
    (dotimes (_ n (reverse acc))
      (setq value (apply fn value args))
      (setq acc (cons value acc)))))

;; ** Tints (addition of white)

(defun chroma-tints (c n pct)
  (chroma--iterate* c n #'chroma-lighten pct))

;; ** Shades (addition of black)

(defun chroma-shades (c n pct)
  (chroma--iterate* c n #'chroma-darken pct))

;; ** Complements

(defun chroma-complement (c)
  (thread-last c
    (chroma-to-list)
    (mapcar (apply-partially #'- 1.0))
    (chroma-rgb)))

;; ** Distance

(cl-defmethod chroma-distance ((c1 chroma-cielab) (c2 chroma-cielab))
             (color-cie-de2000 (chroma-to-list c1) (chroma-to-list c2)))

(cl-defmethod chroma-distance ((c1 t) (c2 t))
  (color-cie-de2000
   (with-slots (r g b) (chroma-to-rgb c1)
     (color-srgb-to-lab r g b))
   (with-slots (r g b) (chroma-to-rgb c2)
     (color-srgb-to-lab r g b))))

(defun chroma-comparator (c0)
  (lambda (c1 c2)
    (< (chroma-distance c0 c1) (chroma-distance c0 c2))))

(defun chroma-closest (chromas chroma)
  (car (sort chromas (chroma-comparator chroma))))

;; ** Fade

(defun chroma-fade (c pct)
  (if (< (chroma-distance c (chroma-hex :hex "#000"))
         (chroma-distance c (chroma-hex :hex "#fff")))
      (chroma-lighten c (fpct pct))
    (chroma-darken c (fpct pct))))


;; * Seq helpers

(defvar chroma-phi (/ (+ 1.0 (sqrt 5)) 2.0))

(defun chroma-phi-seq (f n &optional init)
  (let ((init (or init 1))
        (acc '()))
    (dotimes (_ n (reverse acc))
      (setq init (funcall f init chroma-phi))
      (setq acc (cons init acc)))))

(defun chroma-normalize-seq (seq)
  (let ((max-n (seq-max seq)))
    (seq-map (lambda (x) (/ x max-n)) seq)))

(provide 'chroma)
;;; chroma.el ends here
