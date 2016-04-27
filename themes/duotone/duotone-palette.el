;;; duotone-palette.el --- Duotone palette definition
;;; Commentary:
;; * Tasks
;; ** Add transforms
;; - swap palette slots
;;; Code:
(require 'cl-lib)
(require 'chroma)
(require 'eieio)


;; * Customs

(defcustom duotone-palette-blend 0.9
  "Default blend value."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-sblend 0.02
  "Default subtle blend value.
Used in blending background of strings and docstrings when the
duotone-background-* options are enabled."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-fade 0.1
  "Default fade value."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-sfade 0.03
  "Default fade value."
  :group 'duotone
  :type 'float)


;; * Definition

(defvar duotone-palette-colors
  '(uno-1 uno-2 uno-3 uno-4 duo-1 duo-2 duo-3 fg bg accent renamed added modified removed))

(defclass duotone-palette ()
  ((uno-1    :reader uno-1    :initarg :uno-1    :type chroma)
   (uno-2    :reader uno-2    :initarg :uno-2    :type chroma)
   (uno-3    :reader uno-3    :initarg :uno-3    :type chroma)
   (uno-4    :reader uno-4    :initarg :uno-4    :type chroma)
   (duo-1    :reader duo-1    :initarg :duo-1    :type chroma)
   (duo-2    :reader duo-2    :initarg :duo-2    :type chroma)
   (duo-3    :reader duo-3    :initarg :duo-3    :type chroma)
   (fg       :reader fg       :initarg :fg       :type chroma)
   (bg       :reader bg       :initarg :bg       :type chroma)
   (accent   :reader accent   :initarg :accent   :type chroma)
   (renamed  :reader renamed  :initarg :renamed  :type chroma)
   (added    :reader added    :initarg :added    :type chroma)
   (modified :reader modified :initarg :modified :type chroma)
   (removed  :reader removed  :initarg :removed  :type chroma)
   (blend    :reader blend    :initarg :blend    :type float)
   (sblend   :reader sblend   :initarg :sblend   :type float)
   (fade     :reader fade     :initarg :fade     :type float)
   (sfade    :reader sfade    :initarg :sfade    :type float))
  (:documentation "Duotone palette."))

(defun duotone-palette-make-chroma (hue chroma-or-list)
  "Return a chroma from a a 'HUE' and a list of saturation, luminance.

If the list contains 3 elements the hue is ignored.
If 'CHROMA-OR-LIST' is a 'chroma' return it."
  (cond
   ((cl-typep chroma-or-list 'chroma) (chroma-to-hsl chroma-or-list))
   (t (cl-case (length chroma-or-list)
        (2 (chroma-hsl (cons hue chroma-or-list)))
        (3 (chroma-hsl chroma-or-list))))))

(cl-defmethod initialize-instance :around
  ((p duotone-palette) &optional args)
  (let ((uno      (plist-get args :uno))
        (uno-1    (plist-get args :uno-1))
        (uno-2    (plist-get args :uno-2))
        (uno-3    (plist-get args :uno-3))
        (uno-4    (plist-get args :uno-4))
        (duo      (plist-get args :duo))
        (duo-1    (plist-get args :duo-1))
        (duo-2    (plist-get args :duo-2))
        (duo-3    (plist-get args :duo-3))
        (fg       (plist-get args :fg))
        (bg       (plist-get args :bg))
        (accent   (plist-get args :accent))
        (renamed  (plist-get args :renamed))
        (added    (plist-get args :added))
        (modified (plist-get args :modified))
        (removed  (plist-get args :removed))
        (fade     (plist-get args :fade))
        (sfade    (plist-get args :sfade))
        (blend    (plist-get args :blend))
        (sblend   (plist-get args :sblend)))
    (oset p :uno-1    (duotone-palette-make-chroma uno uno-1))
    (oset p :uno-2    (duotone-palette-make-chroma uno uno-2))
    (oset p :uno-3    (duotone-palette-make-chroma uno uno-3))
    (oset p :uno-4    (duotone-palette-make-chroma uno uno-4))
    (oset p :duo-1    (duotone-palette-make-chroma duo duo-1))
    (oset p :duo-2    (duotone-palette-make-chroma duo duo-2))
    (oset p :duo-3    (duotone-palette-make-chroma duo duo-3))
    (oset p :fg       (duotone-palette-make-chroma uno (or fg uno-2)))
    (oset p :bg       (duotone-palette-make-chroma uno bg))
    (oset p :accent   (duotone-palette-make-chroma duo accent))
    (oset p :renamed  (duotone-palette-make-chroma duo (or renamed duo-3)))
    (oset p :modified (duotone-palette-make-chroma uno (or modified uno-3)))
    (oset p :added    (duotone-palette-make-chroma duo (or added duo-1)))
    (oset p :removed  (duotone-palette-make-chroma duo (or removed duo-3)))
    (oset p :fade     (or fade duotone-palette-fade))
    (oset p :sfade    (or sfade duotone-palette-sfade))
    (oset p :blend    (or blend duotone-palette-blend))
    (oset p :sblend   (or sblend duotone-palette-sblend))
    (cl-call-next-method p nil)))

;; TODO clone is built into eieieo!
(cl-defmethod duotone-palette-clone ((p duotone-palette))
  (make-instance
   'duotone-palette
   :uno-1    (chroma-clone (uno-1 p))
   :uno-2    (chroma-clone (uno-2 p))
   :uno-3    (chroma-clone (uno-3 p))
   :uno-4    (chroma-clone (uno-4 p))
   :duo-1    (chroma-clone (duo-1 p))
   :duo-2    (chroma-clone (duo-2 p))
   :duo-3    (chroma-clone (duo-3 p))
   :fg       (chroma-clone (fg p))
   :bg       (chroma-clone (bg p))
   :accent   (chroma-clone (accent p))
   :renamed  (chroma-clone (renamed p))
   :added    (chroma-clone (added p))
   :modified (chroma-clone (modified p))
   :removed  (chroma-clone (removed p))
   :fade     (fade p)
   :sfade    (sfade p)
   :blend    (blend p)
   :sblend   (sblend p)))


;; * Transforms

(cl-defmethod duotone-palette-chromas ((p duotone-palette))
  (mapcar (lambda (accessor) (funcall accessor p)) duotone-palette-colors))

(cl-defmethod duotone-palette-transform ((p duotone-palette) slot-value-predicate function)
  (seq-doseq (slot (cl--class-slots (cl--find-class 'duotone-palette)))
    (let* ((accessor (cl--slot-descriptor-name slot))
           (obj (slot-value p accessor)))
      (when (funcall slot-value-predicate obj)
        (setf (slot-value p accessor) (funcall function obj))))))

(cl-defmethod duotone-palette-scale-hsl ((p duotone-palette) h* s* l*)
  (duotone-palette-transform
   p
   (lambda (obj)
     (object-of-class-p obj 'chroma))
   (lambda (chrom)
     (with-slots (h s l) chrom
       (chroma-hsl :h (* h h*) :s (* s s*) :l (* l l*))))))

(cl-defmethod duotone-palette-invert ((p duotone-palette))
  (let* ((palette (duotone-palette-clone p)))
    (with-slots (fg bg) palette
      (setf (slot-value palette 'bg) fg)
      (setf (slot-value palette 'fg) bg)
      palette)))

(provide 'duotone-palette)
;;; duotone-palette.el ends here
