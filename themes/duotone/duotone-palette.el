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

(defcustom duotone-palette-mblend 0.6
  "Default match blend value.

Used in blending background of highlighted matches."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-sblend 0.1
  "Default subtle blend value.

Used in blending background of strings and docstrings when the
duotone-background-* options are enabled."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-fade 0.4
  "Default fade value."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-mfade 0.2
  "Default fade value."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-sfade 0.05
  "Default fade value."
  :group 'duotone
  :type 'float)


;; * Defaults

(defvar duotone-palette-default-chroma-added    (chroma-hex :hex "#2acb34"))
(defvar duotone-palette-default-chroma-renamed  (chroma-hex :hex "#437fdc"))
(defvar duotone-palette-default-chroma-removed  (chroma-hex :hex "#fb4847"))
(defvar duotone-palette-default-chroma-modified (chroma-hex :hex "#fbb825"))


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
   (mblend   :reader mblend   :initarg :mblend   :tyle float)
   (sblend   :reader sblend   :initarg :sblend   :type float)
   (fade     :reader fade     :initarg :fade     :type float)
   (mfade    :reader mfade    :initarg :mfade    :type float)
   (sfade    :reader sfade    :initarg :sfade    :type float))
  (:documentation "Duotone palette."))

(defun duotone-palette-make-chroma (hue chroma-or-list)
  "Return a chroma from a a 'HUE' and a list of saturation, luminance.

If the list contains 3 elements the hue is ignored.
If 'CHROMA-OR-LIST' is a 'chroma' return it."
  (cond
   ((eq '() chroma-or-list) nil)
   ((cl-typep chroma-or-list 'chroma) (chroma-to-hsl chroma-or-list))
   (t (cl-case (length chroma-or-list)
        (2 (chroma-hsl (cons hue chroma-or-list)))
        (3 (chroma-hsl chroma-or-list))))))

(cl-defmethod initialize-instance :around
  ((p duotone-palette) &optional args)
  (let* ((fade     (or (plist-get args :fade) duotone-palette-fade))
         (mfade    (or (plist-get args :mfade) duotone-palette-mfade))
         (sfade    (or (plist-get args :sfade) duotone-palette-sfade))
         (blend    (or (plist-get args :blend) duotone-palette-blend))
         (mblend   (or (plist-get args :mblend) duotone-palette-mblend))
         (sblend   (or (plist-get args :sblend) duotone-palette-sblend))
         ;; Uno
         (uno      (plist-get args :uno))
         (uno-1    (duotone-palette-make-chroma uno (plist-get args :uno-1)))
         (uno-2    (duotone-palette-make-chroma uno (plist-get args :uno-2)))
         (uno-3    (duotone-palette-make-chroma uno (plist-get args :uno-3)))
         (uno-4    (duotone-palette-make-chroma uno (plist-get args :uno-4)))
         ;; Duo
         (duo      (plist-get args :duo))
         (duo-1    (duotone-palette-make-chroma duo (plist-get args :duo-1)))
         (duo-2    (duotone-palette-make-chroma duo (plist-get args :duo-2)))
         (duo-3    (duotone-palette-make-chroma duo (plist-get args :duo-3)))
         ;; fg/bg
         (fg       (or (duotone-palette-make-chroma uno (plist-get args :fg)) uno-2))
         (bg       (duotone-palette-make-chroma uno (plist-get args :bg)))
         ;; accents
         (accent   (or (duotone-palette-make-chroma duo (plist-get args :accent)) duo-2))
         (renamed  (or (duotone-palette-make-chroma duo (plist-get args :renamed)) (chroma-blend bg duotone-palette-default-chroma-renamed blend)))
         (added    (or (duotone-palette-make-chroma duo (plist-get args :added)) (chroma-blend bg duotone-palette-default-chroma-added blend)))
         (modified (or (duotone-palette-make-chroma duo (plist-get args :modified)) (chroma-blend bg duotone-palette-default-chroma-modified blend)))
         (removed  (or (duotone-palette-make-chroma duo (plist-get args :removed)) (chroma-blend bg duotone-palette-default-chroma-removed blend))))
    (oset p :uno-1    uno-1)
    (oset p :uno-2    uno-2)
    (oset p :uno-3    uno-3)
    (oset p :uno-4    uno-4)
    (oset p :duo-1    duo-1)
    (oset p :duo-2    duo-2)
    (oset p :duo-3    duo-3)
    (oset p :fg       fg)
    (oset p :bg       bg)
    (oset p :accent   accent)
    (oset p :renamed  renamed)
    (oset p :modified modified)
    (oset p :added    added)
    (oset p :removed  removed)
    (oset p :fade     fade)
    (oset p :mfade    mfade)
    (oset p :sfade    sfade)
    (oset p :blend    blend)
    (oset p :sblend   sblend)
    (oset p :mblend   mblend)
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
   :mfade    (mfade p)
   :sfade    (sfade p)
   :blend    (blend p)
   :mblend   (mblend p)
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
