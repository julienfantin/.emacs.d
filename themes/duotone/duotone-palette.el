;;; duotone-palette.el --- Duotone palette definition
;;; Commentary:
;;; Tasks
;;;; Add transforms
;; - swap palette slots
;;; Code:
(require 'cl-lib)
(require 'chroma)
(require 'eieio)


;;; Customs

(defcustom duotone-palette-blend 0.9
  "Default blend value."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-mblend 0.6
  "Default match blend value.

Used in blending background of highlighted matches."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-sblend 0.05
  "Default subtle blend value.

Used in blending background of strings and docstrings when the
duotone-background-* options are enabled."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-fade 0.4
  "Default fade value."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-mfade 0.1
  "Default fade value."
  :group 'duotone
  :type 'float)

(defcustom duotone-palette-sfade 0.05
  "Default fade value."
  :group 'duotone
  :type 'float)


;;; Defaults

(defvar duotone-palette-default-chroma-added    (chroma-hex :hex "#2acb34"))
(defvar duotone-palette-default-chroma-renamed  (chroma-hex :hex "#437fdc"))
(defvar duotone-palette-default-chroma-removed  (chroma-hex :hex "#fb4847"))
(defvar duotone-palette-default-chroma-modified (chroma-hex :hex "#fbb825"))


;;; Definition

(defvar duotone-palette-colors
  '(
    uno-1 uno-1-fade uno-1-mfade uno-1-sfade
    uno-2 uno-2-fade uno-2-mfade uno-2-sfade
    uno-3 uno-3-fade uno-3-mfade uno-3-sfade
    uno-4 uno-4-fade uno-4-mfade uno-4-sfade
    duo-1 duo-1-fade duo-1-mfade duo-1-sfade
    duo-2 duo-2-fade duo-2-mfade duo-2-sfade
    duo-3 duo-3-fade duo-3-mfade duo-3-sfade
    fg fg-fade fg-mfade fg-sfade
    bg bg-fade bg-mfade bg-sfade
    accent renamed added modified removed))

(defclass duotone-palette ()
  ((uno-1       :reader uno-1       :initarg :uno-1       :type chroma)
   (uno-1-fade  :reader uno-1-fade  :initarg :uno-1-fade  :type chroma)
   (uno-1-mfade :reader uno-1-mfade :initarg :uno-1-mfade :type chroma)
   (uno-1-sfade :reader uno-1-sfade :initarg :uno-1-sfade :type chroma)
   (uno-2       :reader uno-2       :initarg :uno-2       :type chroma)
   (uno-2-fade  :reader uno-2-fade  :initarg :uno-2-fade  :type chroma)
   (uno-2-mfade :reader uno-2-mfade :initarg :uno-2-mfade :type chroma)
   (uno-2-sfade :reader uno-2-sfade :initarg :uno-2-sfade :type chroma)
   (uno-3       :reader uno-3       :initarg :uno-3       :type chroma)
   (uno-3-fade  :reader uno-3-fade  :initarg :uno-3-fade  :type chroma)
   (uno-3-mfade :reader uno-3-mfade :initarg :uno-3-mfade :type chroma)
   (uno-3-sfade :reader uno-3-sfade :initarg :uno-3-sfade :type chroma)
   (uno-4       :reader uno-4       :initarg :uno-4       :type chroma)
   (uno-4-fade  :reader uno-4-fade  :initarg :uno-4-fade  :type chroma)
   (uno-4-mfade :reader uno-4-mfade :initarg :uno-4-mfade :type chroma)
   (uno-4-sfade :reader uno-4-sfade :initarg :uno-4-sfade :type chroma)
   (duo-1       :reader duo-1       :initarg :duo-1       :type chroma)
   (duo-1-fade  :reader duo-1-fade  :initarg :duo-1-fade  :type chroma)
   (duo-1-mfade :reader duo-1-mfade :initarg :duo-1-mfade :type chroma)
   (duo-1-sfade :reader duo-1-sfade :initarg :duo-1-sfade :type chroma)
   (duo-2       :reader duo-2       :initarg :duo-2       :type chroma)
   (duo-2-fade  :reader duo-2-fade  :initarg :duo-2-fade  :type chroma)
   (duo-2-mfade :reader duo-2-mfade :initarg :duo-2-mfade :type chroma)
   (duo-2-sfade :reader duo-2-sfade :initarg :duo-2-sfade :type chroma)
   (duo-3       :reader duo-3       :initarg :duo-3       :type chroma)
   (duo-3-fade  :reader duo-3-fade  :initarg :duo-3-fade  :type chroma)
   (duo-3-mfade :reader duo-3-mfade :initarg :duo-3-mfade :type chroma)
   (duo-3-sfade :reader duo-3-sfade :initarg :duo-3-sfade :type chroma)
   (fg          :reader fg          :initarg :fg          :type chroma)
   (fg-fade     :reader fg-fade     :initarg :fg-fade     :type chroma)
   (fg-mfade    :reader fg-mfade    :initarg :fg-mfade    :type chroma)
   (fg-sfade    :reader fg-sfade    :initarg :fg-sfade    :type chroma)
   (bg          :reader bg          :initarg :bg          :type chroma)
   (bg-fade     :reader bg-fade     :initarg :bg-fade     :type chroma)
   (bg-mfade    :reader bg-mfade    :initarg :bg-mfade    :type chroma)
   (bg-sfade    :reader bg-sfade    :initarg :bg-sfade    :type chroma)
   (accent      :reader accent      :initarg :accent      :type chroma)
   (renamed     :reader renamed     :initarg :renamed     :type chroma)
   (added       :reader added       :initarg :added       :type chroma)
   (modified    :reader modified    :initarg :modified    :type chroma)
   (removed     :reader removed     :initarg :removed     :type chroma)
   (blend       :reader blend       :initarg :blend       :type float)
   (mblend      :reader mblend      :initarg :mblend      :type float)
   (sblend      :reader sblend      :initarg :sblend      :type float)
   (fade        :reader fade        :initarg :fade        :type float)
   (mfade       :reader mfade       :initarg :mfade       :type float)
   (sfade       :reader sfade       :initarg :sfade       :type float))
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
         (uno         (plist-get args :uno))

         (uno-1       (duotone-palette-make-chroma uno (plist-get args :uno-1)))
         (uno-1-fade  (chroma-fade uno-1 fade))
         (uno-1-mfade (chroma-fade uno-1 mfade))
         (uno-1-sfade (chroma-fade uno-1 sfade))

         (uno-2       (duotone-palette-make-chroma uno (plist-get args :uno-2)))
         (uno-2-fade  (chroma-fade uno-2 fade))
         (uno-2-mfade (chroma-fade uno-2 mfade))
         (uno-2-sfade (chroma-fade uno-2 sfade))

         (uno-3       (duotone-palette-make-chroma uno (plist-get args :uno-3)))
         (uno-3-fade  (chroma-fade uno-3 fade))
         (uno-3-mfade (chroma-fade uno-3 mfade))
         (uno-3-sfade (chroma-fade uno-3 sfade))

         (uno-4       (duotone-palette-make-chroma uno (plist-get args :uno-4)))
         (uno-4-fade  (chroma-fade uno-4 fade))
         (uno-4-mfade (chroma-fade uno-4 mfade))
         (uno-4-sfade (chroma-fade uno-4 sfade))

         ;; Duo
         (duo         (plist-get args :duo))

         (duo-1       (duotone-palette-make-chroma duo (plist-get args :duo-1)))
         (duo-1-fade  (chroma-fade duo-1 fade))
         (duo-1-mfade (chroma-fade duo-1 mfade))
         (duo-1-sfade (chroma-fade duo-1 sfade))

         (duo-2       (duotone-palette-make-chroma duo (plist-get args :duo-2)))
         (duo-2-fade  (chroma-fade duo-2 fade))
         (duo-2-mfade (chroma-fade duo-2 mfade))
         (duo-2-sfade (chroma-fade duo-2 sfade))

         (duo-3       (duotone-palette-make-chroma duo (plist-get args :duo-3)))
         (duo-3-fade  (chroma-fade duo-3 fade))
         (duo-3-mfade (chroma-fade duo-3 mfade))
         (duo-3-sfade (chroma-fade duo-3 sfade))

         ;; fg/bg
         (fg       (or (duotone-palette-make-chroma uno (plist-get args :fg)) uno-2))
         (fg-fade  (chroma-fade fg fade))
         (fg-mfade (chroma-fade fg mfade))
         (fg-sfade (chroma-fade fg sfade))

         (bg       (duotone-palette-make-chroma uno (plist-get args :bg)))
         (bg-fade  (chroma-fade bg fade))
         (bg-mfade (chroma-fade bg mfade))
         (bg-sfade (chroma-fade bg sfade))
         ;; accents
         (accent   (or (duotone-palette-make-chroma duo (plist-get args :accent)) duo-2))
         (renamed  (or (duotone-palette-make-chroma duo (plist-get args :renamed)) (chroma-blend bg duotone-palette-default-chroma-renamed blend)))
         (added    (or (duotone-palette-make-chroma duo (plist-get args :added)) (chroma-blend bg duotone-palette-default-chroma-added blend)))
         (modified (or (duotone-palette-make-chroma duo (plist-get args :modified)) (chroma-blend bg duotone-palette-default-chroma-modified blend)))
         (removed  (or (duotone-palette-make-chroma duo (plist-get args :removed)) (chroma-blend bg duotone-palette-default-chroma-removed blend))))
    (oset p :uno-1 uno-1)
    (oset p :uno-1-fade uno-1-fade)
    (oset p :uno-1-mfade uno-1-mfade)
    (oset p :uno-1-sfade uno-1-sfade)
    (oset p :uno-2 uno-2)
    (oset p :uno-2-fade uno-2-fade)
    (oset p :uno-2-mfade uno-2-mfade)
    (oset p :uno-2-sfade uno-2-sfade)
    (oset p :uno-3 uno-3)
    (oset p :uno-3-fade uno-3-fade)
    (oset p :uno-3-mfade uno-3-mfade)
    (oset p :uno-3-sfade uno-3-sfade)
    (oset p :uno-4 uno-4)
    (oset p :uno-4-fade uno-4-fade)
    (oset p :uno-4-mfade uno-4-mfade)
    (oset p :uno-4-sfade uno-4-sfade)
    (oset p :duo-1 duo-1)
    (oset p :duo-1-fade duo-1-fade)
    (oset p :duo-1-mfade duo-1-mfade)
    (oset p :duo-1-sfade duo-1-sfade)
    (oset p :duo-2 duo-2)
    (oset p :duo-2-fade duo-2-fade)
    (oset p :duo-2-mfade duo-2-mfade)
    (oset p :duo-2-sfade duo-2-sfade)
    (oset p :duo-3 duo-3)
    (oset p :duo-3-fade duo-3-fade)
    (oset p :duo-3-mfade duo-3-mfade)
    (oset p :duo-3-sfade duo-3-sfade)
    (oset p :fg fg)
    (oset p :fg-fade fg-fade)
    (oset p :fg-mfade fg-mfade)
    (oset p :fg-sfade fg-sfade)
    (oset p :bg bg)
    (oset p :bg-fade bg-fade)
    (oset p :bg-mfade bg-mfade)
    (oset p :bg-sfade bg-sfade)
    (oset p :accent accent)
    (oset p :renamed renamed)
    (oset p :modified modified)
    (oset p :added added)
    (oset p :removed removed)
    (oset p :fade fade)
    (oset p :mfade mfade)
    (oset p :sfade sfade)
    (oset p :blend blend)
    (oset p :sblend sblend)
    (oset p :mblend mblend)
    (cl-call-next-method p nil)))


;;; Transforms

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

(provide 'duotone-palette)
;;; duotone-palette.el ends here
