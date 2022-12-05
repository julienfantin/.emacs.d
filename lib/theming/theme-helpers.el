;;; theme-helpers.el --- Utilities for theme definition and customization
;;; Commentary:
;; TODO break this down in faces, remap and custom
;; TODO Minor mode to manage remap hooks and advices
;; TODO Unification-style API for remap?
;; With a plist, we can resolve face mappings as follows:
;; face -> spec (default)
;; face -> symbol (tag)
;; symbol -> spec (tag resolution)
;; This would allow to say that 'errors are red' and 'font-lock-error' is an
;; error, or that faces matching "error" or "invalid" etc are errors and hence
;; are red too.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'subr-x)
(require 'chroma)
(require 'shr-color)
(require 'loadhist)


;;; Faces

(defun theme-faces--match (faces &rest regexps)
  "Return a list of faces symbol matching any one of 'REGEXPS'.
Elements of REGEXPS can also be a list of '(\"match-pattern\"
\"exclude-pattern1\" \"exclude-pattern2)."
  (seq-let (regexp &rest more) regexps
    (cl-typecase regexp
      (null nil)
      ;; Filter faces matching first regexp and recurse
      (string
       (append
        (seq-filter (lambda (face) (string-match-p regexp (symbol-name face))) faces)
        (cl-mapcan (apply-partially #'theme-faces--match faces) more)))
      ;; Assume first string in list is match pattern and the rest of the list
      ;; are exclude patterns
      (list
       (seq-let (regexp &rest exclude-patterns) regexp
         (append
          (seq-remove
           (lambda (face)
             (seq-some
              (lambda (exclude-pattern)
                (string-match-p exclude-pattern (symbol-name face)))
              exclude-patterns))
           (theme-faces--match faces regexp))
          (cl-mapcan (apply-partially #'theme-faces--match faces) more)))))))

(defun theme-faces-match (&rest regexps)
  (apply 'theme-faces--match (face-list) regexps))

(defun theme-faces--match-all (faces &rest regexps)
  "Return a list of faces matching all 'REGEXPS'.
'REGEXPS' can be composed of lists, similar to 'theme-faces-match'."
  (let ((acc '()))
    (dolist (regexp regexps acc)
      (let ((found (theme-faces--match faces regexp)))
        (if (seq-empty-p acc)
            (setq acc found)
          (sort (setq acc (seq-intersection acc found)) #'string-lessp))))))

(defun theme-faces-match-all (&rest regexps)
  (apply 'theme-faces--match-all (face-list) regexps))


;;; Theme definition shorthands
;;;; Face specs

(defun theme-expand-faces (bindings)
  "TODO integrate class from color palette?"
  (cl-reduce
   (lambda (acc binding)
     (let ((face (car binding))
           (props (cadr binding)))
       (cond
        ((null face) acc)
        ((facep face) (append acc (list binding)))
        ((face-list-p face) (append acc (mapcar (lambda (face) (list face props)) face)))
        (t acc))))
   bindings
   :initial-value '()))

(defun theme-map-properties-pairs (f face-customization)
  (let ((face (car face-customization))
        (class (cl-caaadr face-customization))
        (props (car (cl-cdaadr face-customization))))
    `(,face
      ((,class
        ,(cl-mapcan
          (apply-partially #'apply f)
          (seq-partition props 2)))))))

(defvar theme-shorthands-alist
  '((:f   . :foreground)
    (:bg  . :background)
    (:b   . :background)
    (:F   . :distant-foreground)
    (:df  . :distant-foreground)
    (:dfg . :distant-foreground)
    (:=   . :inherit)
    (:>   . :inherit)
    (:<   . :inherit)
    (:w   . :weight)))

;;;; Colors

(defun theme-expand-color (kw value)
  "Return a list of keyword, 'chroma'.

If 'KW' is a shorthand, it will be expanded according to
'theme-shorthands-alist'.

'VALUE' is a string compatible with 'chroma-to-string'."
  (list
   (or (alist-get kw theme-shorthands-alist) kw)
   (chroma-to-string value)))

(defun theme-expand-colors (bindings)
  "Expand 'BINDINGS' with 'theme-expand-color'."
  (mapcar (apply-partially #'theme-map-properties-pairs #'theme-expand-color)
          bindings))


;;; API

;;;###autoload
(defun theme-custom-set-faces (theme bindings)
  "TODO integrate class from color palette"
  (thread-last bindings
               (theme-expand-faces)
               (theme-expand-colors)
               (apply #'custom-theme-set-faces theme)))

(provide 'theme-helpers)
;;; theme-helpers.el ends here
