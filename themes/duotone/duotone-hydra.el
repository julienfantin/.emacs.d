;;; duotone-hydra.el --- Hydra for Duotone theme -*- lexical-binding: t ; -*-
;;; Commentary:
;;
;;; Code:
(require 'duotone-theme)
(require 'duotone-palette)
(require 'hydra)

;;; State

(defvar duotone-hydra-current-palette nil)
(defvar duotone-hydra-current-palette-slots '())
(defvar duotone-hydra-current-chroma-slots '())
(defvar duotone-hydra-ratio 0.1)
(defvar duotone-hydra-chroma-slots-ranges
  `((h . (0 1.0))
    (s . (0.0 1.0))
    (l . (0.0 1.0))))

;;; Updates

(defun duotone-hydra-palette-chromas ()
  "Return all chromas for the current palette."
  (mapcar
   (lambda (slot)
     (slot-value duotone-hydra-current-palette slot))
   (or duotone-hydra-current-palette-slots
       duotone-palette-colors)))

(defun duotone-hydra-update (sgn &optional n)
  "Update currently selected chromas applying 'SGN' to a scaled 'N'."
  (dolist (chrom (duotone-hydra-palette-chromas))
    (dolist (slot (or duotone-hydra-current-chroma-slots '(h s l)))
      (let* ((val (slot-value chrom slot))
             (range (alist-get slot duotone-hydra-chroma-slots-ranges))
             (min* (car range))
             (max* (cadr range))
             (val* (* (or n 1.0) (* duotone-hydra-ratio max*))))
        (setf (slot-value chrom slot)
              (funcall sgn val (min (max min* val*) max*))))))
  (let ((duotone-default-palette duotone-hydra-current-palette))
    (load-theme 'duotone nil)))


;;; Commands

(defun duotone-hydra-inc (&optional n)
  (interactive "p")
  (duotone-hydra-update #'+ n))

(defun duotone-hydra-dec (&optional n) (interactive "p") (duotone-hydra-update #'- n))

(defun duotone-hydra-inc-step (&optional n)
  (interactive "p")
  (setq duotone-hydra-ratio (* 2 duotone-hydra-ratio)))

(defun duotone-hydra-dec-step (&optional n)
  (interactive "p")
  (setq duotone-hydra-ratio (* 0.5 duotone-hydra-ratio)))

(defun duotone-hydra-toggle-chroma-slots (&rest slots)
  (dolist (slot slots)
    (setq
     duotone-hydra-current-chroma-slots
     (if (member slot duotone-hydra-current-chroma-slots)
         (delq slot duotone-hydra-current-chroma-slots)
       (push slot duotone-hydra-current-chroma-slots)))))

(defun duotone-hydra-toggle-palette-slots (&rest slots)
  (dolist (slot slots)
    (setq
     duotone-hydra-current-palette-slots
     (if (member slot duotone-hydra-current-palette-slots)
         (delq slot duotone-hydra-current-palette-slots)
       (push slot duotone-hydra-current-palette-slots)))))

(defun duotone-hydra-save ()
  (interactive)
  (setq duotone-default-palette duotone-hydra-current-palette))


;;; Hydra

(defun duotone-hydra-chroma-slot-status (slot)
  (format "%s" (if (member slot duotone-hydra-current-chroma-slots) "*ON*" "")))

(defun duotone-hydra-chroma-status (slot)
  (if (not (member slot duotone-hydra-current-palette-slots))
      ""
    (let* ((chrom (slot-value duotone-hydra-current-palette slot))
           (color (chroma-to-string chrom)))
      (propertize
       (symbol-name slot) 'face
       `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color)) "white" "black"))
         (:background ,color))))))

(defhydra duotone-hydra
  (:body-pre
   (setq hydra-lv t
         duotone-hydra-current-palette (duotone-palette-clone duotone-default-palette))
   :columns 8
   :color pink)
  ("=" (setq duotone-hydra-current-palette (duotone-palette-% `clone duotone-default-palette)))
  ("+" duotone-hydra-inc-step (format "%s +step" duotone-hydra-ratio))
  ("-" duotone-hydra-dec-step "-step")
  ("p" duotone-hydra-inc "increment")
  ("n" duotone-hydra-dec "decrement")
  ("w" (insert (object-write duotone-hydra-current-palette)))
  ("h" (duotone-hydra-toggle-chroma-slots 'h)      (format "%s" (duotone-hydra-chroma-slot-status 'h)))
  ("s" (duotone-hydra-toggle-chroma-slots 's)      (format "%s" (duotone-hydra-chroma-slot-status 's)))
  ("l" (duotone-hydra-toggle-chroma-slots 'l)      (format "%s" (duotone-hydra-chroma-slot-status 'l)))
  ("f" (duotone-hydra-toggle-palette-slots 'fg)    (format "%s" (duotone-hydra-chroma-status 'fg)))
  ("b" (duotone-hydra-toggle-palette-slots 'bg)    (format "%s" (duotone-hydra-chroma-status 'bg)))
  ("u" (duotone-hydra-toggle-palette-slots 'uno-1 'uno-2 'uno-3 'uno-4) "unos")
  ("d" (duotone-hydra-toggle-palette-slots 'duo-1 'duo-2 'duo-3) "duos")
  ("1" (duotone-hydra-toggle-palette-slots 'uno-1) (format "%s" (duotone-hydra-chroma-status 'uno-1)))
  ("2" (duotone-hydra-toggle-palette-slots 'uno-2) (format "%s" (duotone-hydra-chroma-status 'uno-2)))
  ("3" (duotone-hydra-toggle-palette-slots 'uno-3) (format "%s" (duotone-hydra-chroma-status 'uno-3)))
  ("4" (duotone-hydra-toggle-palette-slots 'uno-4) (format "%s" (duotone-hydra-chroma-status 'uno-4)))
  ("5" (duotone-hydra-toggle-palette-slots 'duo-1) (format "%s" (duotone-hydra-chroma-status 'duo-1)))
  ("6" (duotone-hydra-toggle-palette-slots 'duo-2) (format "%s" (duotone-hydra-chroma-status 'duo-2)))
  ("7" (duotone-hydra-toggle-palette-slots 'duo-3) (format "%s" (duotone-hydra-chroma-status 'duo-3)))
  ("~" (duotone-hydra-toggle-palette-slots 'accent) (format "%s" (duotone-hydra-chroma-status 'accent)))
  ("!" (duotone-hydra-toggle-palette-slots 'added) (format "%s" (duotone-hydra-chroma-status 'added)))
  ("@" (duotone-hydra-toggle-palette-slots 'renamed) (format "%s" (duotone-hydra-chroma-status 'renamed)))
  ("#" (duotone-hydra-toggle-palette-slots 'removed) (format "%s" (duotone-hydra-chroma-status 'removed)))
  ("$" (duotone-hydra-toggle-palette-slots 'modified) (format "%s" (duotone-hydra-chroma-status 'modified)))
  ("RET" duotone-hydra-save :exit t))

(provide 'duotone-hydra)
;;; duotone-hydra.el ends here
