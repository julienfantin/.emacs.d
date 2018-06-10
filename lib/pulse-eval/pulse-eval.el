;;; pulse-eval.el --- Pulse interactively evaluated sexps
;;; Commentary:
;;
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'pulse)
(require 'thingatpt)


;; * Customs

(defcustom pulse-eval-overlay-priority 100
  "Priority of the pulse eval overlay."
  :group 'pulse-eval
  :type 'number)

(defcustom pulse-eval-iterations pulse-iterations
  "Priority of the pulse eval overlay."
  :group 'pulse-eval
  :type 'number)

(defcustom pulse-eval-delay pulse-delay
  "Priority of the pulse eval overlay."
  :group 'pulse-eval
  :type 'number)

(defcustom pulse-eval-advices-alist
  '((emacs-lisp-mode
     . ((eval-buffer                      . pulse-eval-highlight-buffer-advice)
        (eval-region                      . pulse-eval-highlight-region-advice)
        (eval-last-sexp                   . pulse-eval-highlight-sexp-advice)
        (eval-print-last-sexp             . pulse-eval-highlight-sexp-advice)
        (eval-defun                       . pulse-eval-highlight-defun-advice)))
    (cider-mode
     . ((cider-load-buffer                . pulse-eval-highlight-buffer-advice)
        (cider-eval-region                . pulse-eval-highlight-region-advice)
        (cider-eval-last-sexp             . pulse-eval-highlight-sexp-advice)
        (cider-pprint-eval-last-sexp      . pulse-eval-highlight-sexp-advice)
        (cider-eval-defun-at-point        . pulse-eval-highlight-defun-advice)
        (cider-pprint-eval-defun-at-point . pulse-eval-highlight-defun-advice))))
  "Advices alist, (mode . (function . around-advice))"
  :type 'alist
  :group 'pulse-eval)

(defface pulse-eval-face
  '((t (:inherit pulse-highlight-start-face)))
  "Face used for pulsing interactively evaluated code"
  :group 'pulse-eval)

(defface pulse-eval-error-face
  '((t (:inherit error)))
  "Face used for pulsing interactively evaluated code"
  :group 'pulse-eval)

(defun pulse-eval--adjust-overlay-priority-advice (overlay &optional _)
  "Advise 'pulse-momentary-highlight-overlay' to change 'OVERLAY' priority.

By setting a higher priority we prevent visually clashes with
other overlay-based modes such as flycheck."
  (overlay-put overlay 'priority pulse-eval-overlay-priority))


;; * Pulse hooks

(defvar pusle-eval-before-pulse-hook nil
  "Hook ran before pulse.")

(defvar pusle-eval-after-pulse-hook nil
  "Hook ran after pulse has finished iterating.")


;; * Thingatpt helpers

(defun pulse-eval--bounds-of-sexp ()
  (or
   (bounds-of-thing-at-point 'sexp)
   (save-excursion
     (backward-sexp)
     (bounds-of-thing-at-point 'sexp))))


;; * Pulse advices

(defun pulse-eval--highlight-advice (start end fn args)
  "Pulse between 'START' and 'END' before invoking 'FN' with 'ARGS'.

In case fn signals an error, pulse region with
'pulse-eval-error-face' before signaling the error."
  (let ((pulse-iterations pulse-eval-iterations)
        (pulse-delay pulse-eval-delay))
    (run-hooks 'pulse-eval-before-pulse-hook)
    (pulse-momentary-highlight-region start end 'pulse-eval-face)
    (condition-case e
        (apply fn args)
      (error
       (pulse-momentary-highlight-region start end 'pulse-eval-error-face)
       (signal (car e) (cdr e))))
    (run-with-timer (* pulse-iterations pulse-delay) nil #'(lambda () (run-hooks 'pulse-eval-after-pulse-hook)))))

(defun pulse-eval--momentary-highlight-sexp* (fn args)
  (save-excursion
    (let ((bounds (pulse-eval--bounds-of-sexp)))
      (pulse-eval--highlight-advice (car bounds) (cdr bounds) fn args))))

(defun pulse-eval-highlight-sexp-advice (fn &rest args)
  "Sexp highlighting."
  (if-let (bounds (bounds-of-thing-at-point 'sexp))
      (pulse-eval--highlight-advice (car bounds) (cdr bounds) fn args)
    (pulse-eval--momentary-highlight-sexp* fn args)))

(defun pulse-eval-highlight-forward-sexp-advice (fn &rest args)
  "Forward Sexp highlighting."
  (save-excursion
    (forward-sexp)
    (apply 'pulse-eval-highlight-sexp-advice fn args)))

(defun pulse-eval-highlight-defun-advice (fn &rest args)
  "Defun highlighting."
  (if-let (bounds (bounds-of-thing-at-point 'defun))
      (pulse-eval--highlight-advice (car bounds) (cdr bounds) fn args)
    (apply #'pulse-eval-highlight-sexp-advice fn args)))

(defun pulse-eval-highlight-region-advice (fn &rest args)
  "Pulse region between 'START' and 'END' points."
  (pulse-eval--highlight-advice
   (or (nth 0 args) (region-beginning))
   (or (nth 1 args) (region-end))
   fn args))

(defun pulse-eval-highlight-buffer-advice (fn &rest args)
  "Pulse 'BUFFER' or 'current-buffer'."
  (with-current-buffer (or (and (listp args) (nth 0 args)) (current-buffer))
    (pulse-eval--highlight-advice (window-start) (window-end) fn args)))


;; * Internal advices

(defun pulse-eval--major-advices ()
  "Return a list of advices for the current 'major-mode'."
  (alist-get major-mode pulse-eval-advices-alist))

(defun pulse-eval--minor-advices ()
  "Return a list of advices for the currently enabled minor modes."
  (let ((minor-modes (cl-remove-if-not
                      (lambda (mode)
                        (and (boundp mode) (symbol-value mode)))
                      minor-mode-list)))
    (cl-reduce
     (lambda (acc mode-bindings)
       (let ((minor-mode (car mode-bindings))
             (bindings (cdr mode-bindings)))
         (if (member minor-mode minor-modes)
             (append acc bindings)
           acc)))
     pulse-eval-advices-alist
     :initial-value nil)))

(defun pulse-eval-all-advices ()
  "Return all applicable advices."
  (append
   (pulse-eval--major-advices)
   (pulse-eval--minor-advices)))

(defun pulse-eval--add-advices ()
  "Add all applicable advices."
  (advice-add #'pulse-momentary-highlight-overlay :before #'pulse-eval--adjust-overlay-priority-advice)
  (dolist (binding (pulse-eval-all-advices))
    (let ((sym (car binding))
          (fn (cdr binding)))
      (advice-add sym :around fn))))

(defun pulse-eval--remove-advices ()
  "Remove all applicable advices."
  (advice-remove #'pulse-momentary-highlight-overlay #'pulse-eval--adjust-overlay-priority-advice)
  (dolist (binding (pulse-eval-all-advices))
    (let ((sym (car binding))
          (fn (cdr binding)))
      (advice-remove sym fn))))


;; * Advices registration

(defun pulse-eval-register-advice (mode fn pulse-advice)
  "Register 'MODE' to advise 'FN' with 'PULSE-ADVICE'."
  (if-let ((lst (alist-get mode pulse-eval-advices-alist))
           (elt (cons fn pulse-advice)))
      (unless (member elt lst)
        (map-put pulse-eval-advices-alist mode (push elt lst)))
    (add-to-list 'pulse-eval-advices-alist (cons mode (list (cons fn pulse-advice))))))

(defun pulse-eval-unregister-advice (mode fn pulse-advice)
  "Register 'MODE' to stop advising 'FN' with 'PULSE-ADVICE'."
  (when-let ((lst (alist-get mode pulse-eval-advices-alist))
             (elt (cons fn pulse-advice)))
    (when (member elt lst)
      (map-put pulse-eval-advices-alist mode (remove elt lst)))))


;; * Minor mode

;;;###autoload
(define-minor-mode pulse-eval-mode
  "Pulse the current sexp, defun or buffer when interactively
  evaluated."
  nil nil nil
  (if pulse-eval-mode (pulse-eval--add-advices) (pulse-eval--remove-advices)))

(provide 'pulse-eval)
;;; pulse-eval.el ends here
