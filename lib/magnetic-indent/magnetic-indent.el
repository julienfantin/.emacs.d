;;; magnetic-indent.el --- Auto-indentation advices  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Julien Fantin

;; Author: Julien Fantin <julien@Juliens-MacBook-Pro.local>
;; Keywords: convenience

(require 'puni-special)

;;; Code:

;;; The idea
;;; An indent func per mode

(defvar magnetic-indent-indent-functions
  '((emacs-lisp-mode . indent-sexp)
    (clojure-mode . magnetic-indent-clojure-indent)
    (clojurec-mode . magnetic-indent-clojure-indent)
    (clojurescript-mode . magnetic-indent-clojure-indent)))

(eval-after-load 'clojure-
  (defun magnetic-indent-clojure-indent ()
    (unwind-protect
        (when-let ((bounds (puni-bounds-of-sexp-around-point)))
          (clojure-indent-region (car bounds) (cdr bounds)))
      t)))

(defun magnetic-indent-indent-function ()
  "Return the indent function for the current major-mode."
  (alist-get major-mode magnetic-indent-indent-functions))

;;; Advise a list of function to run the indent function afterward
(defvar magnetic-indent-advised-commands
  '(kill-line
    newline
    newline-and-indent
    puni-backward-delete-char
    puni-backward-kill-line
    puni-backward-kill-word
    puni-barf-backward
    puni-barf-forward
    puni-forward-delete-char
    puni-kill-line
    puni-raise
    puni-slurp-backward
    puni-slurp-forward
    puni-splice
    puni-split
    puni-special-copy-sexp
    puni-special-close-sexp
    puni-special-open-sexp
    electric-newline-and-maybe-indent
    yank))

(defmacro without-goggles (&rest body)
  "Disable `goggles-delete' while executing `BODY'.
Useful for commands that delete whitespace during movement."
  (let ((goggles-on (make-symbol "goggles-on"))
        (ret (make-symbol "ret")))
    `(let ((,goggles-on (bound-and-true-p goggles-mode)))
       (when ,goggles-on (goggles-delete -1))
       (let ((,ret (progn ,@body)))
         (when ,goggles-on (goggles-delete))
         ,ret))))

(defun magnetic-indent--at-indentation-p ()
  "Return non-nil if the point is before indentation."
  (let ((before (point)))
    (save-excursion
      (beginning-of-line)
      (indent-according-to-mode)
      (eq before (point)))))

(defun magnetic-indent--in-comment-p ()
  "Return t if point is in or at the beginning a comment."
  (save-excursion
    (eq 'comment (syntax-ppss-context (syntax-ppss (+ (if (eolp) 0 1) (point)))))))


(defun magnetic-indent--at-syntax-p (syntax &optional p)
  (eq syntax (puni--syntax-char-after (or p (point)))))

(defun magnetic-indent-around-advice (&optional func &rest args)
  (let ((from-eol (eolp)))
    (call-interactively func args)
    (unless (magnetic-indent--in-comment-p)
      (without-goggles
       (progn
         (when (and from-eol
                    (not (eolp))
                    (not (magnetic-indent--at-syntax-p ?\)))
                    (not (magnetic-indent--at-indentation-p)))
           (just-one-space))
         (when-let ((indent-func (magnetic-indent-indent-function)))
           (save-excursion
             (unwind-protect
                 (progn
                   ;; (puni-up-list 'backward)
                   (puni-up-list 'backward)
                   (funcall indent-func))
               t))))))))

(defun magnetic-indent-toggle-advices ()
  (dolist (cmd magnetic-indent-advised-commands)
    (if (bound-and-true-p magnetic-indent-mode)
        (progn (message "advising %s" cmd)
               (advice-add cmd :around 'magnetic-indent-around-advice))
      (progn (message "unadvising %s" cmd)
             (advice-remove cmd 'magnetic-indent-around-advice)))))

;;;q###autoload
(define-minor-mode magnetic-indent-mode
  "Magnetic indentation"
  :init-value nil
  (magnetic-indent-toggle-advices))

(provide 'magnetic-indent)
;;; magnetic-indent.el ends here
