;;; puni-special-avy.el --- Avy integration for puni-special  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Julien Fantin

;; Author: Julien Fantin <julien@Juliens-MacBook-Pro.local>
;; Keywords: lisp, convenience, matching, matching, matching,

(require 'puni-special)
(require 'avy)

;;; Code:

(defun avy--candidate-sexp-at-point (candidate)
  "Transform an avy `CANDIDATE' to the sexp at point."
  (save-excursion
    (goto-char (caar candidate))
    (setf (car candidate) (puni-bounds-of-sexp-at-point))
    candidate))

(defun avy-goto-sexp-at-point-timer (&optional arg)
  "Like avy-goto-timer with candidates transformed by `sexp-at-point'.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg (not avy-all-windows) avy-all-windows)))
    (avy-with avy-goto-sexp-at-point-timer
      (setq avy--old-cands
            (mapcar 'avy--candidate-sexp-at-point (avy--read-candidates)))
      (avy-process avy--old-cands))))

(defun avy--candidate-sexp-around-point (candidate)
  "Transform an avy `CANDIDATE' to the sexp around point."
  (save-excursion
    (goto-char (caar candidate))
    (setf (car candidate) (puni-bounds-of-sexp-around-point))
    candidate))

(defun avy-goto-sexp-around-point-timer (&optional arg)
  "Like avy-goto-timer with candidates transformed by `sexp-around-point'.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg (not avy-all-windows) avy-all-windows)))
    (avy-with avy-goto-sexp-around-point-timer
      (setq avy--old-cands
            (mapcar 'avy--candidate-sexp-around-point (avy--read-candidates)))
      (avy-process avy--old-cands))))

(defun puni-special-avy--advice-after-mark-sexp (&rest _) (mark-sexp))

(advice-add 'avy-goto-sexp-at-point-timer :after #'puni-special-avy--advice-after-mark-sexp)
(advice-add 'avy-goto-sexp-around-point-timer :after #'puni-special-avy--advice-after-mark-sexp)


(let ((map puni-special-mode-map))
  (define-key map (kbd "C-c j") #'avy-goto-sexp-at-point-timer)
  (define-key map (kbd "C-c h") #'avy-goto-sexp-around-point-timer)
  (define-key map (kbd "j") (puni-special #'avy-goto-sexp-at-point-timer))
  (define-key map (kbd "h") (puni-special #'avy-goto-sexp-around-point-timer)))

(provide 'puni-special-avy)
;;; puni-special-avy.el ends here.
