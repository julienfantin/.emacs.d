;;; puni-special --- Electrict indentation for puni  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'puni)

;;; Helpers

;;; TODO all movements should go to a special position
;;; - forward/backward sexp should mark the symbol at point, preserving mark direction

;;; TODO repl-interaction-mode
;; - Eval defun, sexp at point, sexp up to point

(defun puni-special-at-open-parenthesis-p (&optional p)
  "Return non-nil when point at an open parenthesis, according to the syntax table."
  (eq ?\( (puni--syntax-char-after (or p (point)))))

(defun puni-special-empty-sexp-p ()
  (save-excursion
    (let ((start (point)) left)
      (puni-backward-sexp)
      (setq left (point))
      (puni-forward-sexp 2)
      (= left start (point)))))

(defun puni-special-at-close-parenthesis-p (&optional look-ahead)
  "Return non-nil when point at a close parenthesis, according to the syntax table."
  (let ((p (point)))
    (eq ?\) (puni--syntax-char-after (if look-ahead p (- p 1))))))

(defun puni-special-at-or-before-indentation-p ()
  "Return non-nil if the point is between bol and where the line should indent."
  (let ((before (point))
        (indent)
        (bol))
    (save-excursion
      (beginning-of-line)
      (puni--indent-line)
      (setq indent (point))
      (beginning-of-line)
      (setq bol (point)))
    (<= bol before indent)))

(defun puni-special-p ()
  (interactive)
  (or (and (region-active-p) (not (bound-and-true-p iedit-mode)))
      (puni--begin-of-single-line-comment-p)
      (puni-special-at-open-parenthesis-p)
      (puni-special-at-close-parenthesis-p)))

(defun puni-special (func)
  (lambda ()
    (interactive)
    (if (not (puni-special-p))
        (puni-special--self-insert)
      ;; Don't drag the mark
      (when mark-active
        (deactivate-mark))
      (let ((from (point)))
        (call-interactively func)
        (let ((to (point)))
          ;; Try to mark if we jumped out of special
          (unless (puni-special-p)
            (if (puni-special-empty-sexp-p)
                (puni-mark-sexp-around-point)
              (puni-mark-sexp-at-point))
            (unless (< from to)
              (exchange-point-and-mark))))))))

;;; Movement

(defun puni-special-next-sexp ()
  "Move point to the beginning of the next sexp or comment."
  (interactive "^")
  (puni-forward-sexp)
  (puni-forward-sexp)
  (puni-backward-sexp)
  (when (puni--in-comment-p)
    (puni-special-next-sexp)))

(defun puni-special-prev-sexp ()
  "Move point to the beginning of the previous sexp or comment."
  (interactive "^")
  (puni-backward-sexp)
  (when (puni--in-comment-p)
    (puni-special-prev-sexp)))

(defun puni-special-up-list ()
  (interactive "^")
  (puni-up-list 'backward))

(defun puni-special--down-list (from)
  (unless (puni-special-at-open-parenthesis-p)
    (if (and (puni-strict-forward-sexp t)
             (puni--forward-blanks))
        (puni-special--down-list (point))
      (goto-char from))))

(defun puni-special-down-list ()
  (interactive "^")
  (let ((from (point)))
    (when (puni-special-at-open-parenthesis-p)
      (forward-char))
    (puni-special--down-list from)))

(defun puni-special-beginning-of-sexp ()
  "Redefinition of `puni-beginning-of-sexp' going to prev sexp instead of bob.

Go to the beginning of current sexp.
This means go to the point after the opening delimiter.  If this
is called from there, then go to the point before the delimiter,
so consecutive calling this can take you all the way across
opening delimiters."
  (interactive "^")
  (if (puni-special-at-close-parenthesis-p)
      (puni-backward-sexp)
    (let ((from (point)))
      (or (puni-beginning-of-list-around-point)
          (puni-up-list 'backward))
      (when (bobp)
        (goto-char from)
        (puni-backward-sexp)))))

(defun puni-special-end-of-sexp ()
  "Redefinition of `puni-special-end-of-sexp' that goes to next sexp instead of eob.

Go to the end of current sexp.
This means go to the point before the closing delimiter.  If this
is called from there, then go to the point after the delimiter,
so consecutive calling this can take you all the way across
closing delimiters."
  (interactive "^")
  ;; Move past sexp if we were at the begining
  (if (puni-special-at-open-parenthesis-p)
      (puni-forward-sexp)
    (let ((from (point)))
      (or (puni-end-of-list-around-point)
          (puni-up-list))
      (when (eobp)
        (goto-char from)
        (puni-forward-sexp)))))

;;; Fundamental edits

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

(defun puni-special--self-insert ()
  (setq this-command 'self-insert-command)
  (self-insert-command 1))

(defun puni-special-space ()
  (interactive)
  (let ((from-left (puni-special-at-open-parenthesis-p)))
    (puni-special--self-insert)
    (when from-left
      (backward-char))))

(defun puni-special-open-sexp ()
  (interactive)
  ;; TODO take symboles into account:
  ;; cond| => cond (|
  (let ((beg (puni-special-at-open-parenthesis-p))
        (end (puni-special-at-close-parenthesis-p)))
    (cond
     ((and beg (not end))
      (progn
        (insert " ")
        (backward-char)
        (puni-special--self-insert)
        (forward-char 2)
        (backward-char 2)))
     ((and end (not beg))
      (progn
        (insert " ")
        (puni-special--self-insert)))
     (t (puni-special--self-insert)))))

(defun puni-special-close-sexp ()
  (interactive "^")
  (when (and (puni-special-at-close-parenthesis-p 'look-ahead)
             (puni-special-at-or-before-indentation-p))
    (delete-indentation))
  (puni-special-end-of-sexp)
  (when (not (eolp))
    (delete-all-space)))

;;; Macro edits

(defun puni-special-drag-sexp-backward ()
  "Should be called at begining of sexp."
  (interactive "^")
  (puni-transpose)
  (puni-backward-sexp))

(defun puni-special-drag-sexp-forward ()
  "Should be called at begining of sexp."
  (interactive "^")
  (puni-forward-sexp)
  (puni-transpose)
  (puni-forward-sexp)
  (puni-backward-sexp))

;; TODO insert extra newline when copying top-level, maybe better
(defun puni-special-copy-sexp ()
  "At the beginning of sexp, copies the sexp forward and move to it.
Acts on the region when active. With C-u insert a newline before sexp."
  (interactive "^")
  (if mark-active
      (progn
        (copy-region-as-kill (region-beginning) (region-end))
        (exchange-point-and-mark)
        (newline)
        (puni--indent-line)
        (yank)
        (exchange-point-and-mark))
    (puni-mark-sexp-at-point)
    (kill-ring-save (region-beginning) (region-end))
    (newline)
    (puni--indent-line)
    (yank)
    (puni-backward-sexp)))

;;; Puni integration

(defvar puni-special-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Fundamental edits
    (define-key map (kbd "(") #'puni-special-open-sexp)
    (define-key map (kbd "[") #'puni-special-open-sexp)
    (define-key map (kbd "{") #'puni-special-open-sexp)
    (define-key map (kbd ")") #'puni-special-close-sexp)
    (define-key map (kbd "}") #'puni-special-close-sexp)
    (define-key map (kbd "]") #'puni-special-close-sexp)
    ;; Slurping & barfing
    (define-key map (kbd "C-)") #'puni-slurp-forward)
    (define-key map (kbd "C-(") #'puni-slurp-backward)
    (define-key map (kbd "C-M-)") #'puni-barf-forward)
    (define-key map (kbd "C-M-(") #'puni-barf-backward)
    (define-key map (kbd "M-RET") #'puni-expand-region)
    ;; Macro edits
    (define-key map (kbd "M-k") #'puni-kill-line)
    (define-key map (kbd "M-r") #'puni-raise)
    (define-key map (kbd "M-s") #'puni-splice)
    (define-key map (kbd "C-M-t") #'puni-transpose)
    (define-key map (kbd "C-M-c") #'puni-special-copy-sexp)
    (define-key map (kbd "C-M-<up>") #'puni-special-drag-sexp-backward)
    (define-key map (kbd "C-M-<down>") #'puni-special-drag-sexp-forward)
    ;; Movement
    (define-key map (kbd "C-M-a") #'puni-special-beginning-of-sexp)
    (define-key map (kbd "C-M-e") #'puni-special-end-of-sexp)
    (define-key map (kbd "C-M-u") #'puni-special-up-list)
    (define-key map (kbd "C-M-d") #'puni-special-down-list)
    (define-key map (kbd "C-M-n") #'puni-special-next-sexp)
    (define-key map (kbd "C-M-p") #'puni-special-prev-sexp)
    ;; Special
    (define-key map (kbd "SPC") (puni-special #'puni-special-space))
    (define-key map (kbd "k") (puni-special #'puni-kill-line))
    (define-key map (kbd "m") (puni-special #'puni-expand-region))
    (define-key map (kbd "t") (puni-special #'puni-transpose))
    (define-key map (kbd "c") (puni-special #'puni-special-copy-sexp))
    (define-key map (kbd "s") (puni-special #'puni-splice))
    (define-key map (kbd "r") (puni-special #'puni-raise))
    (define-key map (kbd "f") (puni-special #'puni-syntactic-forward-punct))
    (define-key map (kbd "b") (puni-special #'puni-syntactic-backward-punct))
    (define-key map (kbd "a") (puni-special #'puni-special-beginning-of-sexp))
    (define-key map (kbd "e") (puni-special #'puni-special-end-of-sexp))
    (define-key map (kbd "u") (puni-special #'puni-special-up-list))
    (define-key map (kbd "d") (puni-special #'puni-special-down-list))
    (define-key map (kbd "n") (puni-special #'puni-special-next-sexp))
    (define-key map (kbd "p") (puni-special #'puni-special-prev-sexp))
    map))

;;;###autoload
(define-minor-mode puni-special-mode
  "Electric indentation and lispy-style special bindings for puni."
  :keymap puni-special-mode-map
  (puni-mode puni-special-mode))

(provide 'puni-special)
;;; puni-special.el ends here
