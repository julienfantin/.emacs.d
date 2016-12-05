;;; lisp-minor-mode.el --- Lisp minor mode -*- lexical-binding: t ; -*-
;;; Commentary:
;;
;;; Code:
(require 'prog-mode)
(require 'subword)
(require 'org)


;; * Customs

(defvar lisp-minor-mode-modes-list
  '((elisp-mode        . emacs-lisp-mode)
    (ielm              . ielm-mode)
    (lisp-mode         . lisp-mode)
    (lisp-mode         . common-lisp-mode)
    (scheme            . scheme-mode)
    (cider             . cider-repl-mode)
    (clojure-mode      . clojure-mode)
    (clojure-mode      . clojurec-mode)
    (clojure-mode      . clojurescript-mode)
    (lfe-mode          . lfe-mode)
    (inferior-lfe-mode . inferior-lfe))
  "Alist mapping the name of an elisp file  file to mode symbol.")

(defcustom lisp-minor-mode-prettify-symbols-alist
  '((t
     . (("<=" . (?· (Br . Bl) ?≤))
        (">=" . (?· (Br . Bl) ?≥))))
    ((cider-repl-mode
      clojure-mode
      clojurec-mode
      clojurescript-mode)
     . (("->"  . (?- (Br . Bc) ?- (Br . Bc) ?>))
        ("->>" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                      (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                      (Bc . Bl) ?- (Br . Br) ?>))
        ("fn"  . (?· (Br . Bl) ?λ)))))
  "Alist of major mode (or list of minor or major modes) to alist of prettify symbol mappings."
  :type 'alist
  :group 'lisp-minor-mode)

(defcustom lisp-minor-mode-mini-buffer nil
  "Enable 'lisp-minor-mode' in mini-buffer when it makes sense."
  :type 'boolean
  :group 'lisp-minor-mode)

(defcustom lisp-minor-mode-prettify t
  "Enable 'prettify-symbols-mode' in Lisp modes."
  :type 'boolean
  :group 'lisp-minor-mode)

(defcustom lisp-minor-mode-subword t
  "Enable 'subword-mode' in Lisp modes."
  :type 'boolean
  :group 'lisp-minor-mode)


;; * Helpers

(defun lisp-minor-mode-symbol-to-mode-hook (symbol)
  "Make a hook from 'SYMBOL'."
  (intern (format "%s-hook" (symbol-name symbol))))

(defun lisp-minor-mode-current-modes ()
  "Return current major or minor modes that qualify as a Lisp."
  (let ((minor-modes (cl-remove-if-not
                      (lambda (mode)
                        (and (boundp mode) (symbol-value mode)))
                      minor-mode-list)))
    (cl-reduce
     (lambda (acc mode)
       (if (or (equal mode major-mode)
               (member mode minor-modes))
           (push mode acc)
         acc))
     lisp-minor-mode-modes-list
     :initial-value '()
     :key #'cdr)))

(defun lisp-minor-mode-alist-get (key alist)
  "Like 'alist-get' but 'KEY' will be tested with 'member' when 'ALIST' keys are lists."
  (cl-some
   (lambda (binding)
     (let ((mode (car binding))
           (lst  (cdr binding)))
       (cl-typecase mode
         (list (when (member key (car binding)) lst))
         (symbol (when (eq mode key) lst)))))
   alist))

(defun lisp-minor-mode-mini-buffer-turn-on ()
  "Turn on 'lisp-minor-mode' in mini-buffer."
  (when (eq this-command 'eval-expression)
    (lisp-minor-mode)))


;; * Prettify

(defun lisp-minor-mode-prettify-turn-on ()
  "Turn on 'prettify-symbols-mode' for Lisp."
  (let ((alist nil))
    (dolist (binding (alist-get t lisp-minor-mode-prettify-symbols-alist))
      (push binding alist))
    (dolist (mode (lisp-minor-mode-current-modes))
      (dolist (binding (lisp-minor-mode-alist-get mode lisp-minor-mode-prettify-symbols-alist))
        (push binding alist)))
    (set (make-local-variable 'prettify-symbols-alist) alist))
  (prettify-symbols-mode 1))

(defun lisp-minor-mode-prettify-turn-off ()
  "Turn off 'prettify-symbols-mode' for Lisp."
  (dolist (binding (alist-get t lisp-minor-mode-prettify-symbols-alist))
    (delete binding prettify-symbols-alist))
  (dolist (mode (lisp-minor-mode-current-modes))
    (dolist (binding (lisp-minor-mode-alist-get mode lisp-minor-mode-prettify-symbols-alist))
      (delete binding prettify-symbols-alist))))


;; * Setup

(defun lisp-global-minor-mode-install-hooks ()
  "Install 'lisp-minor-mode' hooks for Lisp modes in 'lisp-minor-mode-modes-list'."
  (when lisp-minor-mode-mini-buffer
    (add-hook 'minibuffer-setup-hook #'lisp-minor-mode-mini-buffer-turn-on))
  (dolist (binding lisp-minor-mode-modes-list)
    (let ((file (car binding))
          (hook (lisp-minor-mode-symbol-to-mode-hook (cdr binding))))
      (with-eval-after-load file
        (add-hook hook #'lisp-minor-mode)))))

(defun lisp-global-minor-mode-remove-hooks ()
  "Remove 'lisp-minor-mode' hooks for Lisp modes in 'lisp-minor-mode-modes-list'."
  (when lisp-minor-mode-mini-buffer
    (remove-hook 'minibuffer-setup-hook #'lisp-minor-mode-mini-buffer-turn-on))
  (dolist (binding lisp-minor-mode-modes-list)
    (let ((file (car binding))
          (hook (lisp-minor-mode-symbol-to-mode-hook (cdr binding))))
      (with-eval-after-load file
        (remove-hook hook #'lisp-minor-mode)))))


;; * Minor modes
;; ** Global

;;;###autoload
(define-minor-mode lisp-global-minor-mode
  "Global minor mode for all thing lisp"
  nil nil nil
  :global t
  :keymap nil
  (if lisp-global-minor-mode
      (lisp-global-minor-mode-install-hooks)
    (lisp-global-minor-mode-remove-hooks)))

;; ** Local

(defvar lisp-minor-mode-map (make-sparse-keymap))

(defun lisp-minor-mode-turn-on ()
  "Activation function."
  (when lisp-minor-mode-prettify (lisp-minor-mode-prettify-turn-on))
  (when lisp-minor-mode-subword (subword-mode 1)))

(defun lisp-minor-mode-turn-off ()
  "Deactivation function."
  (when lisp-minor-mode-prettify (lisp-minor-mode-prettify-turn-off))
  (when lisp-minor-mode-subword (subword-mode -1)))

;;;###autoload
(define-minor-mode lisp-minor-mode
  "Minor mode for all things lisp"
  nil " λ" nil
  :global nil
  :keymap lisp-minor-mode-map
  (if lisp-minor-mode
      (lisp-minor-mode-turn-on)
    (lisp-minor-mode-turn-off)))

(provide 'lisp-minor-mode)
;;; lisp-minor-mode.el ends here
