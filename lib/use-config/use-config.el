;;; use-config.el --- use-config declaration to simplify use-package
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'subr-x)
(require 'config-path)
(require 'package)
(require 'use-package)


;; * Config helpers

(defmacro comment (&rest body) "Ignore 'BODY'." nil)

(defun after-init (&rest syms)
  "When called during init, add 'SYMS' to 'after-init-hook' otherwise call 'SYMS'."
  (when-let (sym (car syms))
    (if after-init-time
        (funcall sym)
      (add-hook 'after-init-hook sym))
    (apply 'after-init (cdr syms))))


;; * Eval after load

;; NOTE: copied from https://github.com/tarao/with-eval-after-load-feature-el

(eval-when-compile (require 'cl))
(eval '(eval-when-compile (require 'cl)))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    ;; This definition is a bit different from that in Emacs 24.4; An
    ;; extra `funcall' is needed for an older Emacs.
    `(eval-after-load ,file `(funcall (function ,(lambda () ,@body))))))

(defun with-eval-after-load-feature-preload-1 (feature)
  (let ((after-load-alist nil))
    (unless (or (and (stringp feature)
                     (load feature :no-message :no-error))
                (and (symbolp feature)
                     (require feature nil :no-error)))
      (message "Cannot find %s" feature)
      'fail)))

(defun with-eval-after-load-feature-preload (feature-list)
  (loop for f in feature-list
        for fail = (with-eval-after-load-feature-preload-1 f)
        when fail
        collect fail))

(defun with-eval-after-load-feature-transform (feature-list body)
  (if (null feature-list)
      body
    (let ((feature (car feature-list)) (rest (cdr feature-list)))
      `((with-eval-after-load ',feature
          ,@(with-eval-after-load-feature-transform rest body))))))

;;;###autoload
(defmacro after (feature &rest body)
  (declare (indent 1) (debug t))
  (let* ((feature (if (and (listp feature) (eq (car-safe feature) 'quote))
                      (cdr feature) feature))
         (fs (if (listp feature) feature (list feature)))
         (form (or (and (eval '(eval-when (compile)
                                 (with-eval-after-load-feature-preload fs)))
                        'with-no-warnings)
                   'progn)))
    `(,form ,@(with-eval-after-load-feature-transform fs body))))


;; * Customs

(defvar use-config-load-path
  (expand-file-name "config/" user-emacs-directory))


;; * Package interop

;; Fixes auto-async-byte-compilation issues for config files
(unless package--initialized
  (package-initialize))


;; * Internal functions

(defvar use-config-keywords '(:disabled))

;;;###autoload
(defun use-config-disabled-p (plist)
  "Return t when 'PLIST' :disabled keyword is t."
  (let ((disabled (plist-get plist :disabled)))
    (and (booleanp disabled) disabled)))

;; ** use-package interop

(defun use-config--package-pre-init-hook (package)
  "Return a symbol for the 'PACKAGE' injected pre-init hook."
  (intern (concat "user-package--" (symbol-name package) "--pre-init-hook")))

(defun use-config--filter-package-plist (plist)
  "Filter 'PLIST' and return the keywords usable in a 'use-package' definition."
  (cl-reduce (lambda (acc kw)
               (let ((value (plist-get plist kw)))
                 (if (or (null value) (member kw use-config-keywords))
                     acc
                   (plist-put acc kw value))))
             use-package-keywords
             :initial-value '()))

(defun use-config--disable-packages (plist)
  "Ignore all packages in 'PLIST' :disabled keyword."
  (dolist (package (plist-get plist :disabled))
    (add-hook (use-config--package-pre-init-hook package) 'ignore)))


;; * API

;;;###autoload
(defmacro use-config (name &rest plist)
  "Use config 'NAME' passing 'PLIST' to 'use-package'.
If :disabled is t the config is ignored, if a list of package
names, will use the injected 'use-package' hooks to prevent
loading the pacakges."
  (declare (indent 1))
  (unless (use-config-disabled-p plist)
    (use-config--disable-packages plist)
    (let ((use-package-inject-hooks t)
          (use-package-plist (use-config--filter-package-plist plist)))
      `(use-package ,name
         :demand t
         ,@use-package-plist))))


;; * Convenience
;; ** Path

(defun use-config-update-path ()
  "Add 'use-config-load-path' to 'load-path'."
  (let ((default-directory use-config-load-path))
    (normal-top-level-add-subdirs-to-load-path)))

(use-config-update-path)

;; ** Font-lock

(defconst use-config-font-lock-keywords
  '(("(\\(use-config\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode use-config-font-lock-keywords)

(put 'use-config 'lisp-indent-function 'defun)


;; ** Required packages

(use-package validate :ensure t :demand t)

(provide 'use-config)
;;; use-config.el ends here
