;;; init.el --- Config bootstrap
;;; Commentary:
;;
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)


;; * Config options

(defvar config-completion-system 'ivy)


;; * Bootstrap

(require 'cl-lib)
(require 'cl-macs)
(require 'package)

;; ** Path

;; Make sure the 'user-emacs-directory' is properly set, a lot of our path
;; definitions rely on this
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

(defun -reload-init ()
  "Reload the init file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(eval-and-compile
  ;; Setup various paths before we do anything, use-config depends on this
  (load-file (expand-file-name "config/config-path.el" user-emacs-directory))
  ;; Setup elpa
  (load-file (expand-file-name "config/config-elpa.el" user-emacs-directory))
  ;; Manually load use-config, it also provide some useful helpers
  (load-file (expand-file-name "lib/use-config/use-config.el" user-emacs-directory)))

(require 'use-package)

;; Setup our env path here, some configs might need this to be set
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))


;; * Configs

;; Load order notes:
;; - Demand use-config and config-path
;; - Load keybindings last

(use-package use-config :demand t)
(use-package private :demand t)

(use-config config-path :demand t)

(use-config config-browser)
(use-config config-buffers)
(use-config config-completion)
(use-config config-debug)
(use-config config-defaults)
(use-config config-doc)
(use-config config-editing)
(use-config config-elpa)
(use-config config-files)
(use-config config-font-lock)
(use-config config-frame :if window-system)
(use-config config-git)
(use-config config-gui)
(use-config config-helm :if (equal 'helm config-completion-system))
(use-config config-help)
(use-config config-indentation)
(use-config config-irc)
(use-config config-ivy :if (equal 'ivy config-completion-system))
(use-config config-layouts)
(use-config config-marks)
(use-config config-org)
(use-config config-outlines)
(use-config config-parsers)
(use-config config-persistence)
(use-config config-prog-mode)
(use-config config-project)
(use-config config-prose)
(use-config config-scratch)
(use-config config-search)
(use-config config-sexp)
(use-config config-shell)
(use-config config-theme)
(use-config config-windows)

;; ** Langs

(use-config config-clojure)
(use-config config-emacs-lisp)
(use-config config-nix)
(use-config config-ocaml)
(use-config config-sql)
(use-config config-web)

;; ** Keybindings

(use-config config-keybindings)

(provide 'init)
;;; init.el ends here
