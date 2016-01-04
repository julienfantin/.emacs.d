;;; emacs-swagger-kit
;;; Commentary:
;; Code:
;; * TODOs


;; * Feature flags

(defvar esk-emacs-lisp t)
(defvar esk-clojure t)
(defvar esk-ocaml t)
(defvar esk-org t)
(defvar esk-notes t)
(defvar esk-irc t)
(defvar esk-shell t)
(defvar esk-sql t)
(defvar esk-gui t)
(defvar esk-spell-check t)
(defvar esk-private t)


;; * Initialization

(require 'cl-lib)
(require 'cl-macs)
(require 'package)
(require 'diminish nil t)
(require 'bind-key nil t)

(defmacro after (mode &rest body)
  "`eval-after-load' `MODE', wrapping `BODY' in `progn'."
  (declare (indent defun))
  (let ((s (if (symbolp mode)
               (symbol-name mode)
             mode)))
    `(eval-after-load ,(symbol-name mode)
       (quote (progn ,@body)))))

(defun turn-on    (f) `(lambda () (funcall #',f +1)))
(defun turn-off   (f) `(lambda () (funcall #',f -1)))
(defun after-init (f) (if after-init-time (funcall f) (add-hook 'after-init-hook f)))
(defmacro comment (&rest body))

(defun esk-debug-init ()
  (toggle-debug-on-error)
  (after-init #'toggle-debug-on-error))

;; ** Path

(defvar homebrew-site-lisp "/usr/local/share/emacs/site-lisp/")

(defun esk-setup-path ()
  (cond
   ((eq system-type 'darwin)
    (let ((default-directory homebrew-site-lisp))
      (normal-top-level-add-subdirs-to-load-path)))))

(progn
  (esk-debug-init)
  (esk-setup-path))

;; ** ELPA

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(cljr-helm . "melpa-stable") t)

(package-initialize)

;; ** use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; ** TLS
;; Note:
;; python -m pip install --user certifi

(defvar esk-pip-tls-trustfile
  (replace-regexp-in-string
   "\\\\" "/"
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "python -m certifi"))))

(defvar esk-pip-tls-program
  (list
   (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
           (if (eq window-system 'w32) ".exe" "") esk-pip-tls-trustfile)))

(use-package tls
  :config
  (setq tls-checktrust t
        tls-program esk-pip-tls-program))

(use-package gnutls
  :config
  (setq gnutls-verify-error t
        gnutls-trustfiles (list esk-pip-tls-trustfile)))

(comment
 ;; Testing
 (if (condition-case e
         (progn
           (url-retrieve "https://wrong.host.badssl.com/"
                         (lambda (retrieved) t))
           (url-retrieve "https://self-signed.badssl.com/"
                         (lambda (retrieved) t))
           t)
       ('error nil))
     (error "tls misconfigured")
   (url-retrieve "https://badssl.com" (lambda (retrieved) t))))

(use-package diminish :ensure t :demand t)

(use-package paradox
  :ensure t
  :defer t
  :commands paradox-enable
  :init (after-init #'paradox-enable)
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package private
  :if esk-private
  :demand t
  :load-path "./lib")

;; * Emacs defaults

(setq-default gc-cons-threshold 200000000)
(setq-default cursor-type 'bar)
(setq initial-scratch-message ""
      inhibit-startup-message t
      scroll-margin 5
      scroll-conservatively 10000
      cursor-in-non-selected-windows nil
      echo-keystrokes 0.
      ring-bell-function nil
      visible-bell t)

(use-package mwheel
  :config
  (setq mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(0.02 ((shift) . 2) ((control) . nil))))

(fset 'yes-or-no-p 'y-or-n-p)

;; ** Paths

(defun temp-file (name)
  "Return a temporary file with `NAME'."
  (expand-file-name name temporary-file-directory))

(defun user-file (name)
  "Return a file with `NAME' in `user-emacs-directory'."
  (expand-file-name name user-emacs-directory))

(defvar user-var-directory (expand-file-name "var/" user-emacs-directory))

(defun user-var-file (name)
  (expand-file-name name user-var-directory))

(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name))
      custom-file (user-file "custom.el"))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; * Windows management
;; ** Perspectives

(use-package simple-screen
  :ensure t
  :defer t
  :commands
  (simple-screen-0
   simple-screen-1
   simple-screen-2
   simple-screen-3
   simple-screen-4))

(use-package perspective
  :ensure t
  :defer t
  :commands persp-mode
  :config (setq persp-show-modestring t))

;; ** Windows

(use-package winner
  :defer t
  :init (after-init #'winner-mode))

(use-package zygospore :ensure t :defer t)

(use-package ace-window
  :ensure t
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
        aw-scope 'frame))

;; * Looks
;; ** Fonts

(defun font-exists-p (font)
  (if (null (x-list-fonts font)) nil t))

(defvar esk-fonts
  '("-*-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
    "-*-M+ 1mn-normal-normal-normal-*-11-*-*-*-p-0-iso10646-1"
    "-*-Fira Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"))

(defvar esk-font
  (cl-find-if #'font-exists-p esk-fonts))

(defun esk-text-scale-increase ()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun esk-text-scale-decrease ()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

(use-package frame
  :if (and window-system esk-font)
  :init
  (progn
    (mac-auto-operator-composition-mode)
    (set-frame-font esk-font)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (setq use-file-dialog nil
          use-dialog-box nil)))

;; ** GUI

(use-package spacemacs-theme
  :if esk-gui
  :ensure t
  :defer t
  :init (after-init (lambda () (load-theme 'spacemacs-light t nil)))
  :config
  (use-package spacemacs-common
    :config
    (setq spacemacs-theme-org-highlight nil
          spacemacs-theme-org-height nil)))

(use-package spaceline
  :ensure t
  :defer t
  :config
  (advice-add
   'load-theme :after
   (lambda (theme &optional no-confirm no-enable)
     (powerline-reset))))

(use-package spaceline-config
  :demand t
  :commands (spaceline-emacs-theme)
  :init
  (progn
    (spaceline-emacs-theme)
    (spaceline-toggle-minor-modes-off)
    (spaceline-helm-mode 1)))

(use-package fringe
  :init (fringe-mode 4)
  :config
  (progn
    (setq indicate-empty-lines t
          indicate-buffer-boundaries t
          indicate-unused-lines t)
    (setf (cdr (assq 'continuation fringe-indicator-alist))
          '(nil right-curly-arrow))))

(use-package beacon
  :ensure t
  :init (after-init (turn-on #'beacon-mode))
  :preface
  (defun esk-beacon-set-color ()
    (setq beacon-color (face-attribute 'font-lock-builtin-face :foreground)))
  :config
  (progn
    (setq beacon-blink-delay 0.1
          beacon-blink-duration 0.2
          beacon-size 50
          ;; doesn't mesh well with smooth scroll
          beacon-blink-when-window-scrolls nil)
    (advice-add
     'load-theme :after
     (lambda (theme &optional no-confirm no-enable)
       (esk-beacon-set-color)))))

;; ** UI

(use-package hl-line
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook (turn-on #'hl-line-mode))
    (add-hook 'dired-mode-hook (turn-on #'hl-line-mode))
    (after magit (add-hook 'magit-mode-hook (turn-on #'hl-line-mode))))
  :config
  (setq global-hl-line-sticky-flag nil
        hl-line-sticky-flag nil))

(use-package linum
  :disabled t
  :defer t
  :init (add-hook 'prog-mode-hook (turn-on #'linum-mode))
  :defines (esk-linum-current-timer)
  :functions (linum-update-current)
  :preface (defvar-local esk-linum-current-timer nil)
  :config
  (progn
    (setq linum-delay t)
    (setq linum-format " %4d ")
    ;; Great hack to keep linum from slowing down scrolling
    (defun linum-schedule ()
      (when (timerp esk-linum-current-timer)
        (cancel-timer esk-linum-current-timer))
      (setq esk-linum-current-timer
            (run-with-idle-timer 1 nil #'linum-update-current)))))

(defvar esk-default-window-margin 3)

(defun esk-set-window-margins ()
  (set-window-margins
   (car (get-buffer-window-list (current-buffer) nil t))
   esk-default-window-margin
   esk-default-window-margin))

(add-hook 'window-configuration-change-hook #'esk-set-window-margins)

(use-package highlight-numbers
  :ensure t
  :defer t
  :commands highlight-numbers-mode
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; ** Theme

(use-package custom
  :defer t
  :config
  (setq custom-theme-directory
        (expand-file-name "themes" user-emacs-directory)))

(comment
 ;; light
 (use-package plan9-theme :ensure t)
 (use-package spacemacs-theme :ensure t)
 ;; dark
 (use-package darktooth-theme :ensure t)
 (use-package zerodark-theme :ensure t)
 (use-package gruvbox-theme :ensure t))

(use-package helm-themes :ensure t :defer t)

(use-package rainbow-mode
  :ensure t
  :defer t
  :commands rainbow-mode
  :diminish rainbow-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (after web-mode (add-hook 'web-mode-hook 'rainbow-mode))))

;; ** Dired

(use-package dired
  :defer t
  :config
  (progn
    (setq dired-auto-revert-buffer t)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

;; ** Minibuffer completion

(use-package psession
  :ensure t
  :defer t
  :init (after-init #'psession-mode)
  :config
  (setq psession-elisp-objects-default-directory (user-var-file "elisp-objects/")))

;; ** Helm
;; *** Core

(use-package helm
  :ensure t
  :functions (helm-autoresize-mode)
  :commands (helm-mini helm-find-files)
  :preface (defvar helm-command-prefix-key "C-c h")
  :config
  (progn
    ;; Force vertical split at bottom
    (setq helm-split-window-in-side-p t
          helm-split-window-default-side 'above
          helm-autoresize-min-height 30
          helm-autoresize-max-height 30
          helm-echo-input-in-header-line t
          helm-candidate-number-limit 300)
    (helm-autoresize-mode 1)
    ;; Remap persistent action to TAB
    (bind-keys :map helm-map
               ("C-z" . helm-select-action)
               ("<tab>" . helm-execute-persistent-action)
               ("C-i" . helm-execute-persistent-action))
    ;; Window setup
    (setq helm-buffers-fuzzy-matching t
          helm-imenu-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-locate-fuzzy-match nil
          helm-M-x-fuzzy-match t
          helm-semantic-fuzzy-match t)))

(use-package helm-config :demand t)
(use-package helm-adaptive
  :init (after helm (helm-adaptive-mode 1))
  :config
  (setq helm-adaptive-history-file (temp-file "helm-adaptive-history")))
(use-package helm-command
  :commands (helm-M-x)
  :config (setq helm-M-x-always-save-history t))
(use-package helm-mode
  :demand t
  :diminish helm-mode
  :init (after-init #'helm-mode)
  :config
  (progn
    (setq helm-idle-delay 0.01
          helm-input-idle-delay 0.01
          helm-ff-transformer-show-only-basename t
          helm-ff-file-name-history-use-recentf t
          helm-ff-skip-boring-files nil)
    (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
    (add-to-list 'helm-boring-file-regexp-list "\\.git$")))

(use-package helm-eshell
  :init
  (after esh-mode
    (bind-key [remap eshell-pcomplete] #'helm-esh-pcomplete eshell-mode-map)
    (bind-key [remap eshell-previous-matching-input-from-input] #'helm-eshell-history eshell-mode-map)))

;; *** Extensions

(use-package helm-flyspell
  :if esk-spell-check
  :ensure t
  :defer t
  :init
  (after flyspell
    (add-hook 'flyspell-mode-hook #'(lambda () (bind-key "M-$" 'helm-flyspell-correct)))))

(use-package helm-flx
  :disabled t
  :ensure t
  :defer t
  :init (after helm (helm-flx-mode 1)))

(use-package helm-fuzzier
  :disabled t
  :ensure t
  :defer t
  :init (after helm-flx (helm-fuzzier-mode 1)))

(use-package helm-descbinds
  :ensure t
  :defer t
  :config (setq helm-descbinds-window-style 'split-window))
(use-package helm-unicode :ensure t :defer t)
(use-package helm-ls-git
  :ensure t
  :defer t
  :config
  (progn
    (setq helm-ls-git-status-command 'magit-status-internal
          helm-ls-git-show-abs-or-relative 'absolute)))

;; ** Files

(setq-default delete-by-moving-to-trash t)

(use-package tramp-cache
  :defer t
  :config
  (setq tramp-persistency-file-name (user-var-file "tramp")))

(use-package autorevert
  :defer t
  :init (after-init #'global-auto-revert-mode)
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info nil))

(use-package super-save
  :ensure t
  :defer t
  :commands super-save-initialize
  :init (after-init #'super-save-initialize))

(use-package simple
  :defer t
  :config
  (progn
    (setq save-interprogram-paste-before-kill t
          create-lockfiles nil
          make-backup-files nil)))

;; * Ineractive commands

(use-package commands :load-path "./lib")

;; * Projects
;; ** Version control

(use-package magit
  :ensure t
  :defer t
  :config
  (progn
    (setq magit-save-repository-buffers 'dontask)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defadvice magit-mode-quit-window (around magit-restore-screen activate)
      (let ((magit-status? (string-match-p "\\magit:.+" (buffer-name))))
        ad-do-it
        (when magit-status?
          (jump-to-register :magit-fullscreen))))))

(use-package git-timemachine :ensure t :defer t)

(use-package ediff
  :defer t
  :config
  (setq
   ;; avoid the crazy multi-frames setup
   ediff-window-setup-function 'ediff-setup-windows-plain
   ;; ignore whitespace
   ediff-diff-options "-w"
   ;; counter-intuitve naming here, but windows should be side-by-side...
   ediff-split-window-function 'split-window-horizontally))

(use-package diff-hl
  :ensure t
  :defer t
  :commands diff-hl-mode
  :init (add-hook 'prog-mode-hook 'diff-hl-mode)
  :config
  (setq diff-hl-draw-borders  nil))

;; ** Projectile

(use-package projectile
  :ensure t
  :defer t
  :commands projectile-golbal-mode
  :init (after-init #'projectile-global-mode)
  :diminish ""
  :config
  (progn
    (setq projectile-cache-file (user-var-file "projectile.cache")
          projectile-known-projects-file (user-var-file "projectile-bookmarks.el")
          projectile-completion-system 'helm
          projectile-use-git-grep t
          projectile-switch-project-action 'projectile-find-file
          projectile-globally-ignored-files
          (append projectile-globally-ignored-directories '("elpa")))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-project-root-files-bottom-up "project.clj")
    (defadvice projectile-replace
        (before projectile-save-all-and-replace activate)
      (save-some-buffers t))
    (defadvice projectile-switch-project (after projectile-sync-default-directory)
      (when (projectile-project-p)
        (setq default-directory (projectile-project-root))))
    (projectile-load-known-projects)))

(use-package persp-projectile
  :ensure t
  :defer t
  :init
  (after projectile
    (require 'persp-projectile nil t)
    (persp-mode 1)))

;; * Editing

(use-package undo-tree
  :ensure t
  :defer t
  :commands (undo-tree)
  :diminish undo-tree-mode
  :init (after-init #'global-undo-tree-mode))

(use-package multiple-cursors
  :ensure t
  :defer t
  :config
  (progn
    ;; Hydra workarounds
    (setq mc/list-file (user-var-file ".mc-lists.el")
          mc/cmds-to-run-once
          '(hydra-multiple-cursors/mc/skip-to-next-like-this
            hydra-multiple-cursors/mc/edit-lines-and-exit
            hydra-multiple-cursors/mc/mark-all-like-this-dwim-and-exit
            hydra-multiple-cursors/mc/mark-next-like-this
            hydra-multiple-cursors/mc/skip-to-next-like-this
            hydra-multiple-cursors/mc/unmark-next-like-this
            hydra-multiple-cursors/mc/mark-previous-like-this
            hydra-multiple-cursors/mc/skip-to-previous-like-this
            hydra-multiple-cursors/mc/unmark-previous-like-this
            hydra-multiple-cursors/mc/mark-all-in-region-regexp-and-exit
            hydra-multiple-cursors/mc/mark-sgml-tag-pair
            hydra-multiple-cursors/mc/mark-more-like-this-extended))))

(use-package iedit
  :ensure t
  :defer t
  :commands (esk-iedit)
  :config
  (progn
    (defun esk-iedit ()
      "iedit in defun or across the entire file when prefixed with C-u."
      (interactive)
      (if iedit-mode
          (iedit-mode nil)
        (when (bound-and-true-p lispy-left-p)
          (forward-char 1))
        (if (and current-prefix-arg
                 (= 4 (car current-prefix-arg)))
            (iedit-mode)
          (iedit-mode 0))))
    (after lispy
      (bind-key "M-i" 'esk-iedit lispy-mode-map-lispy))))

;; * Prose

(defvar prose-mode-map (make-sparse-keymap))

(defvar prose-mode-line-spacing 8)

(easy-mmode-define-minor-mode
    prose-mode
  "A mode for editing and reading prose."
  nil
  "Prose"
  prose-mode-map

  ;; Line spacing
  (set (make-local-variable 'line-spacing)
       (if prose-mode prose-mode-line-spacing line-spacing))

  (variable-pitch-mode prose-mode)
  (visual-line-mode prose-mode))

(use-package adaptive-wrap
  :defer t
  :commands adaptive-wrap-prefix-mode
  :init (add-hook 'prose-mode-hook 'adaptive-wrap-prefix-mode))

;; ** Spell checking

(use-package ispell
  :defer t
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-extra-args '("--sug-mode=fast"
                            "--lang=en_US"
                            "--add-filter=url"
                            "--add-filter=email")))

(use-package flyspell
  :if esk-spell-check
  :ensure t
  :defer t
  :init (after org (add-hook 'org-mode-hook 'flyspell-mode))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

;; ** Markdown

(use-package markdown-mode :ensure t :mode "\\.md\\'")

;; * Navigation
;; ** Bookmarks

(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (user-var-file "bookmarks")))

;; ** Mark ring

(use-package simple
  :defer t
  :config
  (setq mark-ring-max 64 global-mark-ring-max 64))

(use-package visible-mark
  :ensure t
  :init (after-init #'global-visible-mark-mode))

;; ** Files

(use-package recentf
  :defer t
  :init (after-init #'recentf-mode)
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 200))

(use-package savehist
  :defer t
  :init (after-init #'savehist-mode)
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (user-var-file "savehist")))

(use-package saveplace
  :defer t
  :init (setq-default save-place t)
  :config (setq save-place-file (temp-file "places")))

;; ** In-buffer search

(use-package isearch
  :defer t
  :init
  (progn
    ;; Reveal content of subtrees during isearch, alse see reveal-mode
    (setq-default isearch-invisible 'open)
    (setq search-whitespace-regexp ".*?"))
  :config
  ;; Allow deleting chars in the search string, use C-r to search backwards
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish highlight-symbol-mode
  :commands highlight-symbol-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))
  :config
  (progn
    (setq highlight-symbol-on-navigation-p nil
          highlight-symbol-idle-delay 1)))

(use-package imenu
  :defer t
  :config (setq-default imenu-auto-rescan t))

(use-package imenu-anywhere :ensure t :defer t)

(use-package avy
  :ensure t
  :config
  (progn
    (setq avy-style 'at-full ;; 'de-bruijn
          avy-background t
          avy-all-windows t)
    ;; Use C-' in isearch to bring up avy
    (avy-setup-default)))

(use-package link-hint :ensure t :defer t)

(use-package swiper-helm
  :ensure t
  :defer t
  :config
  (after helm
    (setq swiper-helm-display-function 'helm-default-display-buffer)))

(use-package goto-last-change :ensure t :defer t)

;; ** Outlines

(defvar esk-lisps-outline-regexp "^;;;?\s[*]+\s.+")

(defvar esk-outline-regexp-alist
  `((clojure-mode       . ,esk-lisps-outline-regexp)
    (clojurescript-mode . ,esk-lisps-outline-regexp)
    (clojurec-mode      . ,esk-lisps-outline-regexp)
    (emacs-lisp-mode    . ,esk-lisps-outline-regexp)))

(defun esk-outline-major-modes ()
  (mapcar #'car esk-outline-regexp-alist))

(defun esk-outline-set-regexp ()
  (let ((simple-regexp (cdr (assoc major-mode esk-outline-regexp-alist))))
    (when simple-regexp
      (make-local-variable 'outline-regexp)
      (setq outline-regexp simple-regexp))))

(use-package poporg
  :ensure t
  :defer t
  :commands poporg-dwim)

(use-package outline
  :defer t
  :commands outline-minor-mode
  :init
  (progn
    (dolist (mode (esk-outline-major-modes))
      (let ((mode-hook (intern (format "%s-hook" mode))))
        (add-hook mode-hook #'esk-outline-set-regexp)
        (add-hook mode-hook #'outline-minor-mode)))))

(use-package outshine
  :ensure t
  :defer t
  :commands outshine-hook-function
  :init
  (after outline
    (add-hook 'outline-minor-mode-hook #'outshine-hook-function))
  :config
  (setq outshine-startup-folded-p nil))

(use-package outorg :ensure t :defer t)
(use-package navi-mode :ensure t :defer t)

(comment
 (advice-add #'outline-show-all :after #'recenter))

(use-package outline-magic :ensure t :defer t)

;; * Editing

(setq-default comment-auto-fill-only-comments t)
(delete-selection-mode 1)

;; * Programming

(use-package evil-nerd-commenter
  :ensure t
  :defer t
  :commands evilnc-comment-or-uncomment-lines)

(use-package eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook 'eldoc-mode))

(use-package hl-todo
  :ensure t
  :defer t
  :commands hl-todo-mode
  :init (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paren
  :defer t
  :init (add-hook 'prog-mode-hook 'show-paren-mode)
  :config
  (progn
    (add-hook 'activate-mark-hook #'(lambda () (show-paren-mode -1)))
    (add-hook 'deactivate-mark-hook #'(lambda () (show-paren-mode 1)))
    (setq show-paren-delay 0.02
          show-paren-priority 999)))

;; ** Syntax checking

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :commands flycheck-mode
  :init (after-init #'global-flycheck-mode)
  :preface (defvar flycheck-mode-line-lighter " *")
  :config
  (defun esk-flycheck-list-errors ()
    (interactive)
    (call-interactively #'flycheck-list-errors)
    (select-window (get-buffer-window flycheck-error-list-buffer)))
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc clojure-cider-typed)))

;; ** Indentation & whitespace

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(use-package page-break-lines
  :ensure t
  :defer t
  ;; Don't wait after init or restored buffer won't be set
  :init (add-hook 'prog-mode-hook #'page-break-lines-mode)
  :config
  (advice-add
   #'switch-to-buffer :after
   (lambda (_ &optional no-confirm no-enable) (page-break-lines--update-display-tables))))

(use-package aggressive-indent
  :ensure t
  :defer t
  :commands esk-aggressive-indent-mode-maybe
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode
  :config
  (progn
    (setq aggressive-indent-comments-too t)
    (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'tuareg-mode)
    (defun esk-aggressive-indent-mode-maybe (flag)
      (if (member major-mode aggressive-indent-excluded-modes)
          (aggressive-indent-mode -1)
        (aggressive-indent-mode flag)))))

(use-package auto-indent-mode
  :ensure t
  :defer t
  :config
  (setq auto-indent-indent-style 'aggressive
        auto-indent-blank-lines-on-move nil
        auto-indent-current-pairs nil))

(use-package ws-butler
  :ensure t
  :defer t
  :diminish ws-butler-mode
  :commands ws-butler-mode)

(easy-mmode-define-minor-mode
    esk-savage-indent-mode
  "Savage indent mode"
  nil nil nil
  (progn
    (electric-indent-mode (if esk-savage-indent-mode -1 1))
    (setq show-trailing-whitespace esk-savage-indent-mode)
    (auto-indent-mode esk-savage-indent-mode)
    (esk-aggressive-indent-mode-maybe esk-savage-indent-mode)
    (ws-butler-mode esk-savage-indent-mode)))

(add-hook 'prog-mode-hook 'esk-savage-indent-mode)

;; ** Structured editing

(use-package paredit
  :ensure t
  :defer t
  :diminish paredit-mode
  :commands paredit-mode
  :preface
  (progn
    (defun minibuffer-paredit-mode-maybe ()
      (if (eq this-command 'eval-expression)
          (paredit-mode 1)))
    (defun paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))
    (defun paredit-wrap-square-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-square))
    (defun paredit-wrap-curly-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-curly)))
  :functions
  (paredit-wrap-round
   paredit-wrap-square
   paredit-wrap-curly)
  :init
  (progn
    (add-hook 'minibuffer-setup-hook 'minibuffer-paredit-mode-maybe)
    (add-hook 'lisps-mode-hook 'paredit-mode))
  :config
  (bind-keys
   :map paredit-mode-map
   ("M-(" . paredit-wrap-round)
   ("M-)" . paredit-wrap-round-from-behind)
   ("M-[" . paredit-wrap-square)
   ("M-]" . paredit-wrap-square-from-behind)
   ("M-{" . paredit-wrap-curly)
   ("M-}" . paredit-wrap-curly-from-behind)))

(use-package lispy
  :ensure t
  :defer t
  :init (add-hook 'lisps-mode-hook 'lispy-mode t)
  :config
  (setq lispy-no-permanent-semantic t
        lispy-completion-method 'default
        lispy-visit-method 'projectile
        lispy-compat '(edebug cider)))

(use-package clojure
  :if esk-clojure
  :load-path "./lib/clojure-semantic"
  :defer t
  :init
  (after lispy
    (after clojure-mode
      (load-library "clojure"))))

;; ** Completion

(use-package abbrev
  :defer t
  :diminish abbrev-mode
  :if (file-exists-p abbrev-file-name)
  :config
  (progn
    (setq-default abbrev-mode t)
    (quietly-read-abbrev-file)))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :commands company-mode
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    ;; 1. When typing, do not start auto-completion
    ;; 2. On TAB, complete as much as possible inline
    ;; 3. On TAB again, cycle
    ;; 4. Use C-; to open helm company
    (defun esk-overloaded-tab ()
      (interactive)
      (if (and outline-regexp (looking-at outline-regexp))
          (call-interactively #'outline-cycle)
        (call-interactively #'company-indent-or-complete-common)))
    (bind-key "<tab>" 'esk-overloaded-tab company-mode-map)
    (bind-key "<tab>" 'company-complete-common-or-cycle company-active-map)
    (setq company-tooltip-align-annotations t
          company-auto-complete t
          company-idle-delay nil
          company-minimum-prefix-length 2
          company-abort-manual-when-too-short t)))

(use-package company-quickhelp
  :defer t
  :config (setq company-quickhelp-delay 0.4))

(use-package company-dabbrev
  :defer t
  :config
  (setq company-dabbrev-ignore-case t
        company-dabbrev-ignore-invisible t
        company-dabbrev-downcase nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init
  (after company
    (bind-key "C-h" 'company-quickhelp-mode company-active-map)))

(use-package company-statistics
  :ensure t
  :defer t
  :init
  (after company
    (add-hook 'company-mode-hook (turn-on #'company-statistics-mode)))
  :config
  (setq company-statistics-file (user-var-file "company-statistics-cache.el")
        company-statistics-size 2000))

;; * Languages

(use-package semantic
  :defer t
  :init (after-init (turn-on #'semantic-mode)))

;; ** Lisps

(defvar lisps-mode-map (make-keymap))
(setq prettify-symbols-alist nil)

(defun lisps-pretty ()
  (progn
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("lambda" . ?𝝺) prettify-symbols-alist)
    (push '("fn" . ?𝝺) prettify-symbols-alist)
    (turn-on-prettify-symbols-mode)))

(use-package subword :defer t :diminish subword-mode)

(easy-mmode-define-minor-mode
    lisps-mode "Minor mode for lisp-family languages"
    :keymap lisps-mode-map
    (progn
      (lisps-pretty)
      (eldoc-mode 1)
      (subword-mode 1)))

;; ** Clojure

(use-package clojure-mode
  :if esk-clojure
  :ensure t
  :defer t
  :config (add-hook 'clojure-mode-hook (turn-on #'lisps-mode)))

(use-package flycheck-clojure
  :if esk-clojure
  :ensure t
  :defer t
  :init (after flycheck (flycheck-clojure-setup)))

(use-package clojure-mode-extra-font-locking
  :if esk-clojure
  :ensure t
  :defer t
  :init (after clojure-mode
          (require 'clojure-mode-extra-font-locking nil t)))

(use-package cider
  :if esk-clojure
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
    (bind-key "C-c C-t" 'cider-test-jump clojure-mode-map)
    ;; (bind-key "C-c RET" 'esk-cider-repl-redo-last-input cider-mode-map)
    (setq cider-prompt-save-file-on-load 'always-save
          cider-prompt-for-symbol nil
          cider-repl-use-pretty-printing t
          cider-auto-jump-to-error nil
          cider-repl-history-file (user-var-file "nrepl-history"))))

(use-package cider-repl
  :defer t
  :config
  (progn
    (add-hook 'cider-repl-mode-hook 'company-mode)
    (add-hook 'cider-repl-mode-hook (turn-on #'lisps-mode))
    (setq cider-repl-pop-to-buffer-on-connect nil)))

(use-package cider-eval-sexp-fu
  :commands (eval-sexp-fu-flash-mode)
  :ensure t
  :defer t
  :init
  (add-hook 'lisps-mode-hook (turn-on #'eval-sexp-fu-flash-mode)))

(use-package clj-refactor
  :if esk-clojure
  :ensure t
  :defer t
  :diminish clj-refactor-mode
  :init (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (progn
    (setq cljr-magic-requires nil)
    (add-hook 'clojure-mode-hook #'(lambda () (cljr-add-keybindings-with-prefix "C-c C-m")))
    (use-package cljr-helm
      :ensure t
      :defer t
      :init
      (after clojure-mode
        (bind-key "C-c C-. h" 'cljr-helm)))))

;; ** Elisp

(use-package lisp-mode
  :defer t
  :preface
  (defun esk-imenu-add-use-package ()
    "Recognize `use-package` in imenu"
    (when (string= buffer-file-name (expand-file-name "init.el" "~/.emacs.d"))
      (add-to-list
       'imenu-generic-expression
       '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
       t)))
  :config
  (progn
    (bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)
    (add-hook 'emacs-lisp-mode-hook 'esk-imenu-add-use-package)
    (add-hook 'emacs-lisp-mode-hook (turn-on #'lisps-mode))
    (add-hook 'ielm-mode-hook (turn-on #'lisps-mode))))

(use-package elisp-slime-nav
  :ensure t
  :defer t
  :diminish ""
  :init (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

;; ** SQL

(use-package sql-indent
  :if esk-sql
  :ensure t
  :defer t
  :init
  (progn
    (after sql  (require 'sql-indent))
    (after edbi (require 'sql-indent)))
  :config
  (progn
    (setq sql-indent-offset 2
          sql-indent-first-column-regexp
          (concat "\\(^\\s-*"
                  (regexp-opt '("with" "window" "inner" "left" "outer" "right"
                                "select" "update" "insert" "delete"
                                "union" "intersect"
                                "from" "where" "into" "group" "having" "order"
                                "set"
                                "create" "drop" "truncate"
                                "begin" "end" "lock" "commit"
                                "alter" "add" "returning"
                                "copy" "set" "--") t)
                  "\\(\\b\\|\\s-\\)\\)\\|\\(^```$\\)"))))

(use-package sql
  :if esk-sql
  :defer t
  :config
  (progn
    (add-hook 'sql-mode-hook 'esk-savage-indent-mode)
    (after paredit (add-hook 'sql-mode-hook 'paredit-mode))
    (setq sql-product 'postgres
          sql-send-terminator t)))

;; Install:
;; sudo cpan RPC::EPC::Service DBI DBD::Pg
;; perl -MCPAN -e'install DBD::Pg'
;; Connect: dbi:Pg:dbname=mydb
(use-package edbi
  :if esk-sql
  :ensure t
  :defer t
  :config
  (add-hook 'edbi:sql-mode-hook #'(lambda () (run-hooks 'sql-mode-hook))))

(use-package edbi-minor-mode
  :if esk-sql
  :ensure t
  :defer t
  :init
  (progn
    (after sql
      (add-hook 'sql-mode-hook #'edbi-minor-mode))
    (after company-edbi
      (add-hook 'sql-mode-hook #'company-edbi-enable))))

(use-package company-edbi
  :if esk-sql
  :ensure t
  :defer t
  :commands (company-edbi)
  :preface
  (defun company-edbi-enable ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends #'company-edbi)
    (company-mode 1))
  :init
  (after sql
    (add-hook 'sql-mode-hook #'company-edbi-enable)))

;; ** OCaml

;; NOTE: ocp-indent, merlin and utop all have emacs-lisp code shipped as part of
;; their opam packages. It's prefereable to use those files rather than elpa
;; packages in order to ensure compatibility between emacs and the external
;; tools.

;; TODO: What's a robust way to sync with `opam switch` commands?

(use-package opam
  :if esk-ocaml
  :ensure t
  :defer t
  :preface
  (defun add-opam-load-path ()
    (interactive)
    (let ((opam-switch (string-trim (shell-command-to-string "opam config var prefix"))))
      (add-to-list 'load-path (expand-file-name "share/emacs/site-lisp" opam-switch))))
  :init
  (after tuareg
    (opam-init)
    (add-opam-load-path)))

(use-package tuareg
  :if esk-ocaml
  :ensure t
  :defer t
  :commands esk-tuareg-eval
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :config
  (progn
    (defun ocaml-bind-paredit (keymap)
      (bind-keys
       :map keymap
       ("C-M-f" . smie-forward-sexp-command)
       ("C-M-b" . smie-backward-sexp-command)
       ("[" . paredit-open-square)
       ("]" . paredit-close-square)
       ("{" . paredit-open-curly)
       ("}" . paredit-close-curly)
       ("}" . paredit-close-curly)
       ("<backspace>" . paredit-backward-delete)))
    (add-hook 'tuareg-mode-hook (turn-off 'eldoc-mode))
    (after paredit
      (add-hook 'tuareg-mode-hook 'paredit-mode)
      (add-hook 'tuareg-mode-hook  #'(lambda () (ocaml-bind-paredit tuareg-mode-map)))
      (bind-keys
       :map tuareg-mode-map
       ("RET" . reindent-then-newline-and-indent)
       ("C-c C-k" . tuareg-eval-buffer)
       ("C-c C-s" . utop)))))

(use-package utop
  :if esk-ocaml
  :defer t
  :commands utop-minor-mode
  :init (after tuareg (add-hook 'tuareg-mode-hook 'utop-minor-mode))
  :config
  (progn
    (after paredit
      (add-hook 'utop-mode-hook 'paredit-mode)
      (add-hook 'utop-mode-hook  #'(lambda () (ocaml-bind-paredit utop-mode-map))))))

(use-package ocp-indent
  :if esk-ocaml
  :defer t
  :init
  (after tuareg (require 'ocp-indent nil t)))

(use-package merlin
  :if esk-ocaml
  :defer t
  :diminish ""
  :init
  (progn
    ;; NOTE: from the merlin docs
    ;;
    ;; To use merlin-locate to go to the source of things installed with
    ;; opam, you first of all need to keep the source around when
    ;; installing, and let opam create .cmt files:
    ;; Set this in ~/.bash_profile:
    ;; export OPAMKEEPBUILDDIR=true
    ;; export OCAMLPARAM="_,bin-annot=1"
    ;; export OPAMBUILDDOC=true
    (setenv "OPAMKEEPBUILDDIR" "true")
    (setenv "OCAMLPARAM" "_,bin-annot=1")
    (setenv "OPAMBUILDDOC" "true")
    (after tuareg (add-hook 'tuareg-mode-hook 'merlin-mode)))
  :config
  (after tuareg
    (require 'merlin-company)
    (setq
     merlin-completion-types nil
     merlin-completion-arg-type nil
     merlin-completion-with-doc t
     merlin-completion-dwim nil
     merlin-error-after-save nil
     merlin-command 'opam
     merlin-default-flags '("-principal"))
    (define-key merlin-mode-map (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
    (define-key merlin-mode-map (kbd "C-c <down>") 'merlin-type-enclosing-go-down)))

(use-package flycheck-ocaml
  :if esk-ocaml
  :ensure t
  :defer t
  :init
  (after merlin (flycheck-ocaml-setup))
  :config
  (after merlin (setq merlin-error-after-save nil)))

(use-package makefile
  :defer t
  :init
  (progn
    (defun makefile-setup ()
      (set (make-local-variable 'indent-tabs-mode) t)
      (set (make-local-variable 'tab-width) 4))
    (add-hook 'makefile-mode-hook 'makefile-setup)))

;; * Eshell

(use-package eshell
  :if esk-shell
  :defer t
  :config
  (progn
    (setq eshell-scroll-to-bottom-on-output t
          eshell-scroll-show-maximum-output t)
    (add-hook 'eshell-mode-hook 'paredit-mode)))

(use-package em-unix
  :if esk-shell
  :defer t
  :config
  (setq eshell-cp-interactive-query t
        eshell-ln-interactive-query t
        eshell-mv-interactive-query t
        eshell-rm-interactive-query t
        eshell-mv-overwrite-files nil))

(use-package em-cmpl
  :if esk-shell
  :defer t
  :config
  (setq eshell-cmpl-ignore-case t))

(use-package em-term
  :if esk-shell
  :defer t
  :config
  (setq eshell-visual-commands
        (append '("tmux" "screen" "ssh") eshell-visual-commands)))

(use-package em-hist
  :if esk-shell
  :defer t
  :config
  (setq eshell-hist-ignoredups t))

(use-package eshell-z
  :if esk-shell
  :ensure t
  :defer t
  :init (after eshell (require 'eshell-z nil t)))

;; * Scratch buffer

(use-package unkillable-scratch :ensure t :init (unkillable-scratch 1))
(use-package persistent-scratch :ensure t :init (persistent-scratch-setup-default))

;; * Org Mode

(use-package org
  :if esk-org
  :defer t
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (sql . t)
       (ocaml . t)
       (clojure . t)
       (sh . t)))
    (add-to-list 'org-structure-template-alist
                 '("el"
                   "#+begin_src emacs-lisp\n  ?\n#+end_src"
                   "<src lang=\"emacs-lisp\">\n?\n</src>"))))

(use-package ob-core
  :if esk-org
  :defer t
  :config
  (setq org-confirm-babel-evaluate nil))

(use-package org-capture
  :if esk-org
  :defer t
  :config
  (progn
    (setq
     org-reverse-note-order t
     org-capture-templates
     '(("d" "Dev dump" entry (file "~/org/dev.org") "* %?\n  %i\n %a" :kill-buffer  t)
       ("j" "Journal" entry (file "~/org/journal.org") "* %U\n %?i\n %a" :kill-buffer t)))))

(use-package org-clock
  :if esk-org
  :defer t
  :bind ("C-c C-c" . hydra-org-clock/body)
  :preface
  (defvar esk-projects-clock-file
    (expand-file-name "~/projects/clock.org"))
  :config
  (setq org-clock-idle-time 15
        org-clock-in-resume t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-clocked-in-display 'both))

(use-package worf
  :if esk-org
  :ensure t
  :defer t
  :init (after org (add-hook 'org-mode-hook 'worf-mode)))

(use-package deft
  :if esk-notes
  :ensure t
  :defer t
  :config
  (setq deft-recursive t
        deft-use-filename-as-title t
        deft-default-extension "org"))

(use-package erc
  :if esk-irc
  :defer t
  :commands erc
  :config
  (progn
    ;; Ignoring
    (setq erc-hide-list '("JOIN" "PART" "QUIT"))
    ;; Tracking
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
    ;; Filling chan buffers
    (setq erc-fill-function 'erc-fill-static
          erc-fill-static-center 15)
    (use-package erc-hl-nicks
      :ensure t
      :defer t
      :init (add-hook 'erc-mode-hook 'erc-hl-nicks-mode))))

;; * SSH

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))

;; * Communication

(use-package url
  :defer t
  :config (setq url-configuration-directory (user-var-file "url/")))

(use-package browse-url
  :defer t
  :config (setq browse-url-browser-function 'eww-browse-url))

;; * Keybindings

(load-file (expand-file-name "keybindings.el" user-emacs-directory))

;; * Documentation

(use-package know-your-http-well :ensure t :defer t)

;; Profile

(profiler-start 'cpu+mem)

(provide 'init)
;; init.el ends here
