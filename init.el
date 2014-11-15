;;; emacs-swagger-kit --- Elisp goodness!
;;; Commentary:
;;; Code:

;; * Bootstap

(require 'cl-lib)

(progn
  ;; Debug on error when loading
  (toggle-debug-on-error)
  (add-hook 'after-init-hook
            #'(lambda () (toggle-debug-on-error))))

;; ** Elpa & use-package

(progn
  (setq package-archives
        '(("org" . "http://orgmode.org/elpa/")
          ("melpa" . "http://melpa.milkbox.net/packages/")
          ("gnu" . "http://elpa.gnu.org/packages/")))

  ;; (setq package-pinned-packages
  ;;       '((cider . "melpa-stable")))

  (when (require 'package)
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    (unless (package-installed-p 'org-plus-contrib)
      (package-install 'org-plus-contrib))))

(progn
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-idle-interval 1
        use-package-verbose t))

;; ** Config helpers

(defmacro after (mode &rest body)
  "`eval-after-load' `MODE', wrapping `BODY' in `progn'."
  (declare (indent defun))
  (let ((s (if (symbolp mode)
               (symbol-name mode)
             mode)))
    `(eval-after-load ,(symbol-name mode)
       (quote (progn ,@body)))))

(ignore-errors (use-package private))

;; * Defaults

(progn
  (setq-default locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq-default default-truncate-lines t)
  (setq-default truncate-lines t)
  (setq-default confirm-nonexistent-file-or-buffer nil)
  (setq-default gc-cons-threshold (* 8 1024 1024))
  (setq-default completion-styles '(basic partial-completion substring))
  (setq-default completion-cycle-threshold t))

;; ** Paths

(defun temp-file (name)
  "Return a temporary file with `NAME'."
  (expand-file-name name temporary-file-directory))

(defun user-file (name)
  "Return a file with `NAME' in `user-emacs-directory'."
  (expand-file-name name user-emacs-directory))

(progn
  (setq user-emacs-directory
        (file-name-directory
         (or load-file-name
             buffer-file-name)))
  (setq custom-file (user-file "custom.el")))

(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; ** Fonts

(defvar global-text-height-offset 0)

(defvar monospaced-fonts
  '(("Ubuntu Mono" . 120)
    ("Consolas" . 120)
    ("Input Mono Narrow" . 120)
    ("DejaVu Sans Mono" . 130)
    ("Source Code Pro" . 120)
    ("Inconsolata" . 140)
    ("Menlo-12" . 120)))

(defvar variable-fonts
  '(("Input Serif" . 180)
    ("DejaVu Serif" . 190)))

(defun set-preferred-font (height-offset)
  (let* ((mono-font (cl-find-if (lambda (font)
                                  (find-font (font-spec :name (car font))))
                                monospaced-fonts))
         (var-font (cl-find-if (lambda (font)
                                 (find-font (font-spec :name (car font))))
                               variable-fonts)))
    (set-face-attribute 'default nil :family (car mono-font))
    (set-face-attribute 'default nil :weight 'light)
    (set-face-attribute 'default nil :height (+ (cdr mono-font) height-offset))
    (set-face-attribute 'variable-pitch nil :family (car var-font))
    (set-face-attribute 'variable-pitch nil :height (+ (cdr var-font) height-offset))))

(when window-system
  (set-preferred-font 0)
  (setq line-spacing 0))

(defun global-text-height-increase ()
  (interactive)
  (set-preferred-font
   (incf global-text-height-offset 10)))

(defun global-text-height-decrease ()
  (interactive)
  (set-preferred-font
   (incf global-text-height-offset -10)))

(bind-key "C-x C-+" 'global-text-height-increase)
(bind-key "C-x C--" 'global-text-height-decrease)

;; ** GUI

(progn
  (setq initial-scratch-message "")
  (setq inhibit-startup-message t)
  (when window-system
    (add-hook 'after-init-hook
              #'(lambda () (modify-all-frames-parameters
                       '((fullscreen . maximized)))))
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (setq ns-use-native-fullscreen nil
          use-file-dialog nil
          use-dialog-box nil)))

(use-package fringe
  :init (fringe-mode 8))

(defun toggle-transparency ()
  (interactive)
  (let ((default-alpha '(90 90))
        (alpha (frame-parameter (selected-frame) 'alpha)))
    (set-frame-parameter (selected-frame) 'alpha
                         (if (not (eq default-alpha alpha))
                             default-alpha
                           '(100 100)))))

(defun inc-transparency ()
  (interactive)
  (let ((alpha (car (frame-parameter (selected-frame) 'alpha))))
    (set-frame-parameter (ider-pselected-frame) 'alpha (list  (- alpha 5)  (- alpha 5)))))

(defun dec-transparency ()
  (interactive)
  (let ((alpha (car (frame-parameter (selected-frame) 'alpha))))
    (set-frame-parameter (selected-frame) 'alpha (list  (+ alpha 5)  (+ alpha 5)))))

;; ** UI
(setq scroll-margin 5)
(setq scroll-conservatively 10000)
(setq-default cursor-in-non-selected-windows nil)
(setq-default cursor-type 'bar)
(setq echo-keystrokes 0.01)
(setq ring-bell-function nil)
(setq visible-bell t)
(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(0.01 ((shift) . 1) ((control) . nil)))
(setq save-interprogram-paste-before-kill t)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package diminish :ensure diminish)

(use-package hl-line :init (global-hl-line-mode 1))

(use-package highlight-numbers
  :ensure highlight-numbers
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(defvar esk-toggle-map nil)
(define-prefix-command 'esk-toggle-map)
(define-key ctl-x-map "t" 'esk-toggle-map)
(define-key esk-toggle-map "f" 'auto-fill-mode)
(define-key esk-toggle-map "l" 'toggle-truncate-lines)
(define-key esk-toggle-map "r" 'read-only-mode)

;; ** Theme

(use-package smart-mode-line
  :disabled t
  :ensure smart-mode-line
  :init
  (progn
    (setq sml/theme 'respectful)
    (add-hook 'after-init-hook 'sml/setup)))

(defvar dark-themes
  '(smyx gruvbox soft-charcoal minimal stekene-dark zonokai tomorrow))

(defvar light-themes
  '(espresso flatui hemisu-light minimal-light ritchie stekene-light tomorrow-bright))

(use-package theme-changer
  :ensure theme-changer
  :init
  (progn
    (setq custom-theme-directory (expand-file-name "themes/" user-emacs-directory)
          calendar-location-name "Brooklyn, NY"
          calendar-latitude 40.7
          calendar-longitude -74.0)
    (change-theme 'fiatui 'fiatui-dark)))

(use-package rainbow-mode
  :ensure rainbow-mode
  :diminish (rainbow-mode "")
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; ** Dired

(use-package dired
  :config
  (progn
    (setq dired-auto-revert-buffer t)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(use-package launch
  :ensure launch
  :idle (global-launch-mode 1)
  :config
  (progn
    (defun launch-dired-dwim ()
      (let ((marked-files (dired-get-marked-files)))
        (if marked-files
            (launch-files marked-files :confirm)
          (launch-directory (dired-current-directory)))))
    (defun stante-launch-dwim ()
      "Open the current file externally."
      (interactive)
      (if (eq major-mode 'dired-mode)
          (stante-launch-dired-dwim)
        (when (buffer-file-name)
          (launch-file (buffer-file-name))
          (launch-directory default-directory))))
    ))

;; ** Minibuffer completion

(use-package smex
  :disabled t
  :ensure smex
  :bind (("M-x" . smex)
         ("C-x C-m" . smex-major-mode-commands))
  :init (smex-initialize))

(use-package ido
  :disabled t
  :config
  (progn
    (setq
     ;; Like recentf for buffers
     ido-use-virtual-buffers t
     ;; Disable auto-entering matching directory
     ido-auto-merge-work-directories-length -1
     ;; You wish...
     ido-everywhere t
     ido-enable-flex-matching t
     ;; Flat look is easier to scan
     ido-max-window-height 1
     ido-enable-tramp-completion t
     ;; Like mtime sort for dirs
     ido-enable-last-directory-history t
     ido-enable-prefix nil
     ido-create-new-buffer 'always
     ido-use-filename-at-point 'guess
     ido-max-prospects 10
     ido-confirm-unique-completion nil
     ido-cannot-complete-command #'(lambda () (interactive)))

    (use-package ido-complete-space-or-hyphen
      :ensure ido-complete-space-or-hyphen)

    (use-package ido-sort-mtime
      :ensure ido-sort-mtime
      :init (ido-sort-mtime-mode 1))

    (use-package ido-ubitquitous
      :ensure ido-ubiquitous
      :init (ido-ubiquitous-mode 1))

    (use-package flx-ido
      :ensure flx-ido
      :init (flx-ido-mode 1))))

(use-package helm-config
  :ensure helm
  :pre-load (defvar helm-command-prefix-key "C-h")
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring))
  :config
  (progn
    (setq helm-yank-symbol-first t)
    (setq helm-M-x-always-save-history t)

    (custom-set-faces
     '(helm-source-header ((t :inherit mode-line)))
     '(helm-selection ((t :inherit hl-line)))
     '(helm-visible-mark ((t :inherit region))))

    (use-package helm-command
      :config
      (progn
        (bind-key "a" 'helm-apropos helm-command-map)
        (bind-key "o" 'helm-occur helm-command-map)

        (use-package helm-swoop
          :ensure helm-swoop
          :init
          (progn
            (bind-key "s" 'helm-swoop helm-command-map)
            (bind-key "C-s" 'helm-multi-swoop-all helm-command-map)))
        (use-package helm-descbinds
          :ensure helm-descbinds
          :defer t
          :init (bind-key "b" 'helm-descbinds helm-command-map))))

    (use-package helm-mode
      :init (helm-mode 1)
      :diminish (helm-mode "")
      :config
      (progn
        (bind-key "DEL" 'my-helm-ff-up helm-read-file-map)
        ;; Complete immendiately on TAB when finding files
        (bind-key "TAB" 'helm-execute-persistent-action helm-map)
        (bind-key "C-z" 'helm-select-action helm-map)
        (setq helm-quick-update t
              helm-idle-delay 0.01
              helm-input-idle-delay 0.01
              helm-ff-file-name-history-use-recentf t
              helm-ff-auto-update-initial-value nil
              helm-ff-skip-boring-files t
              helm-candidate-number-limit 1000
              helm-M-x-requires-pattern 0
              helm-buffers-fuzzy-matching t)

        (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
        (add-to-list 'helm-boring-file-regexp-list "\\.git$")
        (add-to-list 'helm-boring-file-regexp-list "\\.$")

        (use-package helm-eshell
          :init
          (after eshell
            (add-hook 'eshell-hook
                      #'(lambda ()
                          (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))))

        (defun my-helm-ff-up ()
          "Delete backward or go \"down\" [sic] one level when in
          folder root."
          (interactive)
          (if (looking-back "/")
              (call-interactively 'helm-find-files-up-one-level)
            (delete-char -1)))))))

;; ** Keybindings

(progn
  (setq ns-command-modifier 'control
        ns-control-modifier 'meta
        ns-option-modifier 'super))

(use-package sequential-command
  :ensure sequential-command
  :defer t
  :defines sequential-indent
  :commands sequential-indent
  :init
  (bind-key "TAB" 'sequential-indent prog-mode-map)
  :config
  (progn
    (defun esk-sexp-indent ()
      (interactive)
      (cond
       ((bound-and-true-p paredit-mode) (paredit-indent-sexps))
       ((bound-and-true-p smartparens-mode) (call-interactively 'sp-indent-defun))
       (t (index-sexp))))
    (define-sequential-command sequential-indent
      indent-for-tab-command
      esk-sexp-indent
      esk/cleanup)))

(use-package key-chord
  :ensure key-chord
  :idle (key-chord-mode 1)
  :config
  (progn
    (key-chord-define-global "ii" 'helm-semantic-or-imenu)
    (key-chord-define-global "hh" 'helm-projectile)
    (key-chord-define-global "jj" 'ace-jump-word-mode)
    (key-chord-define-global "jk" 'ace-jump-two-chars-mode)))

;; (use-package mykie
;;   :ensure mykie
;;   ;; :idle (mykie:initialize)
;;   )

;; ** Files

(use-package files
  :init
  (progn
    (setq backup-by-copying t
          backup-directory-alist `((".*" . ,temporary-file-directory))
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))))

(use-package autorevert
  :idle (global-auto-revert-mode 1))

(use-package auto-save-buffers-enhanced
  :ensure auto-save-buffers-enhanced
  :init (auto-save-buffers-enhanced 1)
  :config
  (progn
    (setq auto-save-buffers-enhanced-quiet-save-p t
          auto-save-buffers-enhanced-interval 1)))

;; * Ineractive commands

(use-package commands
  :demand t
  :bind (("C-w" . esk/backward-kill-word)
	 ("C-c \\" . esk/window-split-toggle))
  :commands esk/cleanup
  :load-path "./lib")

(use-package free-keys :ensure free-keys)

;; * Projects

(use-package projectile
  :ensure projectile
  :init (add-hook 'after-init-hook 'projectile-global-mode)
  :diminish ""
  :config
  (progn
    (setq projectile-completion-system 'helm
          projectile-use-git-grep t
          projectile-remember-window-configs t
          projectile-switch-project-action 'projectile-find-file))

  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-project-root-files-bottom-up "project.clj")

  (defadvice projectile-replace
      (before projectile-save-all-and-replace activate)
    (save-some-buffers t))

  (use-package helm-projectile
    :ensure helm-projectile
    :config
    (setq helm-projectile-sources-list
          '(helm-source-projectile-recentf-list
            helm-source-projectile-buffers-list
            helm-source-projectile-files-list)))

  (use-package persp-projectile
    :disabled t
    :ensure persp-projectile))

;; ** Version control

(defun git-add-current-buffer ()
  (interactive)
  (progn
    (save-buffer)
    (let* ((buffile (buffer-file-name))
           (output (shell-command-to-string
                    (concat "git add " (buffer-file-name)))))
      (message
       (if (not (string= output ""))
           output
         (concat "Staged " buffile))))))

(bind-key "C-c a" 'git-add-current-buffer)

(use-package magit
  :ensure magit
  :bind ("C-c g" . magit-status)
  :config
  (progn
    (setq magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient")
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          magit-set-upstream-on-push t)

    ;; Only one window when showing magit, then back!
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defadvice magit-mode-quit-window (around magit-restore-screen activate)
      (let ((magit-status? (string-match-p "\\magit:.+" (buffer-name))))
        ad-do-it
        (when magit-status?
          (jump-to-register :magit-fullscreen))))))

(use-package git-timemachine
  :ensure git-timemachine
  :bind ("C-c t" . git-timemachine))

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
  :ensure diff-hl
  :defer t
  :init (add-hook 'prog-mode-hook 'diff-hl-mode)
  :config
  (progn
    (after git-commit-mode
      (defadvice git-commit-commit (after git-commit-commit-after activate)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when diff-hl-mode
              (diff-hl-update))))))))

;; * Editing

(progn
  (pending-delete-mode 1)
  (setq-default indent-tabs-mode nil))

(use-package hilit-chg
  :diminish (highlight-changes-mode ""))

(use-package zop-to-char
  :ensure zop-to-char
  :bind ("M-z" . zop-to-char))

(use-package goto-chg
  :ensure goto-chg
  :diminish ""
  :defer t
  :init
  (progn
    (bind-key "C-." 'goto-last-change prog-mode-map)
    (bind-key "C-M-." 'goto-last-change-reverse prog-mode-map)))

(use-package undo-tree
  :ensure undo-tree
  :defer t
  :diminish ""
  :init (add-hook 'prog-mode-hook 'undo-tree-mode)
  :config
  (progn
    (setq
     undo-tree-auto-save-history t
     undo-tree-history-directory-alist `((".+" . ,(file-name-as-directory
                                                   (temp-file
                                                    ".undo-tree-history")))))))

(use-package multiple-cursors
  :ensure multiple-cursors
  :defer t
  :init
  (progn
    (bind-key "C->" 'mc/mark-next-like-this prog-mode-map)
    (bind-key "C-<" 'mc/mark-previous-like-this prog-mode-map)
    (bind-key "C-c C-<" 'mc/mark-all-like-this prog-mode-map)))

(use-package expand-region
  :ensure expand-region
  :bind (("C-+" . er/expand-region)
         ("C-=" . er/contract-region)))

(use-package volatile-highlights
  :ensure volatile-highlights
  :diminish ""
  :idle (volatile-highlights-mode 1))

;; * Prose

(defvar prose-mode-map (make-sparse-keymap))

(defvar prose-mode-line-spacing 6)

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
 (visual-line-mode prose-mode)

 (set-preferred-font 0))

(use-package adaptive-wrap
  :ensure adaptive-wrap
  :defer t
  :init (add-hook 'prose-mode-hook 'adaptive-wrap-prefix-mode))

;; ** Markdown

(use-package markdown-mode
  :ensure markdown-mode
  :mode "\\.md\\'")

;; * Navigation
;; ** Buffer

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(use-package god-mode
  :disabled t
  :ensure god-mode
  :init (add-hook 'prog-mode-hook 'god-local-mode)
  :bind ("<escape>" . god-local-mode)
  :config
  (progn
    (after eshell
      (add-to-list 'god-exempt-major-modes 'eshell-mode))
    (after cider-repl
      (add-to-list 'god-exempt-major-modes 'cider-repl-mode))

    (global-set-key (kbd "C-x C-1") 'delete-other-windows)
    (global-set-key (kbd "C-x C-2") 'split-window-below)
    (global-set-key (kbd "C-x C-3") 'split-window-right)
    (global-set-key (kbd "C-x C-0") 'delete-window)

    (define-key god-local-mode-map (kbd "z") 'repeat)
    (define-key god-local-mode-map (kbd ".") 'repeat)

    (defun god-enabled-p ()
      (or god-local-mode buffer-read-only))

    (defun god-update-cursor ()
      (setq cursor-type (if (god-enabled-p) 'box 'bar)))

    (defun god-update-hl-line ()
      (hl-line-mode (if (god-enabled-p) 1 -1)))

    (defun god-update-auto-indent ()
      (after auto-indent-mode
        (auto-indent-mode (if (god-enabled-p) -1 1))))

    (add-hook 'god-mode-enabled-hook 'god-update-hl-line)
    (add-hook 'god-mode-disabled-hook 'god-update-hl-line)
    (add-hook 'god-mode-enabled-hook 'god-update-auto-indent)
    (add-hook 'god-mode-disabled-hook 'god-update-auto-indent)
    (add-hook 'god-mode-enabled-hook 'god-update-cursor)
    (add-hook 'god-mode-disabled-hook 'god-update-cursor)))

(use-package imenu
  :defer t
  :config
  (progn
    (setq imenu-auto-rescan t)))

(use-package imenu-anywhere
  :ensure imenu-anywhere
  :defer t
  :commands helm-imenu
  :init
  (after helm-command
    (bind-key "C-i" 'helm-imenu-anywhere helm-command-map)))

(use-package simple
  :demand t
  :config
  (setq mark-ring-max 64 global-mark-ring-max 64))

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :bind ("C-c C-SPC" . ace-jump-mode)
  :config
  (progn
    (ace-jump-mode-enable-mark-sync)
    (defun ace-jump-two-chars-mode (query-char query-char-2)
      "AceJump two chars mode"
      (interactive (list (read-char "First Char:")
                         (read-char "Second:")))

      (if (eq (ace-jump-char-category query-char) 'other)
          (error "[AceJump] Non-printable character"))
      ;; others : digit , alpha, punc
      (let ((ace-jump-query-char query-char)
            (ace-jump-current-mode 'ace-jump-char-mode)
            (target (regexp-quote (concat (char-to-string query-char)
                                          (char-to-string query-char-2)))))
        (ace-jump-do target)))))

(use-package ace-isearch
  :disabled t
  :ensure ace-isearch
  :idle (global-ace-isearch-mode 1))

(use-package iedit
  :ensure iedit
  :bind ("C-;" . iedit-mode))

(use-package highlight-symbol
  :ensure highlight-symbol
  :defer t
  :diminish ""
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))
  :config
  (progn
    (setq highlight-symbol-on-navigation-p nil
          highlight-symbol-idle-delay 1)
    (bind-key "C-%" 'highlight-symbol-query-replace highlight-symbol-nav-mode-map)))

(use-package outline
  :defer t
  :diminish (outline-minor-mode "")
  :init (add-hook 'prog-mode-hook 'outline-minor-mode)
  :config
  (progn
    (setq outline-minor-mode-prefix "\C-c \C-o")

    (bind-key "M-P" 'outline-previous-heading outline-minor-mode-map)
    (bind-key "M-N" 'outline-next-heading outline-minor-mode-map)))

(use-package outshine
  :disabled t
  :ensure outshine
  :init (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq  save-place-file (temp-file "places"))))

(use-package savehist
  :idle (savehist-mode 1)
  :config
  (progn
    (setq savehist-additional-variables
          '(search-ring regexp-search-ring)
          savehist-autosave-interval 60)))

(use-package recentf
  :idle (recentf-mode 1)
  :config
  (progn
    (setq recentf-max-saved-items 100
          recentf-max-menu-items 100)))

;; * Windows management

(bind-key "s-h" 'bury-buffer)

;; Scroll buffers in-window
(bind-key "s-b" 'previous-buffer)
(bind-key "s-n" 'next-buffer)

(use-package zygospore
  :ensure zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package dedicated
  :ensure dedicated
  :bind ("C-M-\\" . dedicated-mode))

(use-package popwin
  :ensure popwin
  :idle (popwin-mode 1)
  :config
  (progn
    (advice-add
     'popwin:stick-popup-window
     :before #'(lambda () (unless (popwin:popup-window-live-p)
                       (call-interactively 'popwin:popup-last-buffer))))
    (bind-key "C-z" popwin:keymap)
    (bind-key "q" 'popwin:close-popup-window popwin:keymap)
    (setq popwin:popup-window-height 0.2
          popwin:adjust-other-windows t)
    (push '("^\*cider-repl .+\*$" :regexp t :stick t) popwin:special-display-config)
    (push '("*cider-error*" :stick nil) popwin:special-display-config)
    (push '("*grep*" :stick t) popwin:special-display-config)
    (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
    (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)))

(use-package simple-screen
  :ensure simple-screen
  :init (bind-key "C-z" 'simple-screen-map))

(use-package perspective
  :ensure perspective
  :disabled t
  :init (add-hook 'after-init-hook 'persp-mode)
  :config
  (progn
    (progn
      (bind-key "z" 'persp-switch-quick simple-screen-map)
      (bind-key "s" 'persp-switch simple-screen-map)
      (bind-key "n" 'persp-next simple-screen-map)
      (bind-key "p" 'persp-prev simple-screen-map)
      (bind-key "r" 'persp-rename simple-screen-map)
      (bind-key "c" 'persp-kill simple-screen-map)
      (bind-key "k" 'persp-remove-buffer simple-screen-map)
      (bind-key "a" 'persp-add-buffer simple-screen-map)
      (bind-key "i" 'persp-import simple-screen-map))
    (setq persp-initial-frame-name "emacs")))

(use-package win-switch
  :ensure win-switch
  :init (win-switch-setup-keys-ijkl "\C-xo"))

;; Switch
(use-package windmove
  :bind (("<left>" . windmove-left)
         ("<right>" . windmove-right)
         ("<up>" . windmove-up)
         ("<down>" . windmove-down))
  :config (setq windmove-wrap-around nil))

;; Resize
(use-package commands
  :bind (("S-<left>" . move-border-left)
         ("S-<right>" . move-border-right)
         ("S-<up>" . move-border-up)
         ("S-<down>" . move-border-down)))

;; Rearrange
(use-package buffer-move
  :ensure buffer-move
  :bind (("M-<down>" . buf-move-down)
         ("M-<left>" . buf-move-left)
         ("M-<up>" . buf-move-up)
         ("M-<right>" . buf-move-right)))

;; Uno/redo
(use-package winner :init (winner-mode 1))

(progn
  (defadvice split-window (after move-point-to-new-window activate)
    "Move to the newly created window after a split."
    (other-window 1)
    (next-buffer)))

;; * Editing

(setq-default comment-auto-fill-only-comments t)

;; * Search and replace
;; ** Buffer
;; ** Directory
;; * Programming

(use-package prog-mode
  :defer t
  :config
  (progn
    (use-package semantic
      :idle (semantic-mode 1))

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :defer t
      :init (bind-key "M-;" 'evilnc-comment-or-uncomment-lines prog-mode-map))

    (use-package eldoc
      :diminish ""
      :defer t
      :init (add-hook 'prog-mode-hook 'eldoc-mode))

    (use-package hl-todo
      :ensure hl-todo
      :defer t
      :init (add-hook 'prog-mode-hook 'hl-todo-mode))

    (use-package rainbow-delimiters
      :ensure rainbow-delimiters
      :defer t
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

    (use-package hl-sexp
      :ensure hl-sexp
      :defer t

      :init
      (add-hook 'prog-mode-hook 'hl-sexp-mode))))

;; ** Syntax checking

(use-package flycheck
  :ensure flycheck
  :defer t
  :diminish ""
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (defvar flycheck-mode-line-lighter " *")
    (use-package flycheck-color-mode-line
      :ensure flycheck-color-mode-line
      :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))

;; ** Indentation

(use-package aggressive-indent
  :ensure aggressive-indent
  :diminish ""
  :init (add-hook 'prog-mode-hook 'aggressive-indent-mode)
  :config
  (progn
    (setq aggressive-indent-comments-too t)
    (add-to-list 'aggressive-indent-modes-to-prefer-defun 'clojure-mode)))

(use-package auto-indent-mode
  :ensure auto-indent-mode
  :disabled t
  :defer t
  :diminish (auto-indent-mode "")
  :init
  (progn
    (defun esk-auto-indent-mode ()
      (interactive)
      (when (not buffer-read-only)
        (auto-indent-mode 1)))
    (add-hook 'prog-mode-hook 'esk-auto-indent-mode))
  :config
  (progn
    (setq auto-indent-next-pair t
          auto-indent-backward-delete-char-behavior 'all
          auto-indent-indent-style 'moderate
          auto-indent-blank-lines-on-move nil
          auto-indent-kill-line-at-eol nil)))

;; ** Structured editing

(use-package poporg
  :ensure poporg
  :init (bind-key "C-c e" 'poporg-dwim prog-mode-map)
  :config
  (bind-key "C-c C-c" 'poporg-dwim poporg-mode-map))

(use-package paren
  :defer t
  :init (add-hook 'prog-mode-hook 'show-paren-mode)
  :config
  (progn
    (add-hook 'activate-mark-hook #'(lambda () (show-paren-mode -1)))
    (add-hook 'deactivate-mark-hook #'(lambda () (show-paren-mode 1)))
    (setq show-paren-delay 0.02)))

(use-package smartparens-config
  :ensure smartparens
  :disabled t
  :init
  (progn
    (smartparens-global-strict-mode 1)
    (show-smartparens-mode 1))
  :config
  (progn
    (setq sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil)
    (setq sp-show-pair-overlays nil)

    ;; Reindent sexp on newline
    (advice-add
     'sp-newline :after #'(lambda () (call-interactively 'sp-indent-defun)))
    ;; More paredit-like movements:
    (bind-key "RET" 'sp-newline)
    (bind-key ")" 'sp-up-sexp smartparens-mode-map)
    (bind-key "]" 'sp-up-sexp smartparens-mode-map)
    (bind-key "}" 'sp-up-sexp smartparens-mode-map)
    (bind-key "C-M-f" 'sp-forward-sexp smartparens-mode-map)
    (bind-key "C-M-b" 'sp-backward-sexp smartparens-mode-map)
    (bind-key "C-M-u" 'sp-backward-up-sexp smartparens-mode-map)
    (bind-key "C-M-d" 'sp-down-sexp smartparens-strict-mode-map)
    (bind-key "C-M-p" 'sp-backward-down-sexp smartparens-mode-map)
    (bind-key "C-M-n" 'sp-up-sexp smartparens-mode-map)
    (bind-key "C-M-k" 'sp-kill-sexp smartparens-mode-map)
    (bind-key "C-M-w" 'sp-copy-sexp smartparens-mode-map)
    (bind-key "M-s" 'sp-splice-sexp smartparens-mode-map)
    (bind-key "M-<up>" 'sp-splice-sexp-killing-backward smartparens-mode-map)
    (bind-key "M-<down>" 'sp-splice-sexp-killing-forward smartparens-mode-map)
    (bind-key "M-r" 'sp-splice-sexp-killing-around smartparens-mode-map)
    (bind-key "M-?" 'sp-convolute-sexp smartparens-mode-map)
    (bind-key "C-)" 'sp-forward-slurp-sexp smartparens-mode-map)
    (bind-key "C-<right>" 'sp-forward-slurp-sexp smartparens-mode-map)
    (bind-key "C-}" 'sp-forward-barf-sexp smartparens-mode-map)
    (bind-key "C-<left>" 'sp-forward-barf-sexp smartparens-mode-map)
    (bind-key "C-(" 'sp-backward-slurp-sexp smartparens-mode-map)
    (bind-key "C-M-<left>" 'sp-backward-slurp-sexp smartparens-mode-map)
    (bind-key "C-{" 'sp-backward-barf-sexp smartparens-mode-map)
    (bind-key "C-M-<right>" 'sp-backward-barf-sexp smartparens-mode-map)
    (bind-key "M-S" 'sp-split-sexp smartparens-mode-map)
    (bind-key "M-J" 'sp-join-sexp smartparens-mode-map)
    (bind-key "C-M-t" 'sp-transpose-sexp smartparens-mode-map)
    ;; extra strict bindings
    (bind-key "M-q" 'sp-indent-defun smartparens-strict-mode-map)
    (bind-key "C-j" 'sp-newline smartparens-strict-mode-map)))

(use-package paredit
  :ensure paredit
  :defer t
  :diminish ""
  :config
  (progn
    (add-hook 'lisps-mode-hook 'paredit-mode)
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
      (paredit-wrap-curly))

    (bind-key "M-(" 'paredit-wrap-round paredit-mode-map)
    (bind-key "M-)" 'paredit-wrap-round-from-behind paredit-mode-map)
    (bind-key "M-[" 'paredit-wrap-square paredit-mode-map)
    (bind-key "M-]" 'paredit-wrap-square-from-behind paredit-mode-map)
    (bind-key "M-{" 'paredit-wrap-curly paredit-mode-map)
    (bind-key "M-}" 'paredit-wrap-curly-from-behind paredit-mode-map)

    (defun minibuffer-paredit-mode-maybe ()
      (if (eq this-command 'eval-expression)
          (paredit-mode 1)))

    (add-hook 'minibuffer-setup-hook 'minibuffer-paredit-mode-maybe)))

;; ** Completion

(use-package abbrev :diminish "")

(use-package hippie-exp
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package company
  :ensure company
  :defer t
  :diminish ""
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    (bind-key "<tab>" 'company-complete-selection company-active-map)
    (setq company-idle-delay 0.3
          company-minimum-prefix-length 2
          company-require-match nil
          ;;company-begin-commands '(self-insert-command)
          company-show-numbers t
          company-backends '(company-capf
                             (company-dabbrev company-dabbrev-code company-keywords)
                             company-yasnippet))))

(use-package yasnippet
  :ensure yasnippet
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :diminish ""
  :config
  (progn
    (add-to-list 'yas-snippet-dirs (user-file "snippets"))
    (setq yas-indent-line 'auto
          yas-wrap-around-region t)
    (yas-reload-all)))

;; ** Whitespace

(use-package ws-butler
  :ensure ws-butler
  :defer t
  :diminish (ws-butler-mode "")
  :init
  (progn
    (add-hook 'prog-mode-hook 'ws-butler-mode)
    (add-hook 'prog-mode-hook #'(lambda () (setq show-trailing-whitespace t)))))

;; * Languages
;; ** Lisps

(defun lisps-pretty ()
  (progn
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("lambda" . ?λ) prettify-symbols-alist)
    (push '("fn" . ?ƒ) prettify-symbols-alist)
    (turn-on-prettify-symbols-mode)))

(defun lisps-imenu-include-comments ()
  (add-to-list 'imenu-generic-expression
               '("*" "^;; \\(.+\\)$" 1) t))

(defun lisps-insert-header-comment ()
  (interactive)
  (forward-line -1)
  (newline)
  (insert-char ?\; 72)
  (newline)
  (insert-char ?\; 2)
  (newline)
  (forward-line -1)
  (end-of-line)
  (insert " "))

(defvar lisps-mode-map
  (make-keymap))

(easy-mmode-define-minor-mode
 lisps-mode
 "Minor mode for lisp-family languages"
 :keymap lisps-mode-map
 (progn
   (after key-chord
     (key-chord-define lisps-mode-map "//" 'lisps-insert-header-comment))
   (lisps-pretty)
   (lisps-imenu-include-comments)
   (eldoc-mode 1)
   (subword-mode 1)))

(defun lisps-mode-turn-on ()
  (lisps-mode 1))


;; ** Clojure

(use-package clojure-mode
  :ensure clojure-mode
  :defer t
  :config
  (progn
    (put-clojure-indent 'defmulti 'defun)
    (put-clojure-indent 'defmethod 'defun)
    (put-clojure-indent 'defroutes 'defun)

    (add-hook 'clojure-mode-hook 'lisps-mode-turn-on)

    (use-package clojure-mode-extra-font-locking
      :ensure clojure-mode-extra-font-locking)

    (use-package nrepl-eval-sexp-fu
      :ensure nrepl-eval-sexp-fu
      :init (add-hook 'clojure-mode-hook 'nrepl-eval-sexp-fu-flash-mode))

    (use-package cider
      :ensure cider
      :config
      (progn
        (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
        (bind-key "C-c C-t" 'cider-test-jump clojure-mode-map)
        (setq cider-prompt-save-file-on-load nil
              cider-show-error-buffer t
              cider-auto-select-error-buffer nil
              cider-auto-jump-to-error nil
              nrepl-hide-special-buffers nil
              cider-repl-history-file "~/.emacs.d/nrepl-history")

        (use-package cider-repl
          :defer t
          :config
          (progn
            (add-hook 'cider-repl-mode-hook 'company-mode)
            (add-hook 'cider-repl-mode-hook 'lisps-mode-turn-on)
            (setq cider-repl-pop-to-buffer-on-connect nil)))))

    (use-package clj-refactor
      :ensure clj-refactor
      :defer t
      :diminish ""
      :init (add-hook 'clojure-mode-hook 'clj-refactor-mode)
      :config
      (progn
        (cljr-add-keybindings-with-prefix "C-c C-m")
        (after clojure
          (bind-key "C-M->" 'cljr-thread clojure-mode-map)
          (bind-key "C-M-<" 'cljr-unwind clojure-mode-map))))

    (use-package slamhound
      :ensure slamhound
      :defer t
      :init (bind-key "C-c s" 'slamhound clojure-mode-map))

    (use-package typed-clojure-mode
      :ensure typed-clojure-mode
      :disabled t
      :defer t
      :init (add-hook 'clojure-mode-hook 'typed-clojure-mode)
      :config
      (progn
        (defun typed-clojure-font-lock ()
          (font-lock-add-keywords nil
                                  '(("(\\(def\\(record\\|protocol\\)>\\)\\s-+\\(\\w+\\)"
                                     (1 font-lock-keyword-face)
                                     (3 font-lock-function-name-face)))))
        (add-hook 'clojure-mode-hook 'typed-clojure-font-lock)))

    ;; Linting
    (use-package kibit-mode
      :ensure kibit-mode
      :defer t
      :defines (clojure-kibit)
      :commands (clojure-kibit)
      :pre-load
      (progn
        (defun clojure-kibit-enable ()
          (interactive)
          (flycheck-mode 1)
          (add-hook 'clojure-mode-hook 'flycheck-mode))

        (defun clojure-kibit-disable ()
          (interactive)
          (flycheck-mode -1)
          (remove-hook 'clojure-mode-hook 'flycheck-mode))))

    ;; Eastwood, too noisy for now...
    ;; (after cider
    ;;   (defvar eastwood-add-linters
    ;;     (vector
    ;;      ;; :keyword-typos
    ;;      ;;:unused-namespaces
    ;;      :unused-fn-args)
    ;;     "Really doesn't make sense to use a vector here,
    ;;       but saves the formatting for now...")

    ;;   (flycheck-define-checker eastwood
    ;;     "A Clojure lint tool."
    ;;     :command
    ;;     ("lein" "eastwood"
    ;;      (eval
    ;;       (format "{:namespaces [%s] :add-linters []}" (cider-find-ns) eastwood-add-linters)))
    ;;     :error-patterns
    ;;     ((error
    ;;       bol
    ;;       "{:linter" (one-or-more not-newline) ",\n"
    ;;       " :msg" (or (zero-or-one (syntax whitespace)) (zero-or-one "\n")) (message) ",\n"
    ;;       " :line " line ",\n"
    ;;       " :column " column "}" line-end))
    ;;     :modes clojure-mode)
    ;;   (add-to-list 'flycheck-checkers 'eastwood))
    ))

;; ** Elisp

(use-package lisp-mode
  :config
  (progn
    (bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)
    (add-hook 'emacs-lisp-mode-hook 'lisps-mode-turn-on)
    (add-hook 'ielm-mode-hook 'lisps-mode-turn-on)

    (use-package elisp-slime-nav
      :ensure elisp-slime-nav
      :defer t
      :diminish ""
      :init
      (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))))

;; * Eshell

(use-package eshell
  :config
  (progn
    ;; Scrolling
    (setq eshell-scroll-to-bottom-on-output t
          eshell-scroll-show-maximum-output t)

    (use-package esh-mode
      :defer t
      :config
      (progn
        (defun eshell/cds ()
          (eshell/cd (or (locate-dominating-file default-directory "src")
                         (locate-dominating-file default-directory ".git"))))

        (defun eshell/clear ()
          (interactive)
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max)))
          (eshell-send-input))

        (add-hook 'eshell-mode-hook
                  #'(lambda ()
                      (bind-key "C-l" 'eshell/clear eshell-mode-map)))))
    (use-package eshell-opt
      :config
      (use-package eshell-prompt-extras
        :ensure eshell-prompt-extras))

    (use-package em-term
      :defer t
      :config
      (setq eshell-visual-commands
            (append '("tmux" "screen" "ssh") eshell-visual-commands)))

    (use-package em-hist
      :defer t
      :config
      (setq eshell-hist-ignoredups t))))

;; * Org Mode

(use-package org
  :defer t
  :diminish (orgstruct-mode "")
  :config
  (progn
    (setq org-catch-invisible-edits 'smart)
    (setq org-structure-template-alist
          '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
            ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
            ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
            ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
            ("v" "#+begin_verbatim\n?\n#+end_verbatim" "<verbatim>\n?\n</verbatim>")
            ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
            ("l" "#+begin_latex\n?\n#+end_latex"
             "<literal style=\"latex\">\n?\n</literal>")
            ("l" "#+latex: " "<literal style=\"latex\">?</literal>")
            ("h" "#+begin_html\n?\n#+end_html"
             "<literal style=\"html\">\n?\n</literal>")
            ("h" "#+html: " "<literal style=\"html\">?</literal>")
            ("a" "#+begin_ascii\n?\n#+end_ascii" "")
            ("a" "#+ascii: " "")
            ("i" "#+index: ?" "#+index: ?")
            ("i" "#+include: %file ?"
             "<include file=%file markup=\"?\">")))
    (add-hook 'prog-mode-hook 'turn-on-orgstruct++)

    (defun orgstruct-lisps-turn-on ()
      (setq orgstruct-heading-prefix-regexp ";; *"))

    (add-hook 'lisps-mode-hook 'orgstruct-lisps-turn-on)

    (use-package org-capture
      :bind ("C-c c" . org-capture)
      :config
      (progn
        (setq org-reverse-note-order t
              org-capture-templates
              '(("d" "Dev dump" entry (file "~/org/dev.org") "* %?\n  %i\n %a" :kill-buffer  t)
                ("j" "Journal" entry (file "~/org/journal.org") "* %U\n %?i\n %a" :kill-buffer t)))))

    (use-package org-clock
      :config
      (setq org-clock-idle-time 15
            org-clock-in-resume t
            org-clock-persist t
            org-clock-persist-query-resume nil
            org-clock-clocked-in-display 'both))))

(use-package erc
  :defer t
  :config
  (progn
    ;; Joining
    (setq erc-autojoin-timing 'ident)
    ;; Tracking
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
    ;; Filling chan buffers
    (setq erc-fill-function 'erc-fill-static
          erc-fill-static-center 20)

    (use-package erc-hl-nicks
      :ensure erc-hl-nicks
      :init (add-hook 'erc-mode-hook 'erc-hl-nicks-mode))))

(use-package hardhat
  :ensure hardhat
  :idle (global-hardhat-mode 1)
  :config (setq hardhat-mode-lighter nil))

(provide 'init)
;; init.el ends here
