;;; emacs-swagger-kit --- Elisp goodness!
;;; Commentary:
;;; Code:
;;
;;; * Bootstap

(require 'cl-lib)

(progn
  ;; Debug on error when loading
  (setq debug-on-error t)
  (add-hook 'after-init-hook 'toggle-debug-on-error))

;;; ** Elpa & use-package

(progn
  (setq package-archives
        '(("org" . "http://orgmode.org/elpa/")
          ("melpa" . "http://melpa.milkbox.net/packages/")
          ("gnu" . "http://elpa.gnu.org/packages/")))
  (when (require 'package)
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    (unless (package-installed-p 'org-plus-contrib)
      (package-install 'org-plus-contrib))))

(progn
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))

;;; ** Config helpers

(defmacro after (mode &rest body)
  "`eval-after-load' `MODE', wrapping `BODY' in `progn'."
  (declare (indent defun))
  (let ((s (if (symbolp mode)
               (symbol-name mode)
             mode)))
    `(eval-after-load ,(symbol-name mode)
       (quote (progn ,@body)))))

;;; * Defaults

(progn
  (setq-default truncate-lines nil)
  (setq-default gc-cons-threshold (* 8 1024 1024))
  (setq-default completion-styles '(basic partial-completion substring))
  (setq-default enable-recursive-minibuffers t))

;;; ** Paths

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

;;; ** Fonts

(defvar preferred-fonts
  '(("DejaVu Sans Mono" . 120)
    ("Consolas" . 130)
    ("Ubuntu Mono" . 140)
    ("Source Code Pro" . 120)
    ("Inconsolata" . 140)
    ("Menlo-12" . 120)))

(progn
  (let* ((font (cl-find-if
                (lambda (font)
                  (find-font (font-spec :name (car font))))
                preferred-fonts))
         (family (car font))
         (height (cdr font)))
     (set-face-attribute 'default nil :family family)
     (set-face-attribute 'default nil :height height)))

;;; ** GUI

(progn
  (setq initial-scratch-message "")
  (setq inhibit-startup-message t)
  (when window-system
    (add-hook 'after-init-hook
              #'(lambda ()
                  (modify-all-frames-parameters
                   '((fullscreen . maximized)))))
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (setq ns-use-native-fullscreen nil
          use-file-dialog nil
          use-dialog-box nil)))

;;; ** UI

(progn
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type 'bar)
  (setq echo-keystrokes 0.1)
  (setq mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq ring-bell-function (lambda ()))
  (setq save-interprogram-paste-before-kill t)
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package diminish
  :ensure diminish)

(use-package hl-line
  :pre-load
  (progn
    (defvar hl-line-hooks
      '(dired-mode-hook))
    (defun hl-line-turn-on ()
      (when (not (bound-and-true-p hl-line-mode))
        (hl-line-mode 1))))
  :init
  (progn
    (dolist (hook hl-line-hooks)
      (add-hook hook 'hl-line-turn-on))))

(use-package highlight-blocks
  :ensure highlight-blocks
  :disabled t
  :init (add-hook 'prog-mode-hook 'highlight-blocks-mode)
  :config
  (progn
    (defun highlight-blocks-match-theme ()
      (let* ((bg (face-background 'default))
             (bg-lum (nth 2
                          (apply 'color-rgb-to-hsl
                                 (color-name-to-rgb bg))))
             (color-mod-fn (if (< bg-lum 0.5)
                               'color-lighten-name
                             'color-darken-name)))
        (dotimes (i 20)
          (set-face-background
           (intern (format "highlight-blocks-depth-%i-face" (+ 1 i)))
           (funcall color-mod-fn bg (+ 2 i))))))

    (defadvice load-theme
        (after highlight-blocks-match-theme activate)
      (highlight-blocks-match-theme))

    (defadvice disable-theme
        (after highlight-blocks-match-theme activate)
      (highlight-blocks-match-theme))))

(use-package number-font-lock-mode
  :ensure number-font-lock-mode
  :init (add-hook 'prog-mode-hook 'number-font-lock-mode))

(use-package automargin
  :ensure automargin
  :init (add-hook 'after-init-hook 'automargin-mode))

;;; ** Theme

(use-package theme-changer
  :ensure theme-changer
  :config
  (progn
    (setq calendar-location-name "Brooklyn, NY"
          calendar-latitude 40.71
          calendar-longitude -73.95)

    (use-package minimal-theme
      :ensure minimal-theme
      :defer t
      :init
      (progn
        (add-hook 'after-init-hook
                  #'(lambda ()
                      (change-theme 'minimal-light 'minimal)))

        (defun color-contrast-name (name pct)
          (let* ((bg (face-attribute 'default :background))
                 (rgb (color-name-to-rgb bg))
                 (hsl (apply 'color-rgb-to-hsl rgb))
                 (lum (caddr hsl)))
            (if (< 0.5 lum)
                (color-darken-name name pct)
              (color-lighten-name name pct))))

        (defun company-derive-theme ()
          (let* ((theme (car custom-enabled-themes))
                 (bg (face-attribute 'default :background)))
            (when theme
              (custom-theme-set-faces
               theme
               `(company-tooltip ((t (:inherit default :background ,(color-contrast-name bg 8)))))
               `(company-tooltip-selection ((t (:inherit company-tooltip :weight bold))))
               `(company-tooltip-common-selection ((t (:inherit default :background ,(color-contrast-name bg 5)))))
               `(company-tooltip-common ((t (:inherit company-tooltip :weight bold))))
               `(company-scrollbar-bg ((t (:background ,(color-contrast-name bg 15)))))
               `(company-scrollbar-fg ((t (:background ,(color-contrast-name bg 10)))))))))

        (defun flycheck-derive-theme ()
          (let* ((theme (car custom-enabled-themes))
                 (bg (face-attribute 'default :background)))
            (when theme
              (custom-theme-set-faces
               theme
               `(flycheck-fringe-info-face ((t (:inherit default))))
               `(flycheck-fringe-warning-face ((t (:inherit warning))))
               `(flycheck-fringe-error-face ((t (:inherit error))))
               `(flycheck-color-mode-line-info-face ((t (:inherit flycheck-fringe-info-face))))
               `(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning))))
               `(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error-face))))))))

        (defadvice load-theme (after derive-load-theme activate)
          (flycheck-derive-theme)
          (company-derive-theme))
        (defadvice disable-theme (after derive-disable-theme activate)
          (flycheck-derive-theme)
          (company-derive-theme))))))

(use-package rainbow-mode
  :ensure rainbow-mode
  :diminish (rainbow-mode "")
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;;; ** Dired

(use-package dired
  :config
  (progn
    (setq dired-auto-revert-buffer t)))

;;; ** Minibuffer completion

(use-package ido
  :config
  (progn
    (setq
     ido-auto-merge-work-directories-length -1
     ido-everywhere t
     ido-max-window-height 1
     ido-enable-flex-matching t
     ido-show-dot-for-dired t)
    (use-package flx-ido
      :ensure flx-ido
      :init (flx-ido-mode 1)
      :config (setq ido-use-faces nil))))

(use-package helm-config
  :ensure helm
  :pre-load (setq helm-command-prefix-key "C-h")
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini))
  :config
  (progn
    (bind-key "a" 'helm-apropos helm-command-map)

    (custom-set-faces
     '(helm-source-header ((t :inherit mode-line)))
     '(helm-selection ((t :inherit hl-line)))
     '(helm-visible-mark ((t :inherit region))))

    (setq helm-yank-symbol-first t)

    (use-package helm-command
      :config
      (progn
        (setq helm-M-x-always-save-history t)
        (use-package helm-descbinds
          :ensure helm-descbinds
          :init (bind-key "k" 'helm-descbinds helm-command-map))))

    (use-package helm-mode
      :diminish ""
      :init (helm-mode 1)
      :config
      (progn

        (use-package helm-swoop
          :ensure helm-swoop
          :init (bind-key "s" 'helm-swoop helm-command-map))

        (setq helm-buffer-details-flag nil
              helm-ff-file-name-history-use-recentf t
              helm-ff-auto-update-initial-value nil
              helm-ff-skip-boring-files t)
        (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")

        (defun my-helm-ff-down ()
          "Delete backward or go \"down\" [sic] one level when in
          folder root."
          (interactive)
          (if (looking-back "/")
              (call-interactively 'helm-find-files-down-one-level)
            (delete-char -1)))
        (bind-key "DEL" 'my-helm-ff-down helm-read-file-map)
        ;; Complete immendiately on TAB when finding files
        (bind-key "TAB" 'helm-execute-persistent-action helm-read-file-map)))))

;;; ** Keys

(progn
  (setq ns-command-modifier 'control
        ns-function-modifier 'super
        ns-control-modifier 'meta
        ns-option-modifier 'meta))

(use-package sequential-command
  :ensure sequential-command
  :defines sequential-lispy-indent
  :commands sequential-lispy-indent
  :init
  (add-hook
   'prog-mode-hook
   #'(lambda ()
       (local-set-key (kbd "TAB") 'sequential-lispy-indent)))
  :config
  (progn
    (defun paredit-indent-command ()
      (interactive)
      (save-excursion
        (paredit-indent-sexps)))
    (define-sequential-command sequential-lispy-indent
      indent-for-tab-command
      paredit-indent-command
      cleanup-buffer-or-region)))

;;; ** Files

(use-package files
  :init
  (progn
    (setq backup-by-copying t
          backup-directory-alist `((".*" . ,temporary-file-directory))
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))))

(use-package autorevert
  :init (global-auto-revert-mode 1))

;;; * Ineractive commands

(use-package commands
  :demand t
  :commands cleanup-buffer-or-region
  :load-path "./lib"
  :init
  (progn
    (bind-key "C-c C-c" 'cleanup-buffer-or-region prog-mode-map)))


(use-package free-keys
  :ensure free-keys)

;;; * Projects

(use-package projectile
  :ensure projectile
  :idle (projectile-global-mode 1)
  :config
  (progn
    (add-to-list 'projectile-project-root-files-bottom-up "project.clj")
    (setq projectile-completion-system 'helm-comp-read
          projectile-use-git-grep t
          projectile-remember-window-configs t
          projectile-mode-line-lighter " P"
          projectile-enable-caching nil)

    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-directories "elpa")

    (defadvice projectile-replace
        (before projectile-save-all-and-replace activate)
      (save-some-buffers t))

    (use-package helm-projectile
      :ensure helm-projectile
      :init
      (after helm-misc
        (bind-key "p" 'helm-projectile helm-command-map)))))

;;;; ** Version control

(use-package magit
  :ensure magit
  :bind ("C-c g" . magit-status)
  :config
  (progn
    (setq magit-emacsclient-executable "/usr/local/Cellar/emacs/HEAD/bin/emacsclient")
    (setq magit-save-some-buffers 'dontask)

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

(use-package diff-hl
  :ensure diff-hl
  :init (add-hook 'prog-mode-hook 'diff-hl-mode)
  :config
  (progn
    (after git-commit-mode
      (defadvice git-commit-commit (after git-commit-commit-after activate)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when diff-hl-mode
              (diff-hl-update))))))))

;;; * Editing

(progn
  (pending-delete-mode 1)
  (setq-default indent-tabs-mode nil))

(use-package undo-tree
  :ensure undo-tree
  :diminish ""
  :init (add-hook 'prog-mode-hook 'undo-tree-mode)
  :config
  (progn
    (setq
     undo-tree-visualizer-timestamps nil
     undo-tree-visualizer-diff nil
     undo-tree-auto-save-history t
     undo-tree-history-directory-alist `((".+" . ,(file-name-as-directory
                                                   (temp-file
                                                    ".undo-tree-history")))))))

;;; * Markdown

(use-package markdown-mode
  :ensure markdown-mode
  :mode "\\.md\\'")

;;; * Org

(use-package org
  :ensure org
  :config
  (use-package org-capture
    :bind ("C-c c" . org-capture)
    :config
    (progn
      (setq org-reverse-note-order t
            org-capture-templates
            '(("d" "Dev dump"
               entry (file "~/org/dev.org")
               "* %?\n  %i\n %a"
               :kill-buffer  t)
              ("j" "Journal"
               entry (file "~/org/journal.org")
               "* %U\n %?i\n %a"
               :kill-buffer t))))))

;;; * Navigation
;;; ** Buffer

(use-package god-mode
  :disabled t
  :ensure god-mode
  :init (add-hook 'prog-mode-hook 'god-local-mode)
  :bind (("<escape>" . god-local-mode))
  :config
  (progn
    (add-to-list 'god-exempt-major-modes 'eshell-mode)
    (add-to-list 'god-exempt-major-modes 'cider-repl-mode)

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

    ;;(add-hook 'god-mode-enabled-hook #'(lambda () (auto-indent-mode -1)))
    ;;(add-hook 'god-mode-disabled-hook #'(lambda () (auto-indent-mode 1)))
    (add-hook 'god-mode-enabled-hook #'(lambda () (hl-line-mode 1)))
    (add-hook 'god-mode-disabled-hook #'(lambda () (hl-line-mode -1)))
    (add-hook 'god-mode-enabled-hook 'god-update-cursor)
    (add-hook 'god-mode-disabled-hook 'god-update-cursor)))

(use-package imenu
  :config
  (setq imenu-auto-rescan t))

(use-package simple
  :config
  (progn
    (setq mark-ring-max 32
          global-mark-ring-max 32)
    (after helm-command
      (bind-key "SPC" 'helm-all-mark-rings helm-command-map))))

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :bind (("C-c C-SPC" . ace-jump-mode)))

(use-package iedit
  :ensure iedit
  :bind ("C-;" . iedit-mode))

(use-package highlight-symbol
  :ensure highlight-symbol
  :diminish ""
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-symbol-mode)
    (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))
  :config
  (progn
    (setq highlight-symbol-on-navigation-p t
          highlight-symbol-idle-delay 1)
    (bind-key "C-%" 'highlight-symbol-query-replace highlight-symbol-nav-mode-map)))

(use-package outline
  :diminish (outline-minor-mode "")
  :init (add-hook 'prog-mode-hook 'outline-minor-mode)
  :config
  (progn
    (setq outline-minor-mode-prefix "\C-c \C-o")
    (bind-key "M-P" 'outline-previous-heading outline-minor-mode-map)
    (bind-key "M-N" 'outline-next-heading outline-minor-mode-map)))

(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq  save-place-file (temp-file "places"))))

(use-package savehist
  :init (savehist-mode 1)
  :config
  (setq savehist-additional-variables
        '(kill-ring search-ring regexp-search-ring)))

(use-package recentf
  :init (recentf-mode 1)
  :config
  (progn
    (setq recentf-max-saved-items 100
          recentf-max-menu-items 100)))

;;; ** Windows & frames

(bind-key "s-h" 'bury-buffer)

;; Scroll buffers in-window
(bind-key "s-b" 'previous-buffer)
(bind-key "s-n" 'next-buffer)

;; Navigate windows
(bind-key "s-;" 'previous-multiframe-window)
(bind-key "s-'" 'next-multiframe-window)

;; (setq split-height-threshold 10)

(use-package dedicated
  :ensure dedicated
  :bind ("C-M-\\" . dedicated-mode))

(use-package popwin
  :ensure popwin
  :config
  (progn
    (setq popwin:popup-window-height 20
          popwin:popup-window-position 'top)
    (popwin-mode 1)
    (bind-key "C-z" popwin:keymap)))

(use-package perspective
  :ensure perspective
  :init (add-hook 'after-init-hook 'persp-mode)
  :config
  (progn
    (setq persp-initial-frame-name "emacs")
    (defun persp-next ()
      (interactive)
      (when (< (+ 1 (persp-curr-position)) (length (persp-all-names)))
        (persp-switch (nth (1+ (persp-curr-position)) (persp-all-names)))))))

(use-package windmove
  :idle (windmove-default-keybindings)
  :config
  (progn
    (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
    (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
    (global-set-key (kbd "S-C-<down>") 'shrink-window)
    (global-set-key (kbd "S-C-<up>") 'enlarge-window)
    (use-package buffer-move
      :ensure buffer-move
      :bind (("<M-S-down>" . buf-move-down)
             ("<M-S-left>" . buf-move-left)
             ("<M-S-up>" . buf-move-up)
             ("<M-S-right>" . buf-move-right)))))

(use-package win-switch
  :ensure win-switch
  :bind ("C-x o" . win-switch-mode))

(progn
  (defadvice split-window (after move-point-to-new-window activate)
    "Move to the newly created window after a split."
    (other-window 1)
    (next-buffer))
  (bind-key "C-c \\" 'window-focus-toggle)
  (bind-key "C-c |" 'window-split-toggle))

;;; * Search and replace
;;; ** Buffer
;;; ** Directory

;;; * Programming

(use-package prog-mode
  :config
  (progn
    (use-package hl-todo
      :ensure hl-todo
      :init (add-hook 'prog-mode-hook 'hl-todo-mode))

    (use-package eldoc
      :diminish ""
      :init (add-hook 'prog-mode-hook 'eldoc-mode))

    (use-package rainbow-delimiters
      :ensure rainbow-delimiters
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))))

;;; ** Syntax checking

(use-package flycheck
  :ensure flycheck
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (setq flycheck-mode-line-lighter " *")
    (use-package flycheck-color-mode-line
      :ensure flycheck-color-mode-line
      :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))

;;; ** Indentation

(use-package auto-indent-mode
  :ensure auto-indent-mode
  :diminish ""
  :init (add-hook 'prog-mode-hook 'auto-indent-mode)
  :config
  (progn
    (setq auto-indent-current-pairs t)
    (add-hook 'auto-indent-mode-hook
              #'(lambda ()
                  (when (bound-and-true-p electric-indent-mode)
                    (electric-indent-mode -1))))))

;;; ** Structured editing

(use-package paren
  :init (add-hook 'prog-mode-hook 'show-paren-mode)
  :config
  (progn
    (setq show-paren-style 'paren)
    (setq show-paren-delay 0.02)))

(use-package paredit
  :ensure paredit
  :diminish ""
  :init (add-hook 'prog-mode-hook 'paredit-mode))

;;; ** Completion

(use-package company
  :ensure company
  :diminish ""
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    (setq company-abort-manual-when-too-short t
          company-require-match nil
          company-idle-delay 0.1
          company-tooltip-limit 20
          company-minimum-prefix-length 2)
    (bind-key "<tab>" 'company-complete-selection company-active-map)))

(use-package yasnippet
  :ensure yasnippet
  :config
  (progn
    (setq yas-snippet-dirs `(,(user-file "snippets")))
    (unbind-key "<tab>" yas-minor-mode-map)
    (bind-key "C-c TAB" 'yas-expand yas-minor-mode-map)))

;;; ** Whitespace

(use-package whitespace-cleanup-mode
  :ensure whitespace-cleanup-mode
  :diminish (whitespace-cleanup-mode "")
  :init (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

;;; ** Clojure

(use-package clojure-mode
  :ensure clojure-mode
  :config
  (progn
    (progn
      (add-hook 'clojure-mode-hook 'subword-mode)
      (add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
      ;; Defun indent special forms and macros
      (put-clojure-indent 'defmulti 'defun)
      (put-clojure-indent 'defmethod 'defun)
      (put-clojure-indent 'defroutes 'defun))

    (use-package clojure-test-mode
      :ensure clojure-test-mode
      :init (add-hook 'clojure-mode-hook 'clojure-test-mode))

    (use-package cider
      :ensure cider
      :config
      (progn
        (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook 'paredit-mode)

        (setq cider-prompt-save-file-on-load nil
              nrepl-hide-special-buffers nil
              cider-auto-select-error-buffer nil
              cider-popup-stacktraces nil
              cider-repl-popup-stacktraces nil
              cider-repl-print-length 400
              cider-repl-history-file "~/.emacs.d/nrepl-history"
              cider-repl-popup-stacktraces t
              cider-repl-use-clojure-font-lock t
              cider-repl-use-pretty-printing nil)

        (use-package pulse
          :init
          (progn
            (defadvice cider-eval-last-sexp (after cider-flash-last activate)
              (pulse-momentary-highlight-region (save-excursion (backward-sexp) (point)) (point)))
            (defadvice cider-eval-defun-at-point (after cider-flash-at activate)
              (apply #'pulse-momentary-highlight-region (cider--region-for-defun-at-point)))))

        (use-package company-cider
          :ensure company-cider
          :init
          (after cider
            (add-to-list 'company-backends 'company-cider)
            (after cider-repl
              (add-hook 'cider-repl-mode-hook 'company-mode))))))

    (use-package clojure-cheatsheet
      :ensure clojure-cheatsheet
      :config
      (after helm
        (bind-key "C-c C-c" 'clojure-cheatsheet helm-command-map)))

    ;; Linting
    (use-package flycheck
      :init (add-hook 'clojure-mode-hook 'flycheck-mode)
      :config
      (progn
        ;; Kibit
        (use-package kibit-mode
          :ensure kibit-mode
          :defines clojure-kibit
          :defer t
          :init
          (progn
            (autoload 'clojure-kibit "kibit-mode" nil t)
            (add-hook 'clojure-mode-hook 'flycheck-mode)))

        ;; Eastwood, too noisy for now...
        ;; (progn
        ;;   (defvar  eastwood-linters
        ;;     (vector
        ;;      :misplaced-docstrings
        ;;      :deprecations
        ;;      :redefd-vars
        ;;      :def-in-def
        ;;      :wrong-arity
        ;;      :suspicious-test
        ;;      :suspicious-expression
        ;;      :unused-ret-vals
        ;;      :unused-ret-vals-in-try
        ;;      :unused-fn-args ;; (disabled by default)
        ;;      :unused-namespaces ;; (disabled by default)
        ;;      :unlimited-use
        ;;      :keyword-typos ;; (disabled by default)
        ;;      ))
        ;;   (after cider
        ;;     (flycheck-define-checker eastwood
        ;;       "A Clojure lint tool."
        ;;       :command ("lein"
        ;;                 "eastwood"
        ;;                 (eval
        ;;                  (format "{:namespaces [%s] :linters %s}" (cider-current-ns) eastwood-linters)))
        ;;       :error-patterns ((error line-start
        ;;                               "{:linter :" (one-or-more not-newline) ",\n"
        ;;                               " :msg " (message) ",\n"
        ;;                               " :line " line ",\n"
        ;;                               " :column " column "}" line-end))
        ;;       :modes clojure-mode)
        ;;     (add-to-list 'flycheck-checkers 'eastwood)))
        ))

    (use-package clj-refactor
      :ensure clj-refactor
      :init (add-hook 'clojure-mode-hook 'clj-refactor-mode))

    (use-package slamhound
      :ensure slamhound
      :init (bind-key "C-c s" 'slamhound clojure-mode-map))

    (use-package typed-clojure-mode
      :ensure typed-clojure-mode
      :init (add-hook 'clojure-mode-hook 'typed-clojure-mode)
      :config
      (progn
        (defun typed-clojure-font-lock ()
          (font-lock-add-keywords nil
                                  '(("(\\(def\\(record\\|protocol\\)>\\)\\s-+\\(\\w+\\)"
                                     (1 font-lock-keyword-face)
                                     (3 font-lock-function-name-face)))))
        (add-hook 'clojure-mode-hook 'typed-clojure-font-lock)))))

;;; ** Elisp

(defvar elisp-hooks '(emacs-lisp-mode-hook ielm-mode-hook))

(use-package lisp-mode
  :config
  (progn
    (bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)

    (add-hook 'minibuffer-setup-hook 'minibuffer-paredit-mode-maybe)

    (defun minibuffer-paredit-mode-maybe ()
      (if (eq this-command 'eval-expression)
          (paredit-mode 1)))

    (use-package elisp-slime-nav
      :ensure elisp-slime-nav
      :diminish ""
      :config
      (dolist (hook elisp-hooks)
        (add-hook hook 'turn-on-elisp-slime-nav-mode)))))

;;; * Eshell

(use-package eshell
  :config
  (progn
    ;; Scrolling
    (setq eshell-scroll-to-bottom-on-output t
          eshell-scroll-show-maximum-output t)

    (add-to-list 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom)

    (use-package esh-mode
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

    (use-package em-term
      :config
      (setq eshell-visual-commands
            (append '("tmux" "screen" "ssh")
                    eshell-visual-commands)))

    (use-package em-hist
      :config
      (setq eshell-hist-ignoredups t))))

(provide 'init)
;;; init.el ends here
