;;; emacs-swagger-kit
;;; Commentary:
;;; Code:
;; * TODOs
;;;

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

(toggle-debug-on-error)
(after-init 'toggle-debug-on-error)

;; ** ELPA

(setq package-archives
      '(("org"   . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; ** use-package

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-verbose t)

(use-package paradox
  :ensure t
  :defer t
  :commands paradox-enable
  :init (after-init #'paradox-enable)
  :config
  (setq paradox-execute-asynchronously t
        paradox-automatically-star nil))

(use-package private
  :if esk-private
  :demand t
  :load-path "./lib/")

(use-package diminish :ensure t :demand t)

;; * Emacs defaults

(setq-default gc-cons-threshold 200000000)
(setq-default cursor-type 'bar)
(setq initial-scratch-message ""
      inhibit-startup-message t
      scroll-margin 5
      scroll-conservatively 10000
      cursor-in-non-selected-windows nil
      echo-keystrokes 0.
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(0.02 ((shift) . 2) ((control) . nil))
      ring-bell-function nil
      visible-bell t
      save-interprogram-paste-before-kill t)

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

;; * Key-bindings

(use-package hydra :ensure t :defer t)

(use-package free-keys :ensure t :defer t)

(use-package key-chord
  :ensure t
  :defer t
  :commands key-chord-mode
  :init (after-init (turn-on #'key-chord-mode))
  :config (setq key-chord-two-keys-delay 0.1))

(use-package which-key
  :ensure t
  :defer t
  :diminish ""
  :init	(after-init (turn-on #'which-key-mode))
  :config (which-key-setup-side-window-right))

;; * Windows management
;; ** Windows

(defun esk-alternate-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun esk-alternate-window ()
  (interactive)
  (next-multiframe-window)
  (other-window -1))

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
  :init (after-init (turn-on #'persp-mode))
  :config (setq persp-show-modestring t))

;; ** Windows

(use-package winner
  :defer t
  :init (after-init (turn-on #'winner-mode)) :defer t)

(use-package zygospore :ensure t :defer t)

(use-package ace-window
  :ensure t
  :defer t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
	aw-scope 'frame))

;; * Looks
;; ** Fonts

(defvar esk-fonts
  '("-*-Fira Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"
    "-*-M+ 1mn-normal-normal-normal-*-11-*-*-*-p-0-iso10646-1"))

;; (defun font-existsp (font)
;;   (if (null (x-list-fonts font))
;;       nil t))

(when window-system
  (ignore-errors
    (set-frame-font (car esk-fonts))))

;; ** GUI

(use-package spacemacs-theme
  :if esk-gui
  :ensure spacemacs-theme
  :init
  (progn
    (setq spacemacs-theme-org-highlight nil
          spacemacs-theme-org-height nil)
    (after-init (lambda () (load-theme 'spacemacs-light t nil)))))

(use-package spaceline
  :ensure spaceline
  :defer t
  :functions (spaceline-emacs-theme)
  :init (use-package spaceline-config :config (spaceline-emacs-theme))
  :config
  (advice-add 'load-theme :after (lambda (theme &optional no-confirm no-enable) (powerline-reset))))

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq use-file-dialog nil
	use-dialog-box nil))

(use-package fringe
  :init (fringe-mode 4)
  :config
  (progn
    (setq overflow-newline-into-fringe nil
          indicate-empty-lines t
          indicate-buffer-boundaries t
          indicate-unused-lines t)
    (setf (cdr (assq 'continuation fringe-indicator-alist))
          '(nil right-curly-arrow))))

(defun inc-transparency ()
  (interactive)
  (let ((alpha (or (car (frame-parameter (selected-frame) 'alpha)) 100)))
    (set-frame-parameter (selected-frame) 'alpha (list  (- alpha 5)  (- alpha 5)))))

(defun dec-transparency ()
  (interactive)
  (let ((alpha (or (car (frame-parameter (selected-frame) 'alpha)) 100)))
    (set-frame-parameter (selected-frame) 'alpha (list  (+ alpha 5)  (+ alpha 5)))))

;; ** UI

(use-package hl-line
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook (turn-on #'hl-line-mode))
    (add-hook 'dired-mode-hook (turn-on #'hl-line-mode))
    (after magit (add-hook 'magit-mode-hook (turn-on #'hl-line-mode)))))

(use-package linum
  :defer t
  :init (add-hook 'prog-mode-hook (turn-on #'linum-mode))
  :defines (esk-linum-current-timer)
  :functions (linum-update-current)
  :preface (defvar-local esk-linum-current-timer nil)
  :config
  (progn
    (setq linum-delay t)
    (setq linum-format " %4d ")
    ;; coreate a new var to keep track of the current update timer.
    ;; rewrite linum-schedule so it waits for 1 second of idle time
    ;; before updating, and so it only keeps one active idle timer going
    (defun linum-schedule ()
      (when (timerp esk-linum-current-timer)
        (cancel-timer esk-linum-current-timer))
      (setq esk-linum-current-timer
            (run-with-idle-timer 1 nil #'linum-update-current)))))

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
 (use-package plan9-theme :ensure t)
 (use-package spacemacs-theme :ensure t)
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
  :disabled t
  :ensure t
  :defer t
  :init (after-init (turn-on #'psession-mode))
  :config
  (setq psession-elisp-objects-default-directory (user-var-file "elisp-objects/")))

(use-package helm-flx
  :ensure t
  :defer t
  :init (after helm (helm-flx-mode 1)))

(use-package helm
  :ensure t
  :functions (helm-autoresize-mode)
  :commands (helm-M-x
	     helm-mini
	     helm-find-files
	     helm-show-kill-ring)
  :init (after-init (turn-on #'helm-mode))
  :config
  (progn
    ;; Remap persistent action to TAB
    (bind-keys :map helm-map
               ("C-z" . helm-select-action)
               ("<tab>" . helm-execute-persistent-action)
               ("C-i" . helm-execute-persistent-action))
    ;; Window setup
    (setq helm-echo-input-in-header-line t
	  helm-autoresize-max-height 38
	  helm-autoresize-min-height 38
          helm-candidate-number-limit 300
	  helm-split-window-default-side 'below
	  helm-split-window-in-side-p t
	  helm-full-frame nil
	  helm-always-two-windows nil)
    (helm-autoresize-mode 1)
    ;; Fuzzy matching
    (setq helm-recentf-fuzzy-match t
    	  helm-buffers-fuzzy-matching t
    	  helm-locate-fuzzy-match t
    	  helm-M-x-fuzzy-match t
    	  helm-apropos-fuzzy-match t
    	  helm-lisp-fuzzy-completion t
    	  helm-M-x-always-save-history t)
    ;; Sub-packages
    (use-package helm-config :demand t)
    (use-package helm-command
      :defer t
      :config
      (progn
        (setq helm-command-prefix-key "C-c h")
        (bind-key "C-i" 'helm-execute-persistent-action helm-map)
        (bind-key "C-M-p" 'helm-follow-action-backward helm-map)
        (bind-key "C-M-n" 'helm-follow-action-forward helm-map)
        (bind-key "a" 'helm-apropos helm-command-map)
	(bind-key "o" 'helm-occur helm-command-map)))
    ;; Helm completing-read
    (use-package helm-mode
      :diminish helm-mode
      :defer t
      :config
      (progn
        (setq helm-quick-update t
	      helm-idle-delay 0
	      helm-input-idle-delay 0
	      helm-ff-transformer-show-only-basename t
	      helm-ff-file-name-history-use-recentf t
	      helm-ff-skip-boring-files t
	      helm-M-x-requires-pattern 0)
	(add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
	(add-to-list 'helm-boring-file-regexp-list "\\.git$")))
    (use-package helm-org
      :defer t
      :commands helm-org-in-buffer-headings)))

(use-package helm-descbinds :ensure t :defer t)
(use-package helm-unicode :ensure t :defer t)
(use-package helm-ls-git
  :ensure t
  :defer t
  :config
  (progn
    (setq helm-ls-git-show-abs-or-relative 'relative)))

(use-package helm-dash
  :ensure t
  :defer t
  :defines (helm-dash-docsets helm-dash-installed-docsets)
  :functions (esk-helm-dash-install helm-dash-pg helm-dash-clojure helm-dash-web)
  :commands (helm-dash-at-point esk-helm-dash-install)
  :preface
  (progn
    (defvar esk-dash-docsets
      '("Ansible" "Clojure" "OpenCV_Java" "OpenCV_C" "OCaml" "CSS" "HTML" "Bash" "PostgreSQL"))
    (defun esk-helm-dash-install (docset-name)
      (unless (member docset-name (helm-dash-installed-docsets))
        (helm-dash-install-docset docset-name)))
    (defun esk-dash-limit (docsets-names)
      (set (make-local-variable 'helm-dash-docsets) docsets-names))
    (defun helm-dash-ocaml () (esk-dash-limit '("OCaml")))
    (defun helm-dash-bash () (esk-dash-limit '("Bash")))
    (defun helm-dash-pg () (esk-dash-limit '("PostgreSQL")))
    (defun helm-dash-web () (esk-dash-limit '("CSS" "HTML")))
    (defun helm-dash-clojure () (esk-dash-limit '("Clojure"))))
  :init
  (progn
    (after tuareg (add-hook 'tuareg-mode-hook 'helm-dash-ocaml))
    (after sh-script (add-hook 'sh-mode-hook 'helm-dash-bash))
    (after sql (add-hook 'sql-mode-hook 'helm-dash-pg))
    (after css-mode (add-hook 'css-mode-hook 'helm-dash-web))
    (after html-mode (add-hook 'html-mode-hook 'helm-dash-web))
    (after clojure-mode (add-hook 'clojure-mode-hook 'helm-dash-clojure)))
  :config
  (progn
    (setq helm-dash-browser-func 'eww)
    (dolist (docset esk-dash-docsets)
      (esk-helm-dash-install docset))))

;; ** Files

(setq-default delete-by-moving-to-trash t)

(use-package tramp-cache
  :config
  (setq tramp-persistency-file-name (user-var-file "tramp")))

(use-package auto-save-buffers-enhanced
  :ensure t
  :defer t
  :init (auto-save-buffers-enhanced 1)
  :config
  (setq
   auto-save-buffers-enhanced-interval 3
   auto-save-buffers-enhanced-quiet-save-p t
   auto-save-buffers-enhanced-exclude-regexps '("^.+\\.cljs" "^.+\\.cljc")))

(use-package simple
  :disabled t
  :init
  (add-hook 'find-file-hook (turn-on #'auto-save-mode))
  :config
  (progn
    (defun save-buffer-if-visiting-file (&optional args)
      (interactive)
      (when (and (buffer-file-name) (buffer-modified-p))
        (do-auto-save t t)))
    (add-hook 'auto-save-hook 'save-buffer-if-visiting-file)
    (setq create-lockfiles nil
          make-backup-files nil
          backup-directory-alist `((".*" . ,temporary-file-directory))
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
          auto-save-default t
          auto-save-visited-file-name t
          auto-save-interval 1
          auto-save-timeout 1)))

(use-package autorevert
  :disabled t
  :init (after-init (turn-on #'global-auto-revert-mode))
  :config
  (setq auto-revert-interval 2
	auto-revert-check-vc-info nil))

;; * Ineractive commands

(use-package commands :load-path "./lib")

;; * Projects

(use-package skeletor
  :ensure t
  :config (setq skeletor-project-directory "~/projects/"))

(use-package projectile
  :ensure t
  :defer t
  :commands projectile-golbal-mode
  :init (after-init (turn-on #'projectile-global-mode))
  :diminish ""
  :config
  (progn
    (setq
     projectile-cache-file (user-var-file "projectile.cache")
     projectile-known-projects-file (user-var-file "projectile-bookmarks.eld")
     projectile-completion-system 'helm
     projectile-use-git-grep t
     projectile-switch-project-action 'projectile-find-file
     projectile-globally-ignored-files
     (append projectile-globally-ignored-directories '("elpa" ".repl-0.0-3211")))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-project-root-files-bottom-up "project.clj")
    (defadvice projectile-replace
	(before projectile-save-all-and-replace activate)
      (save-some-buffers t))
    (defadvice projectile-switch-project (after projectile-sync-default-directory)
      (when (projectile-project-p)
        (setq default-directory (projectile-project-root))))))

(use-package persp-projectile
  :ensure t
  :defer t
  :init (after projectile (require 'persp-projectile)))

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
  :preface (add-hook 'prog-mode-hook 'diff-hl-mode)
  :config
  (progn
    (setq diff-hl-draw-borders nil)
    (comment (diff-hl-flydiff-mode 1))))

;; * Editing

(use-package undo-tree
  :ensure t
  :defer t
  :commands (undo-tree)
  :diminish undo-tree-mode
  :init (after-init (turn-on #'global-undo-tree-mode)))

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
  :disabled t
  :ensure t
  :config
  (progn
    (defun esk-iedit-bind ()
      (interactive)
      (unless (bound-and-true-p lispy-mode)
        (local-set-key (kbd "M-i") 'iedit-mode)))
    (add-hook 'prog-mode-hook 'esk-iedit-bind)))

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
  :init
  (after org
    (add-hook 'org-mode-hook 'flyspell-mode))
  :config
  (setq flyspell-issue-welcome-flag nil
	flyspell-issue-message-flag nil))

(use-package helm-flyspell
  :if esk-spell-check
  :ensure t
  :defer t
  :init
  (after flyspell
    (add-hook 'flyspell-mode-hook #'(lambda () (bind-key "M-$" 'helm-flyspell-correct)))))

;; ** Markdown

(use-package markdown-mode :ensure t :defer t :mode "\\.md\\'")

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

;; ** Files

(use-package recentf
  :defer t
  :init (after-init (turn-on #'recentf-mode))
  :config
  (setq recentf-max-saved-items 1000
	recentf-max-menu-items 200))

(use-package savehist
  :defer t
  :init (after-init (turn-on #'savehist-mode))
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
	savehist-autosave-interval 60
	savehist-file (user-var-file "savehist")))

(use-package saveplace
  :defer t
  :init (setq-default save-place t)
  :config (setq save-place-file (temp-file "places")))

;; * In-buffer search

(use-package isearch
  :defer t
  :config
  (progn
    ;; Reveal content of subtrees during isearch, alse see reveal-mode
    (setq isearch-invisible 'open)
    ;; Allow deleting chars in the search string, use C-r to search backwards
    (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)))

(use-package avy-zap :ensure t :defer t)

(use-package avy
  :ensure t
  :defer t
  :commands (avy-goto-char-2)
  :init (avy-setup-default))

(use-package swiper :ensure t :defer t)
(use-package swiper-helm :ensure t :defer t)

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

;; * Outlines

(defvar esk-lisps-outline-regexp "^;;;?\s[*]+\s.+")

(defvar esk-lisps-org-headlines-regexp
  "^\\(;;;? \\*+\\)\\(?: +\\(TODO\\|DONE\\|MAYBE\\|DONE\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(.*?\\)\\)??\\(?:[ 	]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ 	]*$")

(defvar esk-outline-regexp-alist
  `((clojure-mode . ,esk-lisps-outline-regexp)
    (emacs-lisp-mode . ,esk-lisps-outline-regexp)))

(defvar esk-org-headings-regexp-alist
  `((clojure-mode . ,esk-lisps-org-headlines-regexp)
    (emacs-lisp-mode . ,esk-lisps-org-headlines-regexp)))

(defun esk-outline-set-regexp ()
  (let ((cons (assoc major-mode esk-outline-regexp-alist)))
    (when cons
      (setq outline-regexp (cdr cons))
      (setq orgstruct-heading-prefix-regexp (cdr cons)))))

(defun esk-org-headings-set-regexp ()
  (let ((cons (assoc major-mode esk-org-headings-regexp-alist)))
    (when cons (setq org-complex-heading-regexp (cdr cons)))))

(add-hook 'prog-mode-hook 'esk-outline-set-regexp)
(add-hook 'prog-mode-hook 'esk-org-headings-set-regexp)

(advice-add #'outline-show-all :after #'recenter)

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

(use-package hl-sexp
  :ensure t
  :defer t
  :commands hl-sexp-mode
  :init (add-hook 'prog-mode-hook 'hl-sexp-mode))

(use-package poporg
  :ensure t
  :defer t
  :commands poporg-dwim
  :config (bind-key "C-c C-c" 'poporg-dwim poporg-mode-map))

(use-package paren
  :defer t
  :init (add-hook 'prog-mode-hook 'show-paren-mode)
  :config
  (progn
    (add-hook 'activate-mark-hook #'(lambda () (show-paren-mode -1)))
    (add-hook 'deactivate-mark-hook #'(lambda () (show-paren-mode 1)))
    (setq show-paren-delay 0.02)
    (setq show-paren-style 'mixed)))

;; ** Syntax checking

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :commands flycheck-mode
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :preface (defvar flycheck-mode-line-lighter " *")
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package helm-flycheck :ensure t :defer t)

;; ** Indentation & whitespace

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(use-package page-break-lines
  :ensure t
  :defer t
  :init
  (after-init (turn-on #'global-page-break-lines-mode)))

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
  :config (setq lispy-no-permanent-semantic t))

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
      (if (and (bound-and-true-p outline-regexp) (looking-at outline-regexp))
          (call-interactively 'outline-cycle)
        (call-interactively 'company-indent-or-complete-common)))
    (bind-key "<tab>" 'esk-overloaded-tab company-mode-map)
    (bind-key "<tab>" 'company-complete-common-or-cycle company-active-map)
    (setq company-tooltip-align-annotations t
          company-auto-complete 'company-explicit-action-p
          company-idle-delay nil
          company-minimum-prefix-length nil
          company-abort-manual-when-too-short t)))

(use-package company-quickhelp
  :defer t
  :config
  (setq company-quickhelp-delay 0.4))

(use-package company-flx
  :ensure t
  :defer t
  :init (after company (company-flx-mode 1)))

(use-package helm-company
  :ensure t
  :defer t
  :init (after company (bind-key "C-;" 'helm-company company-active-map)))

(use-package company-dabbrev
  :defer t
  :config
  (setq company-dabbrev-ignore-case t
	company-dabbrev-ignore-invisible t
	company-dabbrev-downcase nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (after company (bind-key "C-h" 'company-quickhelp-mode company-active-map)))

(use-package company-statistics
  :ensure t
  :defer t
  :init (after company (add-hook 'company-mode-hook (turn-on #'company-statistics-mode)))
  :config
  (setq company-statistics-file (user-var-file "company-statistics-cache.el")
        company-statistics-size 2000))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (progn
    (setq yas-snippet-dirs `(,(user-file "snippets"))
	  yas-indent-line 'auto
	  yas-wrap-around-region t)
    (yas-reload-all)))

(use-package auto-yasnippet
  :ensure t
  :defer t
  :commands aya-open-line)

;; * Languages
;; ** Lisps

(defvar lisps-mode-map (make-keymap))

(defun lisps-pretty ()
  (progn
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("lambda" . ?λ) prettify-symbols-alist)
    (push '("fn" . ?λ) prettify-symbols-alist)
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

(use-package clojure-mode-extra-font-locking
  :if esk-clojure
  :ensure t
  :defer t
  :init (after clojure-mode (require 'clojure-mode-extra-font-locking nil t)))

(use-package cider
  :if esk-clojure
  :ensure t
  :defer t
  :functions (esk-cider-repl-redo-last-input)
  :preface
  (defun esk-cider-repl-redo-last-input ()
    (interactive)
    (save-window-excursion
      (cider-switch-to-default-repl-buffer)
      (cider-repl-previous-input)
      (cider-repl-return)))
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
    (bind-key "C-c C-t" 'cider-test-jump clojure-mode-map)
    (bind-key "C-c RET" 'esk-cider-repl-redo-last-input cider-mode-map)
    (setq cider-prompt-save-file-on-load nil
          cider-prompt-for-project-on-connect nil
          cider-prompt-for-symbol nil
          cider-show-error-buffer t
	  cider-auto-jump-to-error nil
	  nrepl-hide-special-buffers nil
	  cider-repl-use-pretty-printing t
	  cider-repl-history-file (user-var-file "nrepl-history"))
    (use-package cider-repl
      :defer t
      :config
      (progn
	(add-hook 'cider-repl-mode-hook 'company-mode)
        (add-hook 'cider-repl-mode-hook (turn-on #'lisps-mode))
	(setq cider-repl-pop-to-buffer-on-connect nil)))))

(use-package clj-refactor
  :if esk-clojure
  :ensure t
  :defer t
  :diminish clj-refactor-mode
  :init (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (progn
    (cljr-add-keybindings-with-prefix "C-c C-m")
    (use-package cljr-helm
      :ensure t
      :defer t
      :init
      (after clojure-mode
        (bind-key "C-c <RET> h" 'clojure-mode-map)))))

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
  :init (after sql (require 'sql-indent))
  :config
  (progn
    (add-hook 'sql-mode-hook 'esk-savage-indent-mode)
    (after paredit (add-hook 'sql-mode-hook 'paredit-mode))
    (setq sql-indent-offset 2
          sql-indent-first-column-regexp
          (concat "\\(^\\s-*"
                  (regexp-opt '("with" "window" "inner" "left" "outer" "right"
                                "select" "update" "insert" "delete"
                                "union" "intersect"
                                "from" "where" "into" "group" "having" "order"
                                "set"
                                "create" "drop" "truncate"
                                "--"
                                ) t)
                  "\\(\\b\\|\\s-\\)\\)\\|\\(^```$\\)"))))

(use-package sql
  :defer t
  :config
  (setq sql-product 'postgres
        sql-send-terminator t))

(use-package sqlup-mode
  :ensure t
  :defer t
  :init (add-hook 'sql-mode-hook 'sqlup-mode))

(use-package edbi :if esk-sql :ensure t :defer t)

;; ** OCaml

(use-package opam
  :if esk-ocaml
  :ensure t
  :defer t
  :init (after tuareg (opam-init)))

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
  :commands utop-setup-ocaml-buffer
  :init (after tuareg (add-hook 'tuareg-mode-hook 'utop-minor-mode))
  :config
  (progn
    (after paredit
      (add-hook 'utop-mode-hook 'paredit-mode)
      (add-hook 'utop-mode-hook  #'(lambda () (ocaml-bind-paredit utop-mode-map))))))

(use-package ocp-indent
  :disabled t
  :if esk-ocaml
  :ensure t
  :defer t
  :init (after tuareg (require 'ocp-indent nil t)))

(use-package merlin
  :if esk-ocaml
  :ensure t
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
  :defer t
  :ensure t
  :init (after eshell (require 'eshell-z nil t)))

;; * Org Mode

(use-package org
  :if esk-org
  :ensure org
  :defer t
  :diminish orgstruct-mode
  :commands (turn-on-orgstruct)
  :init (add-hook 'prog-mode-hook 'turn-on-orgstruct)
  :config
  (progn
    (setq org-catch-invisible-edits 'smart
          org-hide-leading-stars t)
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
  :init
  (after org
    (add-hook 'org-mode-hook 'worf-mode)))

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
  :config
  (setq url-configuration-directory (user-var-file "url/")))

;; * Keybindings

(load-file (expand-file-name "keybindings.el" user-emacs-directory))

;; * Documentation

(use-package know-your-http-well :ensure t :defer t)

(provide 'init)
;; init.el ends here
