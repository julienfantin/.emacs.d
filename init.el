;;; emacs-swagger-kit --- Elisp goodness!
;;; Commentary:
;;; Code:
;;
;; * Bootstap

(require 'cl-lib)

(progn
  ;; Debug on error when loading
  (setq debug-on-error t)
  (add-hook 'after-init-hook 'toggle-debug-on-error))

;; ** Elpa & use-package

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

;; ** Config helpers

(defmacro after (mode &rest body)
  "`eval-after-load' `MODE', wrapping `BODY' in `progn'."
  (declare (indent defun))
  (let ((s (if (symbolp mode)
               (symbol-name mode)
             mode)))
    `(eval-after-load ,(symbol-name mode)
       (quote (progn ,@body)))))

;; * Defaults
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

(defvar preferred-fonts
  '("Consolas-13"
    "Ubuntu Mono-15"
    "Source Code Pro-11"
    "DejaVu Sans Mono-13"
    "Inconsolata-14"
    "Menlo-12"))

(progn
  (set-frame-font
   (cl-find-if
    (lambda (f) (find-font (font-spec :name f)))
    preferred-fonts)
   t t))

;; ** GUI

(progn
  (setq initial-scratch-message "")
  (setq inhibit-startup-message t)
  (when window-system
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (setq ns-use-native-fullscreen nil
          use-file-dialog nil
          use-dialog-box nil)))

;; ** UI

(progn
  (setq-default cursor-type 'bar)
  (setq ring-bell-function (lambda ()))
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq save-interprogram-paste-before-kill t))

(use-package diminish
  :ensure diminish)

(use-package hl-line
  :pre-load
  (progn
    (defvar hl-line-hooks
      '(dired-mode-hook))
    (defun hl-line-turn-on ()
      (when (not hl-line-mode)
        (hl-line-mode 1))))
  :init
  (progn
    (dolist (hook hl-line-hooks)
      (add-hook hook 'hl-line-turn-on))))

;; ** Theme

(defvar dark-themes
  '(ample
    gruvbox
    monokai
    spolsky
    twilight-anti-bright))

(defvar light-themes
  '(twilight-bright
    ample-light
    mccarthy))

(use-package solarized-theme
  :disabled t
  :ensure solarized-theme
  :init
  (progn
    (setq solarized-height-minus-1 0.95
          solarized-height-plus-1 1.03
          solarized-height-plus-2 1.06
          solarized-height-plus-3 1.09
          solarized-height-plus-4 1.12)
    (set-face-attribute 'mode-line nil :foreground "#fdf6e3" :background "#268bd2")
    (set-face-attribute 'modeline-buffer-id nil :foreground "#268bd2" :background "#fdf6e3")))

(use-package rainbow-mode
  :ensure rainbow-mode
  :diminish (rainbow-mode "")
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; ** Minibuffer completion

(use-package helm-config
  :ensure helm
  :pre-load (setq helm-command-prefix-key "s-SPC")
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ;;("C-x C-f" . helm-find-files)
         ("C-h a" . helm-apropos))
  :config
  (progn
    (use-package helm-command
      :config
      (progn
        (setq helm-M-x-always-save-history t)
        (use-package helm-descbinds
          :ensure helm-descbinds
          :init (bind-key "d" 'helm-descbinds helm-command-map))))

    (use-package helm-mode
      :diminish ""
      :init (helm-mode 1)
      :config
      (progn
        ;; Complete immendiately on TAB when finding files
        (bind-key "TAB" 'helm-execute-persistent-action helm-read-file-map)
        (setq helm-ff-file-name-history-use-recentf t
              helm-ff-auto-update-initial-value t
              helm-ff-skip-boring-files t)
        (add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
        (setq helm-completing-read-handlers-alist
              '((describe-function . helm-completing-read-symbols)
                (describe-variable . helm-completing-read-symbols)
                (debug-on-entry . helm-comleting-read-symbols)
                (find-function . helm-completing-read-symbols)
                (find-tag . helm-completing-read-with-cands-in-buffer)
                (find-file . ido)
                (ffap-alternate-file)
                (tmm-menubar)))))))

;; ** Keys

(progn
  (setq ns-command-modifier 'control
        ns-function-modifier 'super
        ns-control-modifier 'meta
        ns-option-modifier 'meta))

;; ** Files

(use-package files
  :init
  (progn
    (setq backup-by-copying t
          backup-directory-alist `((".*" . ,temporary-file-directory))
          auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))))

(use-package autorevert
  :init (global-auto-revert-mode 1))

;; * Ineractive commands

(use-package commands
  :demand t
  :commands cleanup-buffer-or-region
  :load-path "./lib"
  :init
  (progn
    (bind-key "C-c C-c" 'cleanup-buffer-or-region prog-mode-map)))


(use-package free-keys
  :ensure free-keys)

;; * Projects

(use-package projectile
  :ensure projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-completion-system 'helm-comp-read
          projectile-use-git-grep t
          projectile-remember-window-configs t
          projectile-mode-line-lighter " P"
          projectile-cache-file (temp-file "projectile.cache"))

    (add-to-list 'projectile-globally-ignored-files ".DS_Store")

    (defadvice projectile-replace
        (before projectile-save-all-and-replace activate)
      (save-some-buffers t))

    (use-package helm-projectile
      :ensure helm-projectile
      :init
      (after helm-config
        (bind-key "p" 'helm-projectile helm-command-map)))))

;; ** Version control

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

;; * Editing

(progn
  (pending-delete-mode 1)
  (setq-default indent-tabs-mode nil))

(use-package undo-tree
  :ensure undo-tree
  :diminish ""
  :config
  (progn
    (global-undo-tree-mode 1)
    (setq
     undo-tree-visualizer-timestamps nil
     undo-tree-visualizer-diff nil
     undo-tree-auto-save-history t
     undo-tree-history-directory-alist `((".+" . ,(file-name-as-directory
                                                   (temp-file
                                                    ".undo-tree-history")))))))

;; * Markdown

(use-package markdown-mode
  :ensure markdown-mode
  :mode "\\.md\\'")

;; * Org

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

;; * Navigation
;; ** Buffer

(use-package imenu
  :config
  (setq imenu-auto-rescan t))

(use-package imenu-anywhere
  :ensure imenu-anywhere)

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

(use-package highlight-symbol
  :ensure highlight-symbol
  :init (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  :config
  (progn
    (setq highlight-symbol-on-navigation-p t)
    (bind-key "C-%" 'highlight-symbol-query-replace highlight-symbol-nav-mode-map)))

(use-package outline
  :init (add-hook 'prog-mode-hook 'outline-minor-mode)
  :diminish (outline-minor-mode "")
  :config
  (progn
    (use-package outshine
      :ensure outshine
      :pre-load (setq-default outline-cycle-emulate-tab t)
      :init (add-hook 'outline-minor-mode-hook 'outshine-hook-function))))

(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq  save-place-file (temp-file "places"))))

;; ** Windows & frames

(bind-key "s-h" 'bury-buffer)

;; Scroll buffers in-window
(bind-key "s-b" 'previous-buffer)
(bind-key "s-n" 'next-buffer)

;; Navigate windows
(bind-key "s-;" 'previous-multiframe-window)
(bind-key "s-'" 'next-multiframe-window)

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

;; Too intrusive, can't get the buffer list to filter properly,
;; focuses on saving/restoring, but kinda buggy at that.
(use-package workgroups2
  :disabled t
  :ensure workgroups2
  :pre-load (setq wg-prefix-key (kbd "C-z"))
  :config
  (progn
    (setq wg-default-session-file (temp-file "workgroups.el")
          wg-buffer-auto-association 'strong)
    (bind-key "z" 'wg-switch-to-workgroup wg-prefixed-map)
    (workgroups-mode 1)))

;; Would actually be kinda neat if the buffers could be filtered, but
;; it doesn't seem like it could easily be hacked
(use-package elscreen
  :disabled t
  :config
  (progn
    (elscreen-start)
    (use-package helm-elscreen
      :init
      (progn
        (bind-key "h" 'helm-elscreen elscreen-map)))))

;; Maybe try it again...
(use-package persp-mode
  :disabled t
  :ensure persp-mode
  :init (persp-mode 1)
  :config
  (progn
    (setq persp-nil-name "emacs"
          persp-add-on-switch-or-display t
          persp-when-kill-switch-to-buffer-in-perspective t
          persp-save-dir temporary-file-directory)
    (use-package workgroups
      :ensure workgroups
      :config (setq wg-morph-on nil))))

(use-package windmove
  :init (windmove-default-keybindings)
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

;; * Search and replace
;; ** Buffer
;; ** Directory
;; * Programming

(use-package prog-mode
  :config
  (progn

    (defun disable-truncate-lines ()
      (toggle-truncate-lines -1))

    (add-hook 'prog-mode-hook 'disable-truncate-lines)
    
    (use-package fixmee
      :ensure fixmee
      :diminish ""
      :init (add-hook 'prog-mode-hook 'fixmee-mode))
    
    (use-package flycheck
      :ensure flycheck
      :init (add-hook 'prog-mode-hook 'flycheck-mode)
      :config
      (progn
        (setq flycheck-mode-line-lighter " *")
        (use-package flycheck-color-mode-line
          :ensure flycheck-color-mode-line
          :init (add-hook 'flycheck-mode-hook
                          'flycheck-color-mode-line-mode))
        (use-package helm-flycheck
          :ensure helm-flycheck
          :config
          (after helm
            (bind-key "!" 'helm-flycheck helm-command-map)))))

    (use-package eldoc
      :diminish ""
      :init (add-hook 'prog-mode-hook 'eldoc-mode))

    (use-package rainbow-delimiters
      :ensure rainbow-delimiters
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

    (use-package auto-complete
      :ensure auto-complete
      :diminish ""
      :init
      (progn
        (setq tab-always-indent 'complete)
        (defun ac-turn-on ()
          (setq completion-at-point-functions
                '(lambda () (auto-complete) (ac-expand-common)))
          (auto-complete-mode))
        (add-hook 'prog-mode-hook 'ac-turn-on))
      :config
      (progn
        (use-package auto-complete-config
          :init (ac-config-default)
          :config
          (progn
            (bind-key "C-s" 'ac-isearch ac-completing-map)
            (setq ac-auto-start 2
                  ac-delay 0.15
                  ac-quick-help-delay 1)))
        (use-package fuzzy
          :ensure fuzzy)
        (use-package popup
          :ensure popup)))

    (use-package yasnippet
      :ensure yasnippet
      :diminish (yas-minor-mode "")
      :init (add-hook 'prog-mode-hook 'yas-minor-mode)
      :config
      (progn
        (setq yas-snippet-dirs `(,(user-file "snippets")))
        (unbind-key "<tab>" yas-minor-mode-map)
        (bind-key "C-c TAB" 'yas-expand yas-minor-mode-map)))))

(use-package lispy
  :commands (lispy-define-key lispy-nav-turn-on)
  :pre-load
  (progn
    (defvar lispy-nav-map (make-sparse-keymap))
    (easy-mmode-define-minor-mode
     lispy-nav-mode "Lispy nav" nil nil lispy-nav-map)
    :init
    (progn
      (add-hook 'clojure-mode-hook 'lispy-nav-mode)
      (add-hook 'emacs-lisp-mode-hook 'lispy-nav-mode))
    :config
    (progn
      (require 'lispy)
      (defun lispy-escape ()
        (interactive)
        (insert " ")
        (backward-char))
      (lispy-define-key lispy-nav-map "x" 'lispy-eval)
      (lispy-define-key lispy-nav-map "X" 'lispy-eval-and-insert)
      (lispy-define-key lispy-nav-map "n" 'lispy-down)
      (lispy-define-key lispy-nav-map "p" 'lispy-up)
      (lispy-define-key lispy-nav-map "f" 'lispy-flow)
      (lispy-define-key lispy-nav-map "b" 'lispy-backward)
      (lispy-define-key lispy-nav-map "a" 'lispy-out-backward)
      (lispy-define-key lispy-nav-map "e" 'lispy-out-forward)
      (lispy-define-key lispy-nav-map "o" 'lispy-different)
      (lispy-define-key lispy-nav-map "M-n" 'lispy-move-down)
      (lispy-define-key lispy-nav-map "M-p" 'lispy-move-up)
      (lispy-define-key lispy-nav-map "M-SPC" 'lispy-mark)
      (lispy-define-key lispy-nav-map "SPC" 'lispy-escape))))

(use-package pulse
  :config
  (progn
    (defun pulse-line ()
      (interactive)
      (pulse-momentary-highlight-one-line (point) hl-line-face))

    (bind-key "C-<return>" 'pulse-line)
    
    (defun pulse-last-sexp ()
      (let ((pulse-delay 0)
            (pulse-iterations 6))
        (save-excursion
          (let ((end (point)))
            (backward-sexp)
            (pulse-momentary-highlight-region (point) end)))))

    (defmacro pulse-advise-command (command)
      `(defadvice ,command (around pulse-last-sexp activate)
         (pulse-last-sexp)
         ad-do-it))

    (pulse-advise-command eval-last-sexp)
    (pulse-advise-command eval-defun)
    (after cider
      (pulse-advise-command cider-eval-last-sexp)
      (pulse-advise-command cider-eval-last-expression))))

;; ** Indentation

(use-package auto-indent-mode
  :ensure auto-indent-mode
  :diminish ""
  :init (add-hook 'prog-mode-hook 'auto-indent-mode))

;; ** Structured editing

(use-package paren
  :init (add-hook 'prog-mode-hook 'show-paren-mode)
  :config
  (progn
    (setq show-paren-style 'mixed)
    (setq show-paren-delay 0.02)))

(use-package paredit
  :ensure paredit
  :diminish ""
  :init (add-hook 'prog-mode-hook 'paredit-mode))

;; ** Whitespace

(use-package whitespace-cleanup-mode
  :ensure whitespace-cleanup-mode
  :diminish (whitespace-cleanup-mode "")
  :init (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

;; ** Clojure

(use-package clojure-mode
  :ensure clojure-mode
  :config
  (progn
    (use-package clojure-test-mode
      :ensure clojure-test-mode
      :init (add-hook 'clojure-mode-hook 'clojure-test-mode))

    (progn
      (add-hook 'clojure-mode-hook 'subword-mode)
      (add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
      ;; Defun indent special forms and macros
      (put-clojure-indent 'defmulti 'defun)
      (put-clojure-indent 'defmethod 'defun)
      (put-clojure-indent 'defroutes 'defun))

    (use-package ac-nrepl
      :ensure ac-nrepl
      :init
      (after auto-complete
        (after cider
          (add-to-list 'ac-modes 'cider-repl-mode)
          (add-to-list 'ac-modes 'cider-mode)))
      :config
      (after cider
        (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
        (add-hook 'cider-mode-hook 'ac-nrepl-setup)
        (bind-key "C-c C-d" 'ac-nrepl-popup-doc cider-mode-map)))

    (use-package cider
      :ensure cider
      :config
      (progn
        ;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
        ;; (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
        (add-hook 'cider-repl-mode-hook 'paredit-mode)
        (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
        (setq cider-prompt-save-file-on-load nil
              nrepl-hide-special-buffers nil
              cider-auto-select-error-buffer t
              cider-repl-print-length 400
              cider-repl-history-file "~/.emacs.d/nrepl-history"
              cider-repl-popup-stacktraces t
              ;;cider-repl-use-clojure-font-lock t
              ;;cider-repl-use-pretty-printing t
              )))

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

;; ** Elisp

(defvar elisp-hooks '(emacs-lisp-mode-hook ielm-mode-hook))

(use-package lisp-mode
  :config
  (progn
    (bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)

    (use-package elisp-slime-nav
      :ensure elisp-slime-nav
      :diminish ""
      :config
      (progn
        (dolist (hook elisp-hooks)
          (add-hook hook 'turn-on-elisp-slime-nav-mode))
        (after popup
          (defun popup-documentation-at-point ()
            (interactive)
            (let* ((position (point))
                   (string-under-cursor (buffer-substring-no-properties
                                         (progn (skip-syntax-backward "w_") (point))
                                         (progn (skip-syntax-forward "w_") (point)))))
              (goto-char position)
              (popup-tip (ac-symbol-documentation (intern string-under-cursor)))))
          (bind-key "C-c C-d" 'popup-documentation-at-point elisp-slime-nav-mode-map))))))

(provide 'init)
;;; init.el ends here
