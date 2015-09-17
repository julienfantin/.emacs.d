
;; ---------------------------------------------------------------------
;; Defaults

(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq mouse-wheel-progressive-speed nil
      scroll-step 1
      scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 'always)

(progn
  (setq-default locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq-default default-truncate-lines t)
  (setq-default truncate-lines t)
  (setq-default confirm-nonexistent-file-or-buffer nil)
  (setq-default gc-cons-threshold (* 32 1024 1024))
  (setq-default completion-styles '(basic partial-completion substring))
  (setq-default completion-cycle-threshold t))

;; ---------------------------------------------------------------------
;; Windows etc

(use-package workgroups2
  :ensure workgroups2
  :init (progn
          (setq wg-prefix-key (kbd "C-c z"))
          (workgroups-mode 1)))

(use-package owdriver
  :pre-laod
  (progn
    (global-unset-key (kbd "M-o")))
  :init
  (progn
    (setq owdriver-prefix-key "M-o")
    (owdriver-config-default)
    (owdriver-mode 1))
  :config
  (progn
    (global-set-key (kbd "M-h") 'owdriver-do-scroll-right)
    (global-set-key (kbd "M-j") 'owdriver-do-scroll-up)
    (global-set-key (kbd "M-k") 'owdriver-do-scroll-down)
    (global-set-key (kbd "M-l") 'owdriver-do-scroll-left)))

(use-package shackle
  :ensure shackle
  :disabled t
  :init (shackle-mode 1)
  :config
  (progn
    (setq shackle-default-ratio 0.168)
    (setq shackle-rules
          '(("*helm.+" :regexp t :align 'below :defer t)))))

;; ---------------------------------------------------------------------
;; Keymaps


(use-package hydra
  :ensure hydra)

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

(use-package mykie
  :ensure mykie
  :idle (mykie:initialize))

;; ---------------------------------------------------------------------
;; External programs

(use-package launch
  :ensure launch
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
          (launch-directory default-directory))))))

(use-package prodigy
  :ensure prodigy
  :bind ("C-x p" . prodigy)
  :config
  (progn
    ;;; Tags
    ;;
    (prodigy-define-tag :name 'lein-repl
                        :commands '("lein" "repl" ":headless")
                        :nrepl-host "127.0.0.1"
                        :nrepl-port "56789"
                        :ready-message "^nREPL server started on port.+"
                        :on-output (lambda (service output)
                                     (message output)
                                     (when (s-matches? (plist-get service :ready-message))
                                       (cider (plist-get service :nrepl-host)
                                              (plist-get service :nrepl-port))
                                       (prodigy-set-status service 'ready))))

    (prodigy-define-tag :name 'lein-ring
                        :tags '(lein-repl)
                        :commands '("lein" "ring" "server-headless")
                        :ready-message "^Started server on port.+")

    ;;; Services
    ;;
    (prodigy-define-service :name "Redis"
                            :command "redis-server"
                            :kill-process-buffer-on-stop t
                            :ready-message ".+The server is now ready to accept connections.+")

    (prodigy-define-service :name "SQL server."
                            :commands '("mysql.server" "start")
                            :ready-message ".+SUCCESS!")

    (prodigy-define-service :name "Beta api"
                            :cwd "~/projects/beta-api/api/"
                            :tags '(lein-ring))

    (prodigy-define-service :name "Beta importer"
                            :tags '(lein-repl)
                            :cwd "~/projects/beta-api/importer/"
                            :args '(":port 56788")
                            :nrepl-port "56788")

    (prodigy-define-service
     :name "Beta web"
     :cwd "~/projects/beta-api/web/"
     :commads '("lein" "repl" ":headless" ":port" "56789"))))

(use-package navorski
  :ensure navorski
  :init
  (progn
    (nav/defterminal
     beta-api
     :interactive t
     :buffer-name "*beta-api console*"
     :init-script ("cd ~/projects/beta-api" "lein ring server-headless"))))

;; ---------------------------------------------------------------------
;; Minibuffer

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq gc-cons-threshold 20000000)
    (setq projectile-completion-system 'ido)
    (use-package flx-ido
      :ensure flx-ido
      :init (flx-ido-mode 1)
      :config (setq ido-use-faces nil))))

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

;; ---------------------------------------------------------------------
;; Mail

(use-package mu4e
  :config
  (progn
    (add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e")
    (setq mu4e-drafts-folder "/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/[Gmail].Trash")

    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; setup some handy shortcuts
    ;; you can quickly switch to your Inbox -- press ``ji''
    ;; then, when you want archive some messages, move them to
    ;; the 'All Mail' folder by pressing ``ma''.
    (setq mu4e-maildir-shortcuts
          '( ("/INBOX"               . ?i)
             ("/[Gmail].Sent Mail"   . ?s)
             ("/[Gmail].Trash"       . ?t)
             ("/[Gmail].All Mail"    . ?a)))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "offlineimap")

    ;; something about ourselves
    (setq
     user-mail-address "julienfantin@gmail.com"
     user-full-name  "Julien Fantin"
     message-signature "- Julien")

    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu

    (require 'smtpmail)
    ;; alternatively, for emacs-24 you can use:
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)
    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)))

;; ---------------------------------------------------------------------
;; News

(use-package elfeed
  :ensure elfeed)

;; ---------------------------------------------------------------------
;; Editing

(use-package evil
  :ensure evil
  :init (evil-mode 1)
  :config
  (progn
    (use-package evil-paredit
      :ensure evil-paredit
      :config (add-hook 'prog-mode-hook 'evil-paredit-mode))))

(use-package smart-forward
  :bind (("M-<right>" . smart-forward)
         ("M-<left>" . smart-backward)
         ("M-<down>" . smart-down)
         ("M-<up>" . smart-up)))

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

(use-package ace-isearch
  :disabled t
  :ensure ace-isearch
  :init (global-ace-isearch-mode 1))


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




















Eastwood, too noisy for now...
(after cider
  (defvar eastwood-add-linters
    (vector
     ;; :keyword-typos
     ;;:unused-namespaces
     :unused-fn-args)
    "Really doesn't make sense to use a vector here,
      but saves the formatting for now...")

  (flycheck-define-checker eastwood
    "A Clojure lint tool."
    :command
    ("lein" "eastwood"
     (eval
      (format "{:namespaces [%s] :add-linters []}" (cider-find-ns) eastwood-add-linters)))
    :error-patterns
    ((error
      bol
      "{:linter" (one-or-more not-newline) ",\n"
      " :msg" (or (zero-or-one (syntax whitespace)) (zero-or-one "\n")) (message) ",\n"
      " :line " line ",\n"
      " :column " column "}" line-end))
    :modes clojure-mode)
  (add-to-list 'flycheck-checkers 'eastwood))

;; Terminal stuff

;; Paredit fixes
(define-key input-decode-map "\M-[1;5A" [C-up])
(define-key input-decode-map "\M-[1;5B" [C-down])
(define-key input-decode-map "\M-[1;5C" [C-right])
(define-key input-decode-map "\M-[1;5D" [C-left] [D])

(define-key input-decode-map "\M-[1;3A" [M-up])
(define-key input-decode-map "\M-[1;3B" [M-down])
(define-key input-decode-map "\M-[1;3C" [M-right])
(define-key input-decode-map "\M-[1;3D" [M-left])

(define-key input-decode-map "\M-[1;9A" [M-up])
(define-key input-decode-map "\M-[1;9B" [M-down])
(define-key input-decode-map "\M-[1;9C" [M-right])
(define-key input-decode-map "\M-[1;9D" [M-left])

;; ---------------------------------------------------------------------
;; Purpose

(use-package purpose
  :ensure purpose
  :init
  (setq purpose-user-mode-purposes
	'((term-mode . terminal)
	  (shell-mode . terminal)
	  (ansi-term-mode . terminal)
	  (tuareg-mode . coding)
	  (clojure-mode . coding)
	  (cider-repl-mode . clojure-repl)
	  (css-mode . coding)
	  (html-mode . coding)
	  (compilation-mode . messages)))
  :config
  (purpose-mode 1)
  (purpose-compile-user-configuration))

(setf company-idle-delay 0
      company-minimum-prefix-length 2
      company-show-numbers t
      company-selection-wrap-around t
      company-dabbrev-ignore-case t
      company-dabbrev-ignore-invisible t
      company-dabbrev-downcase nil
      company-backends (list #'company-css
			     #'company-clang
			     #'company-capf
			     (list #'company-dabbrev-code
				   #'company-keywords)
			     #'company-files
			     #'company-dabbrev))
