(use-package multi-eshell
  :ensure multi-eshell)

(use-package workgroups2
  :ensure workgroups2
  :init (progn
          (setq wg-prefix-key (kbd "C-c z"))
          (workgroups-mode 1)))

(setq search-highlight t
      query-replace-highlight t)

(setq gc-cons-threshold (* 8 1024 1024))

(setq byte-compile--use-old-handlers nil)

(push '(width . 130) default-frame-alist)
(push '(height . 72) default-frame-alist)

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

(use-package popwin
  :ensure popwin
  :init (popwin-mode 1)
  :config
  (progn
    (setq display-buffer-alist '(("*+]" popwin:display-buffer)))
    (push '("*cider.*" :height 30 :regexp t) popwin:special-display-config)
    (push '("*helm.+" :height 20 :regexp t) popwin:special-display-config)
    (push '("*Warnings*" :height 30) popwin:special-display-config)
    (push '("*Messages*" :height 30) popwin:special-display-config)
    (push '("*Backtrace*" :height 30 :noselect t) popwin:special-display-config)
    (push '("*Compile-Log*" :height 30 :noselect t) popwin:special-display-config)))

(use-package evil
  :ensure evil
  :init (evil-mode 1)
  :config
  (progn
    (use-package evil-paredit
      :ensure evil-paredit
      :config (add-hook 'prog-mode-hook 'evil-paredit-mode))))

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

(setq-default dired-listing-switches "-alhv")

;; dired-hide-details-mode

;; sentence-highlight-mode
;; auto-capitalize

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


(use-package elfeed
  :ensure elfeed)

