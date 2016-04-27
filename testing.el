
;; ---------------------------------------------------------------------
;; Purposes
;;
;; Clojure config
;; - clojure
;; - repl
;; - documentation
;; - project.clj
;; - logs
;;
;; Clojure(script)
;; - clojurescript
;;
;; ** Hippie expand

(use-package hippie-expand
  :disabled t ; WTF is going on with the crazy parens expansions??
  :defer t
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))
    (defun config-completion-hippie-remove-trailing-paren ()
      (if (and (bound-and-true-p paredit-mode)
               (equal (substring str -1) ")"))
          (progn (backward-delete-char 1) (forward-char))))
    (advice-add #'he-substitute-string :after #'config-completion-hippie-remove-trailing-paren)))


;; ** Packages

(use-package bm
  :ensure t
  :defines (config-marks-load)
  :init (after-init #'config-marks-load)
  :preface
  (defun config-marks-load ()
    (when (file-exists-p bm-repository-file)
      (bm-repository-load)))
  (defun config-marks-bm-save-all ()
    (bm-buffer-save-all)
    (bm-repository-save))
  :config
  (progn
    (setq-default bm-buffer-persistence t)
    (setq
     bm-repository-file (user-var-file "bm-repository")
     bm-highlight-style 'bm-highlight-only-fringe
     bm-cycle-all-buffers t)
    ;; Saving bookmarks:
    (add-hook 'kill-buffer-hook #'bm-buffer-save)
    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'config-marks-bm-save-all)
    (add-hook 'after-save-hook #'bm-buffer-save)
    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    ;; Restoring bookmarks:
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)
    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
    (define-fringe-bitmap
      'bm-marker-left
      [#xF8                             ; ▮ ▮ ▮ ▮ ▮ 0 0 0
       #xFC                             ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
       #xFE                             ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
       #x0F                             ; 0 0 0 0 ▮ ▮ ▮ ▮
       #x0F                             ; 0 0 0 0 ▮ ▮ ▮ ▮
       #xFE                             ; ▮ ▮ ▮ ▮ ▮ ▮ ▮ 0
       #xFC                             ; ▮ ▮ ▮ ▮ ▮ ▮ 0 0
       #xF8])                           ; ▮ ▮ ▮ ▮ ▮ 0 0 0
    ))

(use-package bookmark+
  :ensure t
  :defer t
  :commands (bmkp-desktop-jump))


;; ---------------------------------------------------------------------
;; Defaults

(progn
  (setq-default default-truncate-lines t)
  (setq-default truncate-lines t)
  (setq-default confirm-nonexistent-file-or-buffer nil)
  (setq-default gc-cons-threshold (* 80 1024 1024))
  (setq-default completion-styles '(basic partial-completion substring))
  (setq-default completion-cycle-threshold t))

;; ---------------------------------------------------------------------
;; Windows etc

(use-package workgroups2
  :ensure workgroups2
  :init (progn
          (setq wg-prefix-key (kbd "C-c z"))
          (workgroups-mode 1)))

;; ---------------------------------------------------------------------
;; Keymaps

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
     message-signature "― Julien")

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
          (eshell-mode . terminal)
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

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(eshell-smart-initialize)

(use-package popwin
  :ensure t
  :commands popwin-mode
  :init (after-init #'popwin-mode)
  :config
  (setq
   popwin:popup-window-position 'bottom
   helm-display-function 'popwin:pop-to-buffer
   popwin:special-display-config
   '(("\\*Navi.+" :regexp t :height 0.25)
     "*Warning*"
     "*Help*"
     "*Backtrace*"
     "*Compile-Log*"
     "*Buffer List*")))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)



;; Paxedit

;; C code

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(use-package workgroups2
  :ensure t
  :init (after-init #'workgroups-mode)
  :config
  (setq
   wg-mess-with-buffer-list nil
   wg-mode-line-use-faces nil
   wg-session-load-on-start nil))


(use-package project-persist
  :ensure t
  :init (after-init #'project-persist-mode)
  :preface
  (progn
    ;; Projectile integration
    ;;
    ;; Use projectile for project navigation and make sure we have a persistent
    ;; project available on demand
    (defun esk-project-persist-projectile-open-hook ()
      (after projectile
        (message (format">> syncing pp: %s %s" (projectile-project-root) (projectile-project-name)))
        (let ((root-dir (projectile-project-root))
              (name (projectile-project-name)))
          (if (project-persist--project-exists name)
              (progn
                (message (format ">> open pp:" name))
                (project-persist--project-open name))
            (progn
              (message (format ">> create pp:" name))
              (condition-case err
                  (progn
                    (project-persist--project-setup root-dir name)
                    (project-persist--project-open name))
                (error (project-persist--signal-error err))))))))
    (defun esk-project-persist-projectile-close-hook ()
      (after projectile
        (when-let ((name (projectile-project-name)))
          (when (project-persist--has-open-project)
            (project-persist-save)))))
    ;; Workgroups integration
    ;;
    ;; Load and save sessions as we open and close persistent projects
    (defun esk-project-persist-workgroups-file ()
      (expand-file-name
       "workgroups"
       (project-persist--settings-dir-from-name project-persist-current-project-name)))
    (defun esk-project-persist-create-workgroups ()
      (wg-create-workgroup project-persist-current-project-name))
    (defun esk-project-persist-load-workgroups ()
      (message (format">> loading wg: %s" (esk-project-persist-workgroups-file)))
      (if-let ((file (esk-project-persist-workgroups-file)))
          (wg-open-session file)
        (message "Load workgroups: No workgroups found")))
    (defun esk-project-persist-save-workgroups ()
      (message (format">> saving wg: %s" (esk-project-persist-workgroups-file)))
      (if-let ((file (esk-project-persist-workgroups-file)))
          (wg-save-session-as file nil)
        (message "Save workgroups: No workgroups found"))))
  :config
  (progn
    (setq project-persist-settings-dir (user-var-file "project-persist"))
    ;; Projectile
    (add-hook 'projectile-after-switch-project-hook #'esk-project-persist-projectile-open-hook)
    (add-hook 'projectile-before-switch-project-hook #'esk-project-persist-projectile-close-hook)
    ;; Workgroups
    (add-hook 'project-persist-after-create-hook #'esk-project-persist-create-workgroups)
    (add-hook 'project-persist-after-load-hook #'esk-project-persist-load-workgroups)
    (add-hook 'project-persist-after-save-hook #'esk-project-persist-save-workgroups)))




;; Project shitshow

(use-package project-persist
  :ensure t
  :init (after-init #'project-persist-mode)
  :config
  (progn
    (add-hook 'project-persist-before-create-hook
              #'(lambda () (message (format "======= %s" 'project-persist-before-create-hook))))
    (add-hook 'project-persist-after-create-hook
              #'(lambda () (message (format "======= %s" 'project-persist-after-create-hook))))
    (add-hook 'project-persist-before-save-hook
              #'(lambda () (message (format "======= %s" 'project-persist-before-save-hook))))
    (add-hook 'project-persist-after-save-hook
              #'(lambda () (message (format "======= %s" 'project-persist-after-save-hook))))
    (add-hook 'project-persist-before-load-hook
              #'(lambda () (message (format "======= %s" 'project-persist-before-load-hook))))
    (add-hook 'project-persist-after-load-hook
              #'(lambda () (message (format "======= %s" 'project-persist-after-load-hook))))
    (add-hook 'project-persist-before-close-hook
              #'(lambda () (message (format "======= %s" 'project-persist-before-close-hook))))
    (add-hook 'project-persist-after-close-hook
              #'(lambda () (message (format "======= %s" 'project-persist-after-close-hook))))

    (setq project-persist-settings-dir (user-var-file "project-persist"))

    ;; (defun esk-project-after-create ()
    ;;   ;; (message (format"====== esk-project-after-create %s" (project-persist--settings-get 'name)))
    ;;   ;; (desktop+-create-auto ;; (project-persist--settings-get 'name)
    ;;   ;;  )
    ;;   )

    ;; (defun esk-project-after-load ()
    ;;   (message
    ;;    (format "======= esk-project-after-load %s" (project-persist--settings-get 'name)))
    ;;   (let* ((project-name (project-persist--settings-get 'name))
    ;;          (dirname (desktop+--dirname project-name)))
    ;;     (if (file-exists-p dirname)
    ;;         (desktop+-load dirname)
    ;;       (desktop+-create dirname))))

    ;; (defun esk-project-before-close ()
    ;;   (message
    ;;    (format "====== esk-project-before-close %s" (project-persist--settings-get 'name)))
    ;;   (save-some-buffers t))

    ;; (defun esk-project-persist-file (&optional filename)
    ;;   (assert (project-persist--settings-get 'name))
    ;;   (let* ((project-name (project-persist--settings-get 'name))
    ;;          (settings-dir (project-persist--settings-dir-from-name project-name)))
    ;;     (if (null filename)
    ;;         settings-dir
    ;;       (let ((f (expand-file-name filename settings-dir)))
    ;;         (when (directory-name-p f) (make-directory f t))
    ;;         f))))

    ;; (defun esk-project-root ()
    ;;   (cond
    ;;    ((project-persist--has-open-project) project-persist-current-project-root-dir)
    ;;    ((projectile-project-p) (projectile-project-root))
    ;;    (t default-directory)))

    ;; (defun esk-project-name (&optional dir)
    ;;   (let ((project-root (or dir (esk-project-root))))
    ;;     (cond
    ;;      ((directory-name-p project-root) (file-name-nondirectory (directory-file-name project-root)))
    ;;      (t (file-name-nondirectory project-root)))))

    ;; (add-to-list 'project-persist-before-close-hook #'esk-project-before-close)
    ;; (add-to-list 'project-persist-after-create-hook #'esk-project-after-create)
    ;; (add-to-list 'project-persist-after-load-hook #'esk-project-after-load t)


    ;; Projectile integration
    ;;
    ;; (after projectile


    ;;   (setq projectile-switch-project-action #'helm-projectile))
    ))
