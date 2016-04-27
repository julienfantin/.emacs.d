;; Unusably slow with emacs head on osx
(use-package shackle
  :ensure t
  :init (after-init (turn-on 'shackle-mode))
  :config
  (setq shackle-default-alignment 'below
	shackle-rules
	'(("*cider-error*" :align t)
	  ("\\*cider-repl.+" :regexp t :align 'below :ratio 0.4)
	  ("\\*magit.+" :regexp t :same t :inhibit-window-quit t))))

;; lambda prompt is cute but not read-only'
(use-package eshell-prompt-extras
  :ensure eshell-prompt-extras
  :config
  (setq eshell-highlight-prompt nil
	eshell-prompt-function 'epe-theme-lambda))

;;; True believer!
(use-package evil
  :ensure t
  :disabled t
  :init (after-init (turn-on 'evil-mode))
  :config
  (progn
    (setcdr evil-insert-state-map nil)
    (bind-keys :map evil-insert-state-map
               ([escape] . evil-normal-state)
               ("C-g"    . evil-normal-state))))
(use-package evil-leader
  :ensure t
  :disabled t
  :init (global-evil-leader-mode)
  :config
  (progn
    (setq evil-leader/leader "SPC")
    (evil-leader/set-key "TAB" 'esk-alternate-buffer)))


;; Buggy
(use-package auto-save-buffers-enhanced
  :ensure t
  :defer t
  :disabled t
  :commands auto-save-buffers-enhanced
  :init (after-init (turn-on 'auto-save-buffers-enhanced))
  :config
  (setq auto-save-buffers-enhanced-quiet-save-p t
        auto-save-buffers-enhanced-exclude-regexps '("^.+\\.cljs" "^.+\\.cljc")))

;; Not really using this
(use-package hardhat
  :ensure t
  :defer t
  :init
  (after-init (turn-on 'global-hardhat-mode)))

;; Not finding this usefule
(use-package volatile-highlights
  :disabled t
  :ensure t
  :defer t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :init (add-hook 'prog-mode-hook 'volatile-highlights-mode))

;; Have yet to use typed-clojure for something...
(use-package typed-clojure-mode
  :ensure t
  :disabled t
  :init (add-hook 'clojure-mode-hook 'typed-clojure-mode)
  :config
  (progn
    (defun typed-clojure-font-lock ()
      (font-lock-add-keywords
       nil
       '(("(\\(def\\(record\\|protocol\\)>\\)\\s-+\\(\\w+\\)"
          (1 font-lock-keyword-face)
          (3 font-lock-function-name-face)))))
    (add-hook 'clojure-mode-hook 'typed-clojure-font-lock)))

;; Seems usefule but confusing setup and UX
(use-package org-projectile
  :ensure t
  :disabled t
  :init
  (after org-capture
         (require 'org-projectile)
         (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p")))
  :config
  (setq org-projectile:projects-file "~/projects/TODOs.org"
        org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

;; Causes some issues with projectile
(use-package vagrant-tramp
  :disabled t
  :ensure t
  :defer t
  :commands vagrant-tramp-enable
  :init
  (progn
    ;; TODO Projectile ends up trying to use the local shell
    (setq shell-file-name "/bin/bash")
    (after tramp (vagrant-tramp-enable))))

;; Haven't found some useful bindings for this
(use-package paxedit
  :ensure t
  :defer t
  :diminish paxedit-mode
  :commands paxedit-mode
  :init (add-hook 'lisps-mode-hook 'paxedit-mode)
  :config
  (defhydra paxedit-hydra (:color red)
    ("u" paxedit-backward-up)
    ("e" paxedit-backward-end)
    ("f" paxedit-transpose-forward)
    ("b" paxedit-transpose-backward)
    ("w" paxedit-copy)
    ("k" paxedit-kill)
    ("d" paxedit-delete)
    ("r" paxedit-sexp-raise)
    ("c" paxedit-symbol-change-case)
    ("w" paxedit-symbol-copy)
    ("k" paxedit-symbol-kill))
  (bind-key "C-`" 'paxedit-hydra/body lisps-mode-map))

;; Visually noisy and not that useful with auto-indentation
(use-package hl-sexp
  :disabled t
  :ensure t
  :defer t
  :commands hl-sexp-mode
  :init (add-hook 'prog-mode-hook 'hl-sexp-mode))

;; Lispy does pretty much all of this in a more consistent way?
(use-package paxedit
  :disabled t
  :ensure t
  :defer t
  :after lisp-minor-mode
  :init (add-hook 'lisp-minor-mode-hook #'paxedit-mode)
  :config
  (progn
    (unbind-key "C-M-u"      paredit-mode-map)
    (bind-keys
     :map paxedit-mode-map
     ("C-M-p" . paxedit-transpose-backward)
     ("C-M-n" . paxedit-transpose-forward)
     ("C-M-u" . paxedit-backward-up)
     ("C-M-d" . paxedit-backward-end)
     ("M-F"   . paxedit-next-symbol)
     ("M-B"   . paxedit-previous-symbol)
     ("C-%"   . paxedit-copy)
     ("C-*"   . paxedit-delete)
     ;; ("C-w"   . paxedit-backward-kill)
     ;; ("M-w"   . paxedit-forward-kill)
     ("C-&"   . paxedit-kill)
     ("C-^"   . paxedit-sexp-raise)
     ("M-u"   . paxedit-symbol-change-case)
     ("c-@"   . paxedit-symbol-copy)
     ("C-#"   . paxedit-symbol-kill))))

;; superseded by outorg?
(use-package poporg
  :ensure t
  :defer t
  :commands poporg-dwim
  :config (bind-key "C-c C-c" 'poporg-dwim poporg-mode-map))

;; don't get/like flx
(use-package company-flx
  :disabled t
  :ensure t
  :defer t
  :init (after company (company-flx-mode 1)))

;; haven't used that in a long time!
(use-package yasnippet
  :disabled t
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

;; bit confused
(use-package auto-yasnippet
  :disabled t
  :ensure t
  :defer t
  :commands aya-open-line)

;; Better to make use of the super key
(defun disable-key-chord-mode ()
  (set (make-local-variable 'input-method-function) nil))
(add-hook 'minibuffer-setup-hook #'disable-key-chord-mode)

(use-package space-chord
  :init (key-chord-mode 1)
  :config
  (progn
    (setq space-chord-delay 0.04)
    (space-chord-define-global ?s 'save-buffer)
    (space-chord-define-global ?i 'imenu-anywhere)
    (space-chord-define-global ?w 'avy-goto-word-1)
    (space-chord-define-global ?c 'avy-goto-char)
    (space-chord-define-global ?g 'avy-goto-line)
    (space-chord-define-global ?l 'link-hint-open-link)
    (space-chord-define-global ?o 'helm-org-in-buffer-headings)
    ;; M-g M-n
    ;; (space-chord-define-global ?n 'flycheck-next-error)
    ;; (space-chord-define-global ?p 'flycheck-previous-error)
    ;; C-c ! l
    ;; (space-chord-define-global ?e 'esk-flycheck-list-errors)
    (space-chord-define-global ?b 'helm-bookmarks)
    (space-chord-define-global ?f 'helm-find-files)

    (space-chord-define-global ?m 'helm-all-mark-rings)
    (space-chord-define-global ?i 'helm-imenu-anywhere)
    (space-chord-define-global ?/ 'swiper-helm)))

;; that was just a bad idea...
(use-package sqlup-mode
  :disabled t
  :if esk-sql
  :ensure t
  :defer t
  :init
  (progn
    (after sql  (add-hook 'sql-mode-hook 'sqlup-mode))
    (after edbi (add-hook 'edbi:sql-mode-hook 'sqlup-mode))))


;; Helm completion is distracting and input history not that useful
(use-package helm-eshell
  :disabled t
  :init
  (after esh-mode
    (bind-key [remap eshell-pcomplete] #'helm-esh-pcomplete eshell-mode-map)
    (bind-key [remap eshell-previous-matching-input-from-input] #'helm-eshell-history eshell-mode-map)))

;; Can use helm M-x or descbinds
(use-package cljr-helm
  :ensure t
  :defer t
  :init
  (after clojure-mode
    (bind-key "C-c C-. h" 'cljr-helm)))

;; Seems to make things sluggish and can't really tell what it's doing...
(use-package auto-indent-mode
  :disabled t
  :ensure t
  :defer t
  :config
  (setq auto-indent-indent-style 'aggressive
        auto-indent-blank-lines-on-move nil
        auto-indent-current-pairs nil))

(setq  scroll-margin 5
       scroll-conservatively 10000)


(use-package helm-elscreen
  :defer t
  :commands helm-elscreen
  :config
  (progn
    (defvar helm-better-source-elscreen
      (helm-build-dummy-source
       "Create new screen"
       :action (helm-make-actions
                "Create new screen (C-u to clone current)"
                (lambda (candidate)
                  (let ((screen (if helm-current-prefix-arg (elscreen-clone) (elscreen-create-internal))))
                    (elscreen-set-screen-nickname screen candidate)
                    (elscreen-goto screen)))))
      "Dummy elscreen helm source allowing to create new screens")
    (defun helm-elscreen ()
      (interactive)
      (helm-other-buffer
       '(helm-source-elscreen-history-list
         helm-source-elscreen-list
         helm-better-source-elscreen)
       "*Helm ElScreen*"))))


;; * persp-mode

(use-package persp-mode
  :disabled t
  :ensure t
  :defer t
  :init (after-init #'persp-mode)
  :config
  (with-eval-after-load 'desktop
    (add-hook
     'desktop-read
     (lambda ()
       (let ((persp-save-dir desktop-dirname))
         (message (format "load persp %s %s" persp-save-dir (projectile-project-name)))
         (persp-load-state-from-file (projectile-project-name)))))
    (add-hook
     'desktop-save
     (lambda ()
       (let ((persp-save-dir desktop-dirname))
         (message (format "save persp %s %s" persp-save-dir (projectile-project-name))))))))

(use-package persp-projectile
  :disabled t
  :ensure t
  :defer t
  :init
  (after projectile
         (require 'persp-projectile nil t)
         (persp-mode 1)))
