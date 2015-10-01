;; Not sure why I don't need this anymore...
(use-package exec-path-from-shell
  :disabled t
  :ensure t
  :init (exec-path-from-shell-initialize))

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

;;; True believer
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

;; Need to figure out in-buffer navigation first
(use-package outline-magic :disabled t :ensure t :defer t)


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