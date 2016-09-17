;;; config-files.el --- Files handling configuration
;;; Commentary:
;;
;;; Code:
(require 'use-config)
(require 'config-path)


;; * Defaults

(setq-default delete-by-moving-to-trash t)
(setq-default default-major-mode 'text-mode)
(setq-default large-file-warning-threshold 1000000)


;; * Builtins

(use-package recentf
  :defer t
  :init (after-init #'recentf-mode)
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 200))

(use-package tramp-cache
  :defer t
  :config
  (setq tramp-persistency-file-name (user-var-file "tramp")))

(use-package autorevert
  :defer t
  :init (after-init #'global-auto-revert-mode)
  :config (setq auto-revert-check-vc-info nil))

(use-package simple
  :defer t
  :config
  (progn
    (setq save-interprogram-paste-before-kill t
          create-lockfiles nil
          make-backup-files nil)))

(use-package dired
  :defer t
  :config
  (progn
    (setq dired-auto-revert-buffer t)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(use-package ls-lisp
  :after dired
  :config
  (setq ls-lisp-use-insert-directory-program t))


;; * Packages

(use-package vlf-setup :ensure vlf)

(use-package super-save
  :ensure t
  :defer t
  :init (after-init #'super-save-mode)
  :config
  (progn
    (setq-default save-silently t)
    (add-to-list 'super-save-triggers "ace-window")
    (add-to-list 'super-save-triggers "eyebrowse-switch-to-window-config")
    (add-to-list 'super-save-triggers "persp-switch")
    (add-to-list 'super-save-triggers "completing-read")
    (add-to-list 'super-save-triggers "ivy--read")))

(use-package dired-hacks-utils
  :ensure t
  :bind
  (:map dired-mode-map
        (("n" . dired-hacks-next-file)
         ("p" . dired-hacks-previous-file))))

(use-package dired-subtree
  :ensure t
  :defer t
  :init
  (after 'dired
    (bind-keys
     :map dired-mode-map
     ("i" . dired-subtree-toggle))))

(use-package dired-narrow
  :ensure t
  :defer t
  :init
  (after 'dired
    (bind-keys
     :map dired-mode-map
     ("/" . dired-narrow-fuzzy))))

(use-package dired-x
  :after dired
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-listing-switches "-alhv"
        dired-omit-files "^\\.\\|^#.#$\\|.~$"))


;; * Commands

(defun -find-file-as-sudo ()
  "Find file with sudo.  Default to function `buffer-file-name'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (find-alternate-file (concat "/sudo::" file-name))))

(provide 'config-files)
;;; config-files.el ends here
