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
  :init (after-init #'recentf-mode)
  :after no-littering
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 200))

(use-package autorevert
  :init (after-init #'global-auto-revert-mode)
  :custom (auto-revert-check-vc-info nil))

(use-package simple
  :custom
  (save-interprogram-paste-before-kill t)
  (create-lockfiles nil)
  (make-backup-files nil))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-auto-revert-buffer t))

(use-package dired-k
  :disabled t
  :ensure t
  :config
  (progn
    (add-hook 'dired-initial-position-hook 'dired-k)
    (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))

(use-package ls-lisp
  :disabled t
  :after dired
  :config
  (validate-setq ls-lisp-use-insert-directory-program t))


;; * Packages

(use-package no-littering
  :ensure t
  :demand t
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  :config
  (after 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)))

(use-package vlf-setup :ensure vlf)

(use-package super-save
  :ensure t
  :init (after-init #'super-save-mode)
  :config
  (progn
    (defun config-files--create-buffer-file-parent-directories ()
      (when buffer-file-name
        (let ((dir (file-name-directory buffer-file-name)))
          (when (and (not (file-exists-p dir))
                     (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
            (make-directory dir t)))))
    (setq-default save-silently t)
    (add-to-list 'super-save-triggers "ace-window")
    (add-to-list 'super-save-triggers "eyebrowse-switch-to-window-config")
    (add-to-list 'super-save-triggers "persp-switch")
    (add-to-list 'super-save-triggers "completing-read")
    (add-to-list 'super-save-triggers "ivy--read")
    (add-hook 'before-save-hook 'config-files--create-buffer-file-parent-directories)))

(use-package dired-hacks-utils
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        (("n" . dired-hacks-next-file)
         ("p" . dired-hacks-previous-file))))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))

(use-package dired-narrow
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

(use-package dired-x
  :disabled t
  :after dired
  :config (setq-default dired-omit-files-p t)
  :custom
  (dired-listing-switches "-alh")
  (dired-omit-files "^\\.\\|^#.#$\\|.~$"))


;; * Commands

(defun -find-file-as-sudo ()
  "Find file with sudo.  Default to function `buffer-file-name'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (find-alternate-file (concat "/sudo::" file-name))))

(provide 'config-files)
;;; config-files.el ends here
