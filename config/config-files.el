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
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes ' always))

(use-package dired-k
  :ensure t
  :after dired
  :hook
  (dired-initial-position . dired-k)
  (dired-after-readin     . dired-k-no-revert)
  :custom
  (dired-k-style nil)
  (dired-listing-switches "-alh")
  (dired-k-human-readable t))

(use-package files
  :preface
  (defun config-files--create-buffer-file-parent-directories ()
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
          (make-directory dir t)))))
  :hook
  ((before-save-hook . config-files--create-buffer-file-parent-directories)
   (after-save-hook  . executable-make-buffer-file-executable-if-script-p)))


;; * Packages

(use-package no-littering
  :ensure t
  :demand t
  :config
  (progn
    (setq auto-save-file-name-transforms
	  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
    (after 'recentf
      (add-to-list 'recentf-exclude no-littering-var-directory))))

(use-package vlf-setup :ensure vlf)

(use-package super-save
  :ensure t
  :init (after-init #'super-save-mode)
  :config
  (progn
    (setq-default save-silently t)
    (add-to-list 'super-save-triggers 'eyebrowse-switch-to-window-config)
    (add-to-list 'super-save-triggers 'persp-switch)))

;; ** Dired extensions

(use-package dired-hacks-utils
  :ensure t
  :after dired
  :bind
  ((:map dired-mode-map
         (("n" . dired-hacks-next-file)
          ("p" . dired-hacks-previous-file)))))

(use-package dired-narrow
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

(use-package dired-x
  :after dired
  :custom
  (dired-omit-files "^\\.\\|^#.#$\\|.~$"))

(use-package sudo-edit :ensure t)


;; * Commands

;; https://emacs.stackexchange.com/questions/24459/revert-all-open-buffers-and-ignore-errors
(defun -revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.

Buffers in modified (not yet saved) state in emacs will not be
reverted.  They will be reverted though if they were modified
outside emacs.  Buffers visiting files which do not exist any
more or are no longer readable will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(provide 'config-files)
;;; config-files.el ends here
