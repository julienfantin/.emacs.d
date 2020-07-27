;;; config-files.el --- Files handling configuration
;;; Commentary:
;;
;;; Code:

(require 'use-package)
(require 'config-path)

;;; Builtins

(use-package emacs
  :custom
  (delete-by-moving-to-trash t)
  (default-major-mode 'text-mode)
  (large-file-warning-threshold 1000000))

(use-package recentf
  :hook (after-init . recentf-mode)
  :after no-littering
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 200)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory))

;; From https://github.com/The-BigDaddy/.emacs.d
(use-package autorevert
  :hook ((focus-in . -auto-revert-buffers)
         (after-save . -auto-revert-buffers))
  :custom
  (auto-revert-verbose t)
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  :config
  ;; Instead of using `auto-revert-mode' or `global-auto-revert-mode', we employ
  ;; lazy auto reverting on `focus-in-hook'.
  ;;
  ;; This is because autorevert abuses the heck out of inotify handles which can
  ;; grind Emacs to a halt if you do expensive IO (outside of Emacs) on the
  ;; files you have open (like compression). We only really need revert changes
  ;; when we switch to a buffer or when we focus the Emacs frame.
  (defun -auto-revert-buffers (&optional buffer-list)
    (dolist (buf (cl-loop for buf in (or buffer-list (buffer-list))
                          when (get-buffer-window buf)
                          collect buf))
      (with-current-buffer buf
        (auto-revert-handler)))))

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

(use-package dired-x
  :after dired
  :custom
  (dired-omit-files "^\\.\\|^#.#$\\|.~$"))

(use-package files
  :preface
  (defun config-files--create-buffer-file-parent-directories ()
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
          (make-directory dir t)))))
  :hook
  ((before-save . config-files--create-buffer-file-parent-directories)
   (after-save  . executable-make-buffer-file-executable-if-script-p)))

;;; Third-party

(use-package no-littering
  :straight t
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package vlf-setup
  :straight vlf)

(use-package super-save
  :straight t
  :hook (after-init . super-save-mode)
  :config
  (setq-default save-silently t))

(use-package sudo-edit :straight t)

(use-package dired-k
  :disabled t
  :straight t
  :after dired
  :hook ((dired-initial-position . dired-k)
         (dired-after-readin     . dired-k-no-revert))
  :custom
  (dired-k-style nil)
  (dired-listing-switches "-alh")
  (dired-k-human-readable t))

(use-package dired-hacks-utils
  :disabled t
  :straight t
  :after dired
  :bind (:map dired-mode-map
              (("n" . dired-hacks-next-file)
               ("p" . dired-hacks-previous-file))))

(use-package dired-narrow
  :disabled t
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow-fuzzy)))

;;; Commands

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
