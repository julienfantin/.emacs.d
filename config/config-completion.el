;;; config-completion.el --- Read and auto completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'use-package)

(defvar config-completion-completion-at-point 'company)

;;; Built-ins

(use-package minibuffer
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config (defalias 'yes-or-no-p 'y-or-n-p)
  :custom
  (enable-recursive-minibuffers t)
  (read-answer-short t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (resize-mini-windows t))

;;; Third-party

;;
;; Minibuffer
;;

(use-package emacs
  :custom
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Consult et al.

(use-package orderless
  :straight t
  :custom
  (orderless-matching-styles
   '(orderless-literal
     orderless-regexp
     orderless-initialism
     orderless-prefixes))
  ;; Optionally use the `orderless' completion style. See
  ;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
  ;; dispatcher. Additionally enable `partial-completion' for file path
  ;; expansion. `partial-completion' is important for wildcard support.
  ;; Multiple files can be opened at once with `find-file' if you enter a
  ;; wildcard. You may also give the `initials' completion style a try.
  (completion-styles '(basic orderless))
  (completion-category-defaults nil)
  (completion-ignore-case t)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (project-file (styles . (basic substring partial-completion orderless)))
     (imenu (styles . (basic substring orderless)))
     (kill-ring (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless))))))

(use-package vertico
  :straight t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize nil))

(use-package vertico-prescient
  :straight t
  :hook (vertico-mode . vertico-prescient-mode))

;; More convenient directory navigation commands
(use-package vertico-directory
  :after vertico
  :load-path "~/.emacs.d/straight/repos/vertico/extensions"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; jump to ~/
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Repeat the last vertico session via the `vertico-repeat' command.
(use-package vertico-repeat
  :after vertico
  :load-path "~/.emacs.d/straight/repos/vertico/extensions"
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-g r" . vertico-repeat))

(use-package marginalia
  :straight t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode)
  :config
  (add-to-list 'marginalia-prompt-categories '("Completion: " . completion))
  (add-to-list 'marginalia-command-categories '(completion-at-point . completion)))

(use-package consult
  :straight t
  :commands (consult-flysome)
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c h" . consult-history)
         ("C-c C-1" . consult-flysome)
         ("C-c m" . consult-mode-command)
         ;;("C-c b" . consult-bookmark)
         ;;("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ;; ([remap imenu] consult-imenu)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (define-key global-map [remap imenu] 'consult-imenu)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize consult-theme
                     :preview-key
                     (list (kbd "M-.")
                           :debounce 0.5 (kbd "<up>") (kbd "<down>")
                           :debounce 1 'any))
  (consult-customize
   ;; Preview immediately on M-., on up/down after 0.5s, on any other key after 1s
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (defun consult-todos  ()
    (interactive)
    (consult-ripgrep (projectile-project-root) "TODO|FIXME|HACK|XXX"))

  (defun consult-flysome ()
    (interactive)
    (cond
     ((bound-and-true-p flymake-mode) (call-interactively 'consult-flymake))
     ((bound-and-true-p flycheck-mode) (call-interactively 'consult-flycheck)))))

(use-package consult-dir
  :straight t
  :bind (:map minibuffer-local-completion-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)
              :map vertico-map
              ("C-x C-d" . consult-dir-maybe)
              ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :after (consult projectile)
  :bind ([remap projectile-find-file] . consult-projectile))

(use-package consult-flycheck
  :straight t
  :after (flycheck))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun with-minibuffer-keymap (keymap)
    (lambda (fn &rest args)
      (minibuffer-with-setup-hook
          (lambda ()
            (use-local-map
             (make-composed-keymap keymap (current-local-map))))
        (apply fn args))))

  (defvar embark-completing-read-prompter-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-M-i") 'abort-recursive-edit)
      map))

  (advice-add
   'embark-completing-read-prompter :around
   (with-minibuffer-keymap embark-completing-read-prompter-map))

  (define-key vertico-map (kbd "C-M-i") 'embark-act-with-completing-read)

  (defun embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (act (propertize "Act" 'face 'highlight))
           (embark-indicator (lambda (_keymap targets) nil)))
      (embark-act arg))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;
;; Completion at point
;;

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent t))

(use-package prescient
  :straight t)

;; Company

(use-package company
  :if (eq config-completion-completion-at-point 'company)
  :straight t
  :hook (after-init . global-company-mode)
  :bind ((:map company-mode-map
               ("TAB" . company-indent-or-complete-common))
         (:map company-active-map
               ("<tab>" . company-complete-selection)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("M-/" . company-other-backend))
         (:map company-search-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :custom
  (company-global-modes
   '(not dired-mode
         git-commit-mode
         magit-status-mode
         markdown-mode
         org-mode
         snippet-mode
         text-mode))
  (company-backends
   '(company-files
     (company-dabbrev-code company-keywords)
     company-dabbrev
     (company-capf :with company-yasnippet)))
  ;; use company-box
  (company-frontends nil)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-search-regexp-function 'company-search-words-in-any-order-regexp)
  (company-show-numbers nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 10)
  (company-tooltip-minimum-width 50))

(use-package company-quickhelp
  :if (eq config-completion-completion-at-point 'company)
  :straight t
  :hook (company-mode . company-quickhelp-local-mode))

(use-package company-dabbrev
  :if (eq config-completion-completion-at-point 'company)
  :after company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-minimum-length 4))

(use-package company-prescient
  :if (eq config-completion-completion-at-point 'company)
  :straight t
  :after (company no-littering)
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode 1))

(use-package company-box
  :if (and window-system (eq config-completion-completion-at-point 'company))
  :straight t
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-doc-delay 0.2)
  (company-box-show-single-candidate t)
  :config
  (add-to-list 'company-box-doc-frame-parameters '(internal-border-width . 0))
  (add-to-list 'company-box-frame-parameters '(internal-border-width . 0))
  (defun company-box-doc (selection frame)
    (when company-box-doc-enable
      ;; stop flickering while typing
      ;; (company-box-doc--hide frame)
      (when (timerp company-box-doc--timer)
        (cancel-timer company-box-doc--timer))
      (setq company-box-doc--timer
            (run-with-timer
             company-box-doc-delay nil
             (lambda nil
               (company-box-doc--show selection frame)
               (company-ensure-emulation-alist)))))))

(use-package company-elisp
  :if (eq config-completion-completion-at-point 'company)
  :after company
  :hook (emacs-lisp-mode . config-completion-company-elisp)
  :preface
  (defun config-completion-company-elisp ()
    (setq-local company-backends (cons '(company-elisp :with company-yasnippet) company-backends)))
  :commands config-completion-company-elisp
  :custom
  (company-elisp-detect-function-context nil))

(use-package company-lsp
  :straight nil
  :after (company lsp-mode)
  :hook (lsp-mode . config-completion-company-lsp)
  :preface
  (defun config-completion-company-lsp ()
    (setq-local company-backends
                (cons '(company-capf :with company-yasnippet) company-backends))))

;; Corfu

(use-package corfu
  :if (eq config-completion-completion-at-point 'corfu)
  :straight (:files (:defaults "extensions/*.el"))
  ;; Hit C-M-i during completion to insert a space while keeping the completion
  ;; ui open and switch to orderless completion. The ui remains open until a
  ;; word boundary is detected, and stays open in case there is no match. The
  ;; latter is useful when searching for a result with C-M-i, we don't want to
  ;; exit the search because of a typo.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  ;; Optional customizations
  :custom
  (corfu-auto t)                      ;; Enable auto completion
  (corfu-on-exact-match 'insert)      ;; Insert after typing the match
  (corfu-quit-at-boundary 'separator) ;; Never quit at completion boundary
  (corfu-separator ?\s)               ;; Orderless field separator
  (corfu-quit-no-match t)
  (corfu-preview-current nil) ;; Disable current candidate preview
  (corfu-scroll-margin 5)     ;; Use scroll margin
  (corfu-min-width 50)
  (corfu-count 14)
  (corfu-popupinfo-max-height 100)
  (corfu-popupinfo-delay t)
  :custom
  (corfu-excluded-modes '(markdown-mode org-mode))
  :custom-face
  (corfu-popupinfo ((t :height 1.0)))
  :config
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (defun corfu-enable-in-minibuffer ()
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (with-eval-after-load "config-prose"
    (defun corfu-prose-mode ()
      (setq-local corfu-auto nil)
      (corfu-mode 1))
    (add-hook 'prose-minor-mode-hook #'corfu-mode)))

(use-package corfu-history
  :if (eq config-completion-completion-at-point 'corfu)
  :after (corfu savehist-mode)
  :hook (savehist-mode . corfu-history-mode)
  :config
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-prescient
  :if (eq config-completion-completion-at-point 'corfu)
  :straight t
  :after (corfu no-littering)
  :hook (corfu-mode . corfu-prescient-mode)
  :config
  (prescient-persist-mode 1))

(use-package kind-icon
  :if (eq config-completion-completion-at-point 'corfu)
  :straight t
  :after (corfu)
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face 'corfu-default))

(use-package cape
  :if (eq config-completion-completion-at-point 'corfu)
  :straight t
  :defer 2
  :preface
  (defvar cape-completion-functions-alist
    '((prog-mode . (cape-symbol
                    cape-keyword
                    cape-history
                    cape-file
                    cape-dabbrev))
      (text-mode . (cape-file
                    cape-ispell
                    cape-dict))))
  :init
  (defun cape-set-completion-functions ()
    (dolist (cell cape-completion-functions-alist)
      (let ((mode (car cell)))
        (when (or (eq major-mode mode)
                  (derived-mode-p major-mode mode))
          (dolist (func (cdr cell))
            (add-to-list 'completion-at-point-functions func))))))
  :hook ((prog-mode . cape-set-completion-functions)
         (text-mode . cape-set-completion-functions)))

;;
;; Templates
;;

(defvar config-completion-templates 'yasnippet)

(use-package tempel
  :if (eq config-completion-templates 'tempel)
  :straight t
  :bind (("M-+" . tempel-expand) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :custom
  (tempel-trigger-prefix nil)
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :custom
  (tempel-path "~/.emacs.d/etc/templates"))

(use-package yasnippet
  :defer 2
  :if (eq config-completion-templates 'yasnippet)
  :hook (after-init . yas-global-mode)
  :custom
  (yas-snippet-dirs (list (expand-file-name "etc/snippets/" user-emacs-directory))))

(use-package copilot
  :straight (copilot :host github :repo "zerolfx/copilot.el" :files ("*.el" "dist"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-M-i" . copilot-accept-completion-by-word)
              ("C-M-I" . copilot-accept-completion)
              ("C-M-n" . copilot-next-completion)
              ("C-M-p" . copilot-previous-completion))
  :config
  (setq copilot-node-executable (expand-file-name "~/.nvm/versions/node/v17.9.1/bin/node")))

(provide 'config-completion)
;;; config-completion.el ends here
