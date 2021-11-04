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

;;; Built-ins

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (history-length most-positive-fixnum))

(use-package abbrev
  :after no-littering
  :config
  (progn
    (setq-default abbrev-mode t)
    (quietly-read-abbrev-file))
  :custom
  (save-abbrevs 'silently))

(use-package minibuffer
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config (defalias 'yes-or-no-p 'y-or-n-p)
  :custom
  (enable-recursive-minibuffers t)
  ;; (completion-category-overrides
  ;;  '((file (styles initials basic))
  ;;    (buffer (styles initials basic))
  ;;    (info-menu (styles basic))))
  (read-answer-short t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t))

;;; Third-party

(use-package orderless
  :straight t
  :custom
  ;; Optionally use the `orderless' completion style. See
  ;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
  ;; dispatcher. Additionally enable `partial-completion' for file path
  ;; expansion. `partial-completion' is important for wildcard support.
  ;; Multiple files can be opened at once with `find-file' if you enter a
  ;; wildcard. You may also give the `initials' completion style a try.
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package vertico
  :straight t
  :hook (after-init . vertico-mode))

;; More convenient directory navigation commands
(use-package vertico-directory
  :after vertico
  :load-path "~/.emacs.d/straight/repos/vertico/extensions"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; Commands to select using Avy-style quick keys
(use-package vertico-quick
  :after vertico
  :load-path "~/.emacs.d/straight/repos/vertico/extensions"
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("C-q" . vertico-quick-exit)))

;; Repeat the last vertico session via the `vertico-repeat' command.
(use-package vertico-repeat
  :after vertico
  :load-path "~/.emacs.d/straight/repos/vertico/extensions"
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :bind ("M-g r" . vertico-repeat))

;; A few more useful configurations...
(use-package emacs
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode)
  :config
  (add-to-list 'marginalia-prompt-categories '("Completion: " . completion))
  (defun my-completion-annotator (cand)
    (when (bound-and-true-p clojure-mode)
      (message "got %s" cand)
      (concat (propertize " " 'display '(space :align-to center))
              (propertize cand))))

  (add-to-list 'marginalia-annotator-registry
               '(completion my-completion-annotator builtin none))
  (add-to-list 'marginalia-command-categories '(completion-at-point . completion)))

(use-package consult
  :straight t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
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
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   ;; consult-ripgrep
   ;; consult-git-grep
   ;; consult-grep
   ;; consult-bookmark
   ;; consult-recent-file
   ;; consult-xref
   ;; consult--source-file
   ;; consult--source-project-file
   ;; consult--source-bookmark
   ;; :preview-key (kbd "M-.")
   )

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "."
  ;; ".git")))

  (setq completion-in-region-function
        #'consult-completion-in-region)

  (advice-add #'completing-read-multiple
              :override #'consult-completing-read-multiple)

  (consult-customize
   consult-completion-in-region
   :completion-styles '(substring basic orderless)
   :cycle-threshold 1)


  (defun consult-todos  ()
    (interactive)
    ;; TODO
    (consult-ripgrep (projectile-project-root) "TODO|FIXME|HACK|XXX"))
  )

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :after (consult projectile)
  :config
  (define-key global-map [remap projectile-find-file] consult-projectile))

(use-package consult-flycheck
  :straight t
  :after (flycheck))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



;; (orderless-define-completion-style
;;     orderless-inline
;;   (orderless-matching-styles
;;    '(orderless-prefixes
;;      ;; orderless-literal
;;      ;; orderless-regexp
;;      )))
;; (setq completion-category-overrides
;;       '((command (styles orderless-inline))
;;         (symbol (styles orderless-inline))
;;         (variable (styles orderless-inline))))


;;;; Company

(use-package company
  :disabled t
  :straight t
  :hook (prog-mode . company-mode)
  :bind ((:map company-mode-map
               ("TAB" . company-indent-or-complete-common))
         (:map company-active-map
               ("TAB" . company-complete-common-or-cycle)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("M-/" . company-other-backend))
         (:map company-search-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :custom
  (company-backends
   '((company-capf :with company-yasnippet)
     company-files
     (company-dabbrev-code company-keywords)
     company-dabbrev))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-search-regexp-function 'company-search-words-in-any-order-regexp)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 10)
  (company-tooltip-minimum-width 50))

(use-package company-quickhelp
  :disabled t
  :straight t
  :hook (company-mode . company-quickhelp-local-mode))

(use-package company-dabbrev
  :disabled t
  :after company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-minimum-length 4))

(use-package company-prescient
  :disabled t
  :straight t
  :after (company no-littering)
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode 1))

(use-package compdef
  :disabled t
  :straight t
  :after company
  :config
  (compdef
   :modes 'prog-mode
   :company '((company-capf company-files company-keywords company-dabbrev-code :with company-yasnippet))))

;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


;; Dabbrev works with Corfu
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

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
  (setq tab-always-indent 'complete))


(use-package consult-company
  :disabled t
  :straight t
  (:bind (:map company-mode-map
               ([remap completion-at-point] #'consult-company))))

(use-package consult-dir
  :straight t
  :bind (:map minibuffer-local-completion-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)
              :map vertico-map
              ("C-x C-d" . consult-dir-maybe)
              ("C-x C-j" . consult-dir-jump-file)))

(provide 'config-completion)
;;; config-completion.el ends here
