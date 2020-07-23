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
(require 'map)


;; * Built-ins

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (history-length most-positive-fixnum))

;; ** Abbrev

(use-package abbrev
  :after no-littering
  :config
  (progn
    (setq-default abbrev-mode t)
    (quietly-read-abbrev-file))
  :custom
  (save-abbrevs 'silently))

;; ** Mini-buffer

(use-package minibuffer
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config (defalias 'yes-or-no-p 'y-or-n-p)
  :custom
  (enable-recursive-minibuffers t)
  (completion-category-overrides
   '((file (styles initials basic))
     (buffer (styles initials basic))
     (info-menu (styles basic))))
  (read-answer-short t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t))

;; ** Icomplete

(use-package icomplete
  :if (equal config-completion-system 'icomplete)
  :hook (after-init . fido-mode)
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("<right>" . icomplete-forward-completions)
              ("<down>" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("<left>" . icomplete-backward-completions)
              ("<up>" . icomplete-backward-completions))
  :custom
  (icomplete-hide-common-prefix t)
  (icomplete-delay-completions-threshold 0)
  (icomplete-compute-delay 0)
  (icomplete-max-delay-chars 0)
  (icomplete-prospects-height 1)
  (icomplete-show-matches-on-no-input t)
  (icomplete-tidy-shadowed-file-names t))


;; * Mini-buffer completion packages

(use-package icomplete-vertical
  :if (equal config-completion-system 'icomplete)
  :straight t
  :after (minibuffer icomplete)
  :bind (("C-M-y" . -icomplete-yank-kill-ring)
         :map icomplete-minibuffer-map
         ("C-v" . icomplete-vertical-toggle))
  :config
  (defun -icomplete-yank-kill-ring ()
    "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.
Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
    (interactive)
    (let ((kills                        ; do not sort items
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do
       (:separator 'dotted-line :height (/ (frame-height) 4))
       (when (use-region-p)
         (delete-region (region-beginning) (region-end)))
       (insert
        (completing-read "Yank from kill ring: " kills nil t)))))
  :custom
  (icomplete-vertical-prospects-height 10))

(use-package embark
  :if (equal config-completion-system 'icomplete)
  :straight (embark :type git :host github :repo "oantolin/embark")
  :bind
  (:map minibuffer-local-map
        (">" . embark-become))
  (:map minibuffer-local-completion-map
        (":" . embark-act-noexit)
        (";" . embark-act)
        ("C-o" . embark-occur)
        ("M-e" . embark-export)
        ("M-q" . embark-occur-toggle-view))
  (:map completion-list-mode-map
        (";" . embark-act))
  (:map embark-occur-mode-map
        ("a")
        (";" . embark-act)
        ("'" . avy-embark-occur-choose)
        ("\"" . avy-embark-occur-act)))

(use-package which-key
  :after (which-key embark)
  :config
  ;; Ignore numeric prefixes
  (push
   '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
   which-key-replacement-alist))

(use-package completing-history
  :if (equal config-completion-system 'icomplete)
  :straight (completing-history :type git :host github :repo "oantolin/completing-history")
  :hook (after-init . completing-history-setup-keybinding))

(use-package amx
  :straight t
  :hook (after-init . amx-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless)))


;; * Company

(use-package company
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
  :straight t
  :hook (company-mode . company-quickhelp-local-mode))

(use-package company-posframe
  :straight t
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41422
  :disabled t
  :hook (company-mode . company-posframe-mode))

(use-package company-dabbrev
  :after company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-minimum-length 4))

(use-package company-prescient
  :straight t
  :after (company no-littering)
  :hook (company-mode . company-prescient-mode)
  :config
  (prescient-persist-mode 1))

(use-package compdef
  :straight t
  :after company
  :config
  (compdef
   :modes 'prog-mode
   :company '((company-capf company-files company-keywords company-dabbrev-code :with company-yasnippet))))

(provide 'config-completion)
;;; config-completion.el ends here
