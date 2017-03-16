;;; config-prose.el --- Prose editing                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: languages

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
(require 'use-config)

;; * Customs

(defvar config-prose-enable-prose-spell-checking t)
(defvar config-prose-enable-code-spell-checking t)


;; * Spell-checking

(use-package guess-language
  :disabled t
  :ensure t
  :defer t
  :after prose-mode
  :config
  (after 'prose-minor-mode
    (add-hook 'prose-minor-mode-hook 'guess-language-mode)))

(use-package ispell
  :defer t
  :config
  (progn
    (setq ispell-silently-savep t)
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra"
                                "--lang=en_US"
                                "--add-filter=url"
                                "--add-filter=email")))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args '("-d en_US"))))))

(use-package company
  :ensure t
  :defer t
  :preface
  (defun config-prose-completion-turn-on ()
    (setq-local company-tooltip-limit 3)
    (setq-local company-frontends '(company-preview-frontend))
    (company-mode 1))
  :init
  (progn
    (after 'config-completion
      (config-completion-add-backends 'prose-minor-mode 'company-ispell))
    (after 'prose-minor-mode
      (add-to-list 'prose-minor-mode-hook 'config-prose-completion-turn-on))))

(use-package flyspell
  :defer t
  :functions (config-prose-enable-spell-checking)
  :config
  (progn
    (defun config-prose-enable-spell-checking ()
      (cond
       ((and config-prose-enable-code-spell-checking (derived-mode-p 'prog-mode))
        (flyspell-prog-mode))
       ((and config-prose-enable-prose-spell-checking (bound-and-true-p prose-minor-mode))
        (flyspell-mode))))
    (validate-setq
     ;; Save corrections
     flyspell-abbrev-p t
     flyspell-issue-welcome-flag nil
     flyspell-issue-message-flag nil)
    (add-hook 'prog-mode-hook #'config-prose-enable-spell-checking)
    (after 'prose-minor-mode
      (add-hook 'prose-mode-hook #'config-prose-enable-spell-checking))))

(use-package flyspell-lazy
  :ensure t
  :init (after 'flyspell (flyspell-lazy-mode 1)))

(use-package flyspell-correct
  :ensure t
  :config
  (use-package flyspell-correct-ivy
    :ensure t
    :config
    (define-key flyspell-mode-map
      (kbd "C-c $") 'flyspell-correct-previous-word-generic)))


;; * Prose minor mode

(use-package prose-minor-mode
  :config
  (add-hook 'prose-minor-mode-hook 'visual-line-mode))


;; * Wrapping

(use-package adaptive-wrap
  :ensure t
  :defer t
  :commands adaptive-wrap-prefix-mode
  :init
  (after 'prose-minor-mode
    (add-hook 'prose-minor-mode-hook 'adaptive-wrap-prefix-mode)))


;; * Linters

(use-package writegood-mode
  :ensure t
  :defer t
  :init
  (after 'prose-minor-mode
    (add-hook 'prose-minor-mode-hook #'writegood-mode)))

(use-package artbollocks-mode
  :ensure t
  :defer t
  :init
  (after 'prose-minor-mode
    (add-hook 'prose-minor-mode-hook #'artbollocks-mode)))


;; * Major modes

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (after 'prose-minor-mode
    (add-hook 'markdown-mode-hook 'prose-minor-mode)))

(use-package org
  :defer t
  :config
  (after 'prose-minor-mode
    (add-hook 'org-mode-hook 'prose-minor-mode)))

(provide 'config-prose)
;;; config-prose.el ends here
