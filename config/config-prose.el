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
  :ensure t
  :after prose-mode
  :config
  (after 'prose-minor-mode
    (add-hook 'prose-minor-mode-hook 'guess-language-mode)))

(use-package ispell
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

(use-package flyspell
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

(use-package flyspell-lazy
  :ensure t
  :after flyspell
  :init (flyspell-lazy-mode 1))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . 'flyspell-correct-previous-word-generic)))


;; * Prose minor mode

(use-package visual-fill-column :ensure t)

(use-package prose-minor-mode
  :hook ((org-mode . prose-minor-mode)
         (markdown-mode . prose-minor-mode)
         (prose-minor-mode . visual-line-mode)
         (prose-minor-mode . visual-fill-column-mode)
         (prose-minor-mode . flyspell-mode)))


;; * Wrapping

(use-package adaptive-wrap
  :ensure t
  :commands adaptive-wrap-prefix-mode
  :after prose-minor-mode
  :hook (prose-minor-mode . adaptive-wrap-prefix-mode))


;; * Linters

(use-package writegood-mode
  :ensure t
  :after prose-minor-mode
  :hook (prose-minor-mode . writegood-mode))


;; * Major modes

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(provide 'config-prose)
;;; config-prose.el ends here
