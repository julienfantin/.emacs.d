;;; config-prose.el --- Prose editing                -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received c copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'use-package)

(defvar config-prose-dicts-dir
  (expand-file-name "etc/dictionaries/" user-emacs-directory))

;; (setq ispell-library-directory (expand-file-name "etc/dictionaries/" user-emacs-directory))


;; * Spell-checking

(use-package ispell
  :ensure-system-package (hunspell)
  :config
  (progn
    (require 'dash)
    (setenv "DICPATH" config-prose-dicts-dir)
    (cond
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args '("-d en_US,fr-classique")))
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args '("--sug-mode=ultra"
                                "--lang=en_US"
                                "--add-filter=url"
                                "--add-filter=email")))))
  :custom (ispell-silently-savep t))

(use-package flyspell
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

(use-package flyspell-lazy
  :disabled t
  :straight t
  :after flyspell
  :hook (flyspell-mode . flyspell-lazy-load))

(use-package flyspell-correct-ivy
  :straight t
  :after flyspell
  :init (require 'flyspell-correct-ivy nil t)
  :bind
  (:map flyspell-mode-map
        ("C-c $" . 'flyspell-correct-previous-word-generic)))


;; * Prose minor mode

(use-package visual-fill-column :straight t)

(use-package prose-minor-mode
  :hook ((org-mode         . prose-minor-mode)
         (markdown-mode    . prose-minor-mode)
         (prose-minor-mode . visual-line-mode)
         (prose-minor-mode . visual-fill-column-mode)
         (prose-minor-mode . flyspell-mode)))


;; * Wrapping

(use-package adaptive-wrap
  :straight t
  :commands adaptive-wrap-prefix-mode
  :after prose-minor-mode
  :hook (prose-minor-mode . adaptive-wrap-prefix-mode))


;; * Linters

(use-package writegood-mode
  :straight t
  :after prose-minor-mode
  :hook (prose-minor-mode . writegood-mode))


;; * Major modes

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :custom (markdown-fontify-code-blocks-natively t))

(provide 'config-prose)
;;; config-prose.el ends here
