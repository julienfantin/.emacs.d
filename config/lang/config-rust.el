;;; config-rust.el --- rust                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages, languages, languages, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; cargo install rustfmt racer

;;; Code:
(require 'use-package)
(require 'config-lsp)

(use-package rustic
  ;; This will download and install the toolchain but the PATH won't be updated
  ;; and the compiler won't in the exec-path until it's updated or Emacs is
  ;; restarted. Additionally if this form is evaluated twice in the first
  ;; session it will try to re-install the toolchain.
  :ensure-system-package (rustup-init)
  :ensure-system-package (rustc . "rustup-init")
  :straight t
  :custom
  (rustic-lsp-server 'rust-analyzer))

(use-package flycheck
  :after (flycheck rustic)
  :config
  (push 'rustic-clippy flycheck-checkers))

(use-package rustfmt
  :straight t
  :after rustic
  :hook (rustic-mode . rustfmt-enable-on-save))

(use-package cargo
  :straight t
  :after rustic
  :hook (rustic-mode . cargo-minor-mode))

(use-package flycheck-rust
  :disabled t
  :straight t
  :hook (rustic-mode . flycheck-rust-setup))

;; LSP
;; rustup component add rls rust-analysis rust-src

(use-package lsp-mode
  :ensure-system-package (rust-analyzer)
  :if (eq config-lsp-frontend 'lsp-mode)
  :straight t
  :after rustic
  :hook (rustic-mode . lsp)
  :custom
  (lsp-rust-server 'rust-analyzer))

(provide 'config-rust)
;;; config-rust.el ends here
