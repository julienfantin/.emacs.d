;;; config-clojure.el --- Clojure programming        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages, lisp

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

;;; Third-party

(defvar config-clojure-align-binding-forms
  '(
    "gen/let"                           ; test.check
    "with-let"                          ; reagent
    ))

(use-package clojure-mode
  :straight t
  :ensure-system-package  ((clj . clojure))
  :mode "\\.repl\\'"
  :config
  (dolist (form config-clojure-align-binding-forms)
    (push form clojure-align-binding-forms))
  :custom
  (clojure-align-forms-automatically nil)
  (clojure-toplevel-inside-comment-form t))

(use-package paredit
  :hook (clojure-mode . paredit-mode))

(use-package cider
  :hook (cider-repl-mode . paredit-mode))

(use-package clojure-mode-extra-font-locking
  :straight t
  :after clojure-mode
  :init (require 'clojure-mode-extra-font-locking nil t))

(use-package cider
  :straight t
  :bind (:map clojure-mode-map
              ("C-c C-t" . cider-test-jump)
              ("C-c C-z" . cider-switch-to-repl-buffer))
  :hook ((cider-mode cider-repl-mode) . eldoc-mode)
  :custom
  (cider-preferred-build-tool 'clojure-cli)
  (cider-dynamic-indentation nil)
  (cider-font-lock-dynamically '(macro core function deprecated var))
  ;; Too slow
  (cider-invert-insert-eval-p t)
  (cider-switch-to-repl-after-insert-p nil)
  (cider-eval-toplevel-inside-comment-form t)
  (cider-print-fn 'puget)
  (cider-prompt-for-symbol nil)
  (cider-save-file-on-load t)
  (cider-save-files-on-cider-refresh t))

(use-package cider-stacktrace
  :custom
  (cider-stacktrace-default-filters '(clj tooling dup java))
  (cider-stacktrace-positive-filters '(projectb)))

(use-package cider-debug
  :preface
  (defun cider-debug-toggle-eldoc ()
    "Disable eldoc during debugging."
    (if (bound-and-true-p cider--debug-mode)
        (eldoc-mode -1)
      (eldoc-mode 1)))
  :hook (cider--debug-mode . cider-debug-toggle-eldoc))

(use-package cider-repl
  :bind (:map cider-repl-mode-map
              ("C-c C-o" . cider-repl-clear-buffer))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-history-file (user-var-file "nrepl-history"))
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t))

(use-package cider-debug
  :custom (cider-debug-display-locals t))

(use-package clj-refactor
  :straight t
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c r")
  :custom
  (cljr-favor-prefix-notation nil)
  (cljr-ignore-analyzer-errors t)
  (cljr-magic-requires t)
  (cljr-magic-require-namespaces
   '(("a"    . "clojure.core.async")
     ("m"    . "missionary.core")
     ("d"    . "datomic.api")
     ("edn"  . "clojure.edn")
     ("io"   . "clojure.java.io")
     ("json" . "cheshire.core")
     ("log"  . "clojure.tools.logging")
     ("s"    . "clojure.spec.alpha")
     ("set"  . "clojure.set")
     ("st"   . "clojure.spec.alpha.test")
     ("str"  . "clojure.string")
     ("walk" . "clojure.walk")
     ("zip"  . "clojure.zip")))
  (cljr-warn-on-eval nil))

(use-package sayid
  :disabled t
  :straight t
  :after cider
  :init (sayid-setup-package))

(use-package flycheck-clj-kondo
  :ensure-system-package (clj-kondo . borkdude/brew/clj-kondo)
  :straight t
  :after clojure-mode
  :init (require 'flycheck-clj-kondo nil t))

(use-package clojars
  :straight t)

(provide 'config-clojure)
;;; config-clojure.el ends here
