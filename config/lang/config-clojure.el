;;; config-clojure.el --- Clojure programming        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
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
(require 'use-config)
(require 'config-clojurescript)

;; * clojure-mode

(use-package clojure-mode
  :ensure t
  :ensure-system-package
  ((clj . clojure)
   (lein . leiningen))
  :mode "\\.repl\\'"
  :config
  (progn
    (define-clojure-indent
      (gen/let nil))
    ;; test.check
    (add-to-list 'clojure-align-binding-forms "gen/let")
    ;; reagent
    (add-to-list 'clojure-align-binding-forms "with-let")
    (define-key clojure-mode-map [remap forward-sexp] #'clojure-forward-logical-sexp)
    (define-key clojure-mode-map [remap backward-sexp] #'clojure-backward-logical-sexp)))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :after clojure-mode
  :init (require 'clojure-mode-extra-font-locking nil t))


;; * cider

(use-package cider
  :ensure t
  ;; :pin melpa-stable
  :bind
  (:map clojure-mode-map
        ("C-c C-t" . cider-test-jump)
        ("C-c C-z" . cider-switch-to-repl-buffer))
  :hook
  ((cider-mode cider-repl-mode) . eldoc-mode)
  :config
  (after (lispy-mnemonic cider-interaction)
    (defun config-clojure--set-lispy-pp-eval-function ()
      (setq-local lispy-pp-eval-sexp-function #'(lambda (&optional _) (cider-pprint-eval-last-sexp))))
    (add-hook 'cider-mode-hook #'config-clojure--set-lispy-pp-eval-function)
    (add-hook 'cider-repl-mode-hook #'config-clojure--set-lispy-pp-eval-function))
  :custom
  (cider-preferred-build-tool 'clojure-cli)
  (cider-auto-jump-to-error nil)
  (cider-dynamic-indentation nil)
  (cider-font-lock-dynamically '(macro core function deprecated var)) ;; Too slow
  (cider-font-lock-dynamically nil)
  (cider-pprint-fn 'pprint)
  (cider-prefer-local-resources t)
  (cider-prompt-for-symbol nil)
  (cider-save-file-on-load t)
  (cider-save-files-on-cider-refresh t))

(use-package cider-stacktrace
  :custom
  (cider-stacktrace-default-filters '(clj tooling dup java repl))
  (cider-stacktrace-positive-filters '(project)))

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
  (cider-repl-use-clojure-font-lock nil)
  (cider-repl-use-pretty-printing t))

(use-package cider-debug
  :custom (cider-debug-display-locals t))


;; * clj-refactor

(use-package clj-refactor
  :ensure t
  ;; :pin melpa-stable
  :after clojure-mode
  :hook  (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c .")
  :custom
  (cljr-favor-prefix-notation nil)
  (cljr-ignore-analyzer-errors t)
  (cljr-magic-requires t)
  (cljr-magic-require-namespaces
   '(("a"    . "clojure.core.async")
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
     ("zip"  . "clojure.zip")
     ("rf"   . "re-frame.core")))
  (cljr-warn-on-eval nil))


;; * Sayid

(use-package sayid
  :ensure t
  ;; :pin melpa-stable
  :after cider
  :init (sayid-setup-package))


;; * Flycheck

(use-package flycheck-joker
  :ensure t
  :ensure-system-package (joker . candid82/brew/joker)
  :after (flycheck clojure-mode)
  :init (require 'flycheck-joker nil t))


;; * Smart Jump

(use-package smart-jump
  :ensure t
  :after (cider)
  :config
  (smart-jump-register
   :modes '(clojure-mode cider-mode cider-repl-mode)
   :jump-fn 'cider-find-var
   :pop-fn 'cider-pop-back
   :refs-fn 'cljr-find-usages
   :should-jump 'cider-connected-p
   :heuristic 'point
   :async 500))

(provide 'config-clojure)
;;; config-clojure.el ends here
