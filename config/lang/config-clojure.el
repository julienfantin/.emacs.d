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
  :defer t
  :config
  (progn
    (after 'config-completion
      (config-completion-add-backends 'clojure-mode #'company-capf))
    (define-key clojure-mode-map [remap forward-sexp] #'clojure-forward-logical-sexp)
    (define-key clojure-mode-map [remap backward-sexp] #'clojure-backward-logical-sexp)))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t
  :after clojure-mode)


;; * cider

(use-package cider
  :ensure t
  :defer t
  :config
  (progn
    (bind-key "C-c C-t" 'cider-test-jump clojure-mode-map)
    (bind-key "C-c C-z" 'cider-switch-to-repl-buffer clojure-mode-map)
    (validate-setq
     cider-font-lock-dynamically '(macro core function deprecated var) ;; Too slow
     cider-font-lock-dynamically nil
     cider-prompt-save-file-on-load 'always-save
     cider-prompt-for-symbol nil
     cider-auto-jump-to-error nil
     cider-prefer-local-resources t
     cider-dynamic-indentation nil
     cider-pprint-fn 'pprint)
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'eldoc-mode)
    (after (lispy-mnemonic cider-interaction)
      (defun config-clojure--set-lispy-pp-eval-function ()
        (setq-local lispy-pp-eval-sexp-function #'(lambda (&optional _) (cider-pprint-eval-last-sexp))))
      (add-hook 'cider-mode-hook #'config-clojure--set-lispy-pp-eval-function)
      (add-hook 'cider-repl-mode-hook #'config-clojure--set-lispy-pp-eval-function))))

(use-package cider-stacktrace
  :defer t
  :config
  (setq cider-stacktrace-default-filters '(tooling dup clj java repl)))

(use-package cider-debug
  :defer t
  :config
  (progn
    (defun cider-debug-toggle-eldoc ()
      "Disable eldoc during debugging."
      (if (bound-and-true-p cider--debug-mode)
          (eldoc-mode -1)
        (eldoc-mode 1)))
    (add-hook 'cider--debug-mode-hook 'cider-debug-toggle-eldoc)))

(use-package cider-repl
  :defer t
  :config
  (progn
    (bind-key "C-c C-o" 'cider-repl-clear-buffer cider-repl-mode-map)
    (setq cider-repl-pop-to-buffer-on-connect nil
          cider-repl-use-clojure-font-lock nil
          cider-repl-use-pretty-printing t
          cider-repl-history-file (user-var-file "nrepl-history")
          cider-repl-display-help-banner nil)))

(use-package cider-debug
  :defer t
  :config
  (validate-setq cider-debug-display-locals nil))


;; * clj-refactor

(use-package clj-refactor
  :ensure t
  :defer t
  :preface
  (defun config-clojure-cljr-enable ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c ."))
  :init
  (after 'clojure-mode
    (add-hook 'clojure-mode-hook 'config-clojure-cljr-enable))
  :config
  (validate-setq
   cljr-magic-requires nil
   cljr-favor-prefix-notation nil
   cljr-warn-on-eval nil
   cljr-magic-require-namespaces
   (append cljr-magic-require-namespaces
           '(("edn"       . "clojure.edn")
             ("a"         . "clojure.core.async")
             ("async"     . "clojure.core.async")
             ("component" . "com.stuartsierra.component")
             ("s"         . "schema.core")
             ("fs"        . "me.raynes.fs")
             ("json"      . "cheshire.core")
             ("log"       . "taoensso.timbre")))))


(provide 'config-clojure)
;;; config-clojure.el ends here
