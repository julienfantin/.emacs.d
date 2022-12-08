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
(require 'config-lsp)

(defvar config-clojure-portal-inject t)

(defvar config-clojure-portal-wrap-nrepl t)

(defun config-clojure-portal-wrap-nrepl-p (&rest _)
  (and config-clojure-portal-inject config-clojure-portal-wrap-nrepl))

(defvar config-clojure-portal-launcher
  (if (featurep 'xwidget) :emacs :app))

;;; Third-party

(use-package clojure-mode
  :straight t
  :ensure-system-package  ((clj . clojure))
  :mode "\\.repl\\'"
  :config
  (put-clojure-indent 'tests 0)
  (put-clojure-indent '-> 0)
  (put-clojure-indent '->> 0)
  :custom
  (clojure-indent-style 'always-indent)
  ;; https://github.com/clojure-emacs/clojure-mode/issues/586
  (clojure-toplevel-inside-comment-form nil))

(use-package clojure-mode-extra-font-locking
  :if (eq config-lsp-frontend nil)
  :straight t
  :after clojure-mode
  :init (require 'clojure-mode-extra-font-locking nil t))

(use-package cider
  :straight t
  :bind (:map clojure-mode-map
              ("C-c C-t" . cider-test-jump)
              ("C-c C-z" . cider-switch-to-repl-buffer)
              ("C-M-<return>". cider-eval-defun-at-point))
  :hook ((cider-mode cider-repl-mode) . eldoc-mode)
  :config
  (cider-add-to-alist 'cider-jack-in-dependencies "org.clojure/tools.deps.alpha" "0.14.1222")
  ;;(cider-add-to-alist 'cider-jack-in-dependencies "mx.cider/enrich-classpath" "1.9.0")
  (add-to-list 'cider-nrepl-middlewares "portal.nrepl/wrap-portal" 'append)
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace))
  ;; TODO hotload dependency with neil
  ;; (require '[clojure.tools.deps.alpha.repl :refer [add-libs]])
  ;; (add-libs 'domain/library {:mvn/version "1.0.1"})
  :custom
  (cider-enrich-classpath t)
  (cider-clojure-cli-aliases ":dev")
  (cider-font-lock-dynamically '(macro core function var))
  ;; (cider-font-lock-dynamically (eq config-lsp-frontend nil))
  (cider-overlays-use-font-lock t)
  ;; conflicts with lsp server
  (cider-eldoc-display-for-symbol-at-point (eq config-lsp-frontend nil))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-preferred-build-tool 'clojure-cli)
  (cider-connection-message-fn #'cider-random-tip)
  (cider-invert-insert-eval-p t)
  ;; Fipp has issues display reified objects correctly
  ;; https://github.com/metosin/malli/issues/789#issuecomment-1342146886
  (cider-print-fn 'pprint)
  (cider-save-file-on-load t)
  (cider-save-files-on-cider-refresh t)
  (cider-repl-prompt-function #'cider-repl-prompt-newline))

(use-package cider
  :if config-clojure-portal-inject
  :preface
  (defun portal.api/open ()
    (interactive)
    (cider-nrepl-sync-request:eval
     (format
      "%s"
      `(do
        (ns user.repl.portal)
        (def portal
             ((requiring-resolve 'portal.api/open) {:launcher ,config-clojure-portal-launcher))
        (add-tap (requiring-resolve 'portal.api/submit))))))
  (defun portal.api/clear ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/clear)"))
  (defun portal-api/tap ()
    (interactive)
    (cider-nrepl-sync-request:eval "(add-tap (requiring-resolve 'portal.api/submit))"))
  (defun portal-api/untap ()
    (interactive)
    (cider-nrepl-sync-request:eval "(remove-tap (requiring-resolve 'portal.api/submit))"))
  (defun portal.api/close ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/close)"))
  :config
  ;; the emcas launcher uses emacsclient to open portal in webkit
  (server-start)
  (when config-clojure-portal-inject
    (cider-add-to-alist 'cider-jack-in-dependencies "djblue/portal" "0.34.2"))
  (add-to-list 'cider-jack-in-nrepl-middlewares
               '("portal.nrepl/wrap-portal" :predicate config-clojure-portal-wrap-nrepl-p) 'append))

(use-package clj-decompiler
  :straight t
  :hook (cider-mode . clj-decompiler-setup))

(use-package cider-repl
  :bind (:map cider-repl-mode-map
              ("C-c C-o" . cider-repl-clear-buffer))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-history-file (user-var-file "nrepl-history"))
  (cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package cider-eval
  :custom
  (cider-eval-result-duration nil))

(use-package cider-debug
  :custom (cider-debug-display-locals t))

(use-package cider-doc
  :hook (cider-doc-mode . (lambda () (setq-local truncate-lines t))))

(use-package cider-stacktrace
  :disabled
  :custom
  (cider-stacktrace-positive-filters '(tooling dup)))

(use-package cider-completion
  :disabled t
  :custom
  (cider-annotate-completion-candidates nil))

(use-package cider-debug
  :disabled t
  :preface
  (defun cider-debug-toggle-eldoc ()
    "Disable eldoc during debugging."
    (if (bound-and-true-p cider--debug-mode)
        (eldoc-mode -1)
      (eldoc-mode 1)))
  :hook (cider--debug-mode . cider-debug-toggle-eldoc))

(use-package cider-test
  :preface
  (defun config-clojure-cider-test-report-render-ansi-escape-advice (&rest _)
    "Fixes rendering issues with matcher combinators."
    (with-current-buffer cider-test-report-buffer
      (read-only-mode -1)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode +1)))
  (defun config-clojure-cider-eval-defun-at-point-run-test-advice (&rest _)
    (let* ((ns  (clojure-find-ns))
           (def (clojure-find-def))
           (deftype (car def)))
      (when (and ns (member deftype cider-test-defining-forms))
        (call-interactively 'cider-test-run-test))))
  :config
  (advice-add
   'cider-test-render-report :after
   'config-clojure-cider-test-report-render-ansi-escape-advice)
  (advice-add
   'cider-eval-defun-at-point :after
   'config-clojure-cider-eval-defun-at-point-run-test-advice))

(use-package clj-refactor
  :if (eq config-lsp-frontend nil)
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
     ("m"    . "missionary.core")
     ("mi"   . "malli.core")
     ("rcf"  . "hyperfiddle.rcf")
     ("s"    . "clojure.spec.alpha")
     ("set"  . "clojure.set")
     ("st"   . "clojure.spec.alpha.test")
     ("str"  . "clojure.string")
     ("walk" . "clojure.walk")
     ("zip"  . "clojure.zip")))
  (cljr-warn-on-eval nil))

;; Compatibility with other packages

(use-package paredit
  :disabled t
  :after (clojure-mode)
  :bind (:map paredit-mode-map
              ([remap paredit-forward] . clojure-forward-logical-sexp)
              ([remap paredit-backward] . clojure-backward-logical-sexp)))

(use-package flymake
  :ensure nil
  :bind (([f8] . flymake-goto-next-error)
         ([f7] . flymake-goto-prev-error))
  :hook (prog-mode . (lambda () (flymake-mode t)))
  :config (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package flymake-kondor
  :if (eq config-lsp-frontend nil)
  :straight t
  :hook (clojure-mode . flymake-kondor-setup))

(use-package flycheck-clj-kondo
  :if (eq config-lsp-frontend nil)
  :straight t
  :ensure-system-package (clj-kondo . borkdude/brew/clj-kondo)
  :after clojure-mode
  :init (require 'flycheck-clj-kondo nil t))

(use-package neil
  :ensure-system-package (neil . babashka/brew/neil)
  :straight (neil :type git :host github :repo "babashka/neil")
  :custom
  (neil-inject-dep-to-project-p t))

(use-package eglot
  :if (eq config-lsp-frontend 'eglot)
  :straight t
  :hook ((clojure-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)))

(use-package lsp-mode
  :if (eq config-lsp-frontend 'lsp-mode)
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)))

(use-package lsp-clojure
  :demand t
  :after lsp-mode
  :preface
  (defvar config-clojure-prefer-cider-completion nil)
  :hook (cider-mode . config-clojure-set-lsp-completion)
  :preface
  (defun config-clojure-set-lsp-completion ()
    (lsp-completion-mode
     (if (and config-clojure-prefer-cider-completion cider-mode) -1 1))))

(use-package clj-deps-new
  :straight t)

(use-package jarchive
  :straight (jarchive :type git :host github :repo "emacs-straight/jarchive")
  :hook (clojure-mode . jarchive-setup))

(provide 'config-clojure)
;;; config-clojure.el ends here
