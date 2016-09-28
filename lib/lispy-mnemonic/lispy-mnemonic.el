;;; lispy-mnemonic.el --- Mnemonic key bindings for Lispy. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Tim Krones

;; Authors: Tim Krones <t.krones@gmx.net>, Julien Fantin <julienfantin@gmail.com>
;; Version: 0.2
;; Package-Requires: ((lispy "0.23.0") (hydra "0.12.0"))
;; URL: https://github.com/itsjeyd/lispy-mnemonic
;; Keywords: lisp

;;; This file is not part of GNU Emacs

;;; License

;; This program is free software: you can redistribute it and/or modify
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
;; This package provides a minor mode that sets up mnemonic[1] key
;; bindings for Lispy[2]. lispy-mnemonic also makes extensive use of
;; hydras[3] for grouping related commands and improving
;; discoverability.
;;
;; Background
;;
;; lispy-mnemonic started out as an attempt to make it easier for
;; myself to learn Lispy. I'm familiar with the concept of modal
;; editing, but I'm not a Vim user (which means that it doesn't come
;; naturally to me to use, e.g., hjkl for movement). Also, I find it
;; harder to commit a key sequence to memory if it is completely
;; unrelated to the command it is bound to.
;;
;; Philosophy
;;
;; There are probably some exceptions but in general, key binding
;; choices are based on the following principles:
;;
;; 1. *Make key bindings match command names.* Alternatively, base key
;;    binding choices on default bindings for related functionality
;;    that ships with Emacs.
;;
;; 2. *Group related commands* under a common prefix. Separate
;;    commands that share a common prefix in Lispy if they don't seem
;;    to be strongly related in terms of functionality.
;;
;; 3. *Improve discoverability*, but don't be overly obtrusive.
;;
;; 4. *Do not override default key bindings* (especially if the
;;    functionality they provide is completely unrelated to the
;;    command you are trying to bind).
;;
;; Target Audience
;;
;; If you:
;;
;; - would like to start learning Lispy
;; - have played around with Lispy but not mastered it
;; - haven't burned Vim-style key bindings into your muscle memory
;; - find that mnemonics make it easier to learn and remember new
;;   commands and key bindings
;;
;; ... there is a good chance you'll benefit from using lispy-mnemonic.
;;
;; If you have already memorized most or all of Lispy's commands and
;; key bindings (kudos!), you probably don't need this package. But
;; before you leave, *do* have a look at the hydras that
;; lispy-mnemonic ships with[4]. Who knows, maybe you'll find a couple
;; that you like :)
;;
;; Installation
;;
;; lispy-mnemonic is not on MELPA[5] (yet). To start using it, follow
;; these steps:
;;
;; 1. If you haven't already, install Lispy:
;;
;;    M-x package-install RET lispy RET
;;
;; 2. Clone this repo:
;;
;;    $ git clone https://github.com/itsjeyd/lispy-mnemonic.git
;;
;; 3. Add the following code to your init-file:
;;
;;    (add-to-list 'load-path "~/path/to/lispy-mnemonic/")
;;    (require 'lispy-mnemonic)
;;
;; 4. **Optional**: To turn `lispy-mnemonic-mode' on automatically
;;    for, e.g., buffers that are in `emacs-lisp-mode', add the
;;    following code to your init-file:
;;
;;         (add-hook 'emacs-lisp-mode-hook 'lispy-mnemonic-mode)
;;
;; Usage
;;
;; Do M-x lispy-mnemonic-mode RET to turn `lispy-mnemonic-mode' on or
;; off.
;;
;; See [6] for a full list of bindings.
;;
;; Customization
;;
;; By default, lispy-mnemonic does not alter Lispy bindings that
;; conflict with default bindings for built-in commands. If you would
;; like to restore the original behavior of any bindings that Lispy
;; overrides, add the following to your init-file:
;;
;;     (setq lispy-mnemonic-restore-bindings t)
;;
;; Here is a list of bindings that will be restored:
;;
;;  | Keys | Command                 |
;;  |------+-------------------------|
;;  | C-2  | digit-argument          |
;;  | C-3  | digit-argument          |
;;  | C-4  | digit-argument          |
;;  | C-7  | digit-argument          |
;;  | C-8  | digit-argument          |
;;  | C-9  | digit-argument          |
;;  | M-,  | tags-loop-continue      |
;;  | M-m  | back-to-indentation     |
;;  | M-i  | tab-to-tab-stop         |
;;  | M-j  | indent-new-comment-line |
;;  | M-J  | indent-new-comment-line |
;;  |------+-------------------------|
;;
;; Links
;;
;; [1] https://en.wikipedia.org/wiki/Mnemonic
;; [2] https://github.com/abo-abo/lispy
;; [3] https://github.com/abo-abo/hydra
;; [4] https://github.com/itsjeyd/lispy-mnemonic/blob/master/bindings.org#hydras
;; [5] http://melpa.org/
;; [6] https://github.com/itsjeyd/lispy-mnemonic/blob/master/bindings.org

;; NOTES: (Julien Fantin)
;;
;; Do not inherit from the base lispy bindings
;; Change basic navigation commands to match emacs list navigation more closely
;; Use Capital letters in special for hydra bodies
;; Map some hydra heads directly to lower case letters in special
;; Use C-M bindings in non-special and try to be consistent with special ones
;; Prefer mapping commands to keys that have similar semantics in emacs
;; (e.g. left and right are in fact up and down)
;; Prefer remapping stock bindings with similar semantics rather than finding a
;; mnemonic one (e.g. M-@ mark-word is remapped to lispy-mark-symbol)
;;
;; TODO org-speed-commands for outline navigation
;;; Code:

(require 'lispy)
(require 'hydra)


;; * Customs

(defvar lispy-pp-eval-sexp-function #'pp-eval-last-sexp)


;; * Helpers

;; ** Pprint

(defun lispy-pp-eval-sexp (&optional arg)
  "Move forward and call 'lispy-pp-eval-sexp-function' with 'ARG'.
This command preserves the point position."
  (interactive)
  (save-excursion
    (lispy-forward 1)
    (funcall-interactively lispy-pp-eval-sexp-function arg)))

;; ** Delete blank lines

(defun lispy-delete-blank-lines (&optional arg)
  "Ugly hack to delete lines above defun."
  (interactive)
  (if (not (lispy-left-p))
      (delete-blank-lines)
    (forward-line -1)
    (delete-blank-lines)
    (forward-line 1)))

;; ** Eldoc
;; Disable eldoc while hydras execute

(defvar lispy-mnemonic--eldoc-toggle nil)

(defun lispy-mnemonic--eldoc-command ()
  "Return a symbol to toggle 'eldoc'."
  (cond
   ((bound-and-true-p global-eldoc-mode) 'global-eldoc-mode)
   ((bound-and-true-p eldoc-mode) 'eldoc-mode)
   (t lispy-mnemonic--eldoc-toggle)))

(defun lispy-mnemonic--eldoc-turn-off ()
  "Turn off 'eldoc-mode' or 'global-eldoc-mode' if they are enabled."
  (setq lispy-mnemonic--eldoc-toggle nil)
  (when-let ((cmd (lispy-mnemonic--eldoc-command)))
    (setq lispy-mnemonic--eldoc-toggle cmd)
    (funcall cmd -1)))

(defun lispy-mnemonic--eldoc-turn-on ()
  "Turn on 'eldoc-mode' or 'global-eldoc-mode' if they were enabled."
  (when-let ((cmd (lispy-mnemonic--eldoc-command)))
    (setq lispy-mnemonic--eldoc-toggle nil)
    (funcall cmd 1)))


;; * Navigation commands

(defvar lispy-mnemonic-f #'lispy-flow)
(defvar lispy-mnemonic-b #'lispy-backward)
(defvar lispy-mnemonic-n #'lispy-down)
(defvar lispy-mnemonic-p #'lispy-up)
(defvar lispy-mnemonic-u #'lispy-left)
(defvar lispy-mnemonic-d #'lispy-right)


;; * Hydra

(defhydra hydra-lispy-ace (:color blue :body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy ace"
  ("c" lispy-ace-char "char")
  ("p" lispy-ace-paren "paren")
  ("r" lispy-ace-symbol-replace "replace")
  ("s" lispy-ace-symbol "symbol")
  ("w" lispy-ace-subword "word")
  ("d" lispy-goto-def-ace "definition")
  ("t" lispy-teleport "teleport"))

(defhydra hydra-lispy-debug (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy debug"
  ("a" lispy-arglist-inline "args")
  ("e" lispy-edebug "edebug")
  ("s" lispy-debug-step-in "step in")
  ("S" lispy-edebug-stop "stop")
  ("d" lispy-describe "describe")
  ("D" lispy-describe-inline "describe inline"))

(defhydra hydra-lispy-eval
  (:color red :body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy eval"
  ("e" lispy-eval "eval")
  ("P" lispy-pp-eval-sexp "pp")
  ("r" lispy-eval-and-replace "replace")
  ("o" lispy-eval-other-window "other window")
  ("i" lispy-eval-and-insert "insert")
  ("c" lispy-eval-and-comment "comment")
  ("f" (call-interactively lispy-mnemonic-f))
  ("b" (call-interactively lispy-mnemonic-b))
  ("n" (call-interactively lispy-mnemonic-n))
  ("p" (call-interactively lispy-mnemonic-p))
  ("u" (call-interactively lispy-mnemonic-u))
  ("d" (call-interactively lispy-mnemonic-d))
  ("g" keyboard-quit :exit t))

(defhydra hydra-lispy-format (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy format"
  ("o" lispy-oneline "one line")
  ("m" lispy-multiline "multiple lines"))

(defhydra hydra-lispy-goto (:color blue :body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy goto"
  ("a" lispy-goto-def-ace "ace")
  ("d" lispy-goto-def-down "down")
  ("f" lispy-follow "follow")
  ("g" lispy-goto "goto")
  ("l" lispy-goto-local "local")
  ("p" lispy-goto-projectile "projectile")
  ("r" lispy-goto-recursive "recursive")
  ("s" lispy-goto-symbol "symbol")
  ("." lispy-goto-symbol "symbol")
  ("*" pop-tag-mark "pop tag mark" :color red))

(defhydra hydra-lispy-mark (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy mark"
  ("m" lispy-mark "mark (or expand)")
  ("c" lispy-mark-car "car")
  ("l" lispy-mark-list "list")
  ("s" lispy-mark-symbol "symbol")
  ("b" lispy-mark-left "left")
  ("f" lispy-mark-right "right")
  ("<" lispy-slurp "slurp")
  (">" lispy-barf "barf"))

(defhydra hydra-lispy-move (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy move"
  ("n" lispy-move-down "down")
  ("b" lispy-move-left "left")
  ("f" lispy-move-right "right")
  ("p" lispy-move-up "up"))

(defhydra hydra-lispy-outline (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy outline"
  ("i" lispy-tab "tab")
  ("I" lispy-shifttab "shift-tab")
  ("n" lispy-outline-next "next")
  ("p" lispy-outline-prev "previous")
  ("c" lispy-outline-goto-child "child")
  ("b" lispy-outline-left "left")
  ("f" lispy-outline-right "right"))

(defhydra hydra-lispy-raise (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy raise"
  ("r" lispy-raise-sexp "raise")
  ("s" lispy-raise-some "some"))

(defhydra hydra-lispy-slurp (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy slurp"
  ("(" lispy-slurp "slurp")
  ("d" lispy-down-slurp "down")
  ("u" lispy-up-slurp "up")
  (")" lispy-barf "barf"))

(defhydra hydra-lispy-x (:body-pre (lispy-mnemonic--eldoc-turn-off) :before-exit (lispy-mnemonic--eldoc-turn-on))
  "Lispy x"
  ("f" lispy-flatten "flatten")
  ("c" lispy-to-cond "to cond")
  ("d" lispy-to-defun "to defun")
  ("i" lispy-to-ifs "to ifs")
  ("l" lispy-to-lambda "to lambda")
  ("r" lispy-eval-and-replace "replace")
  ("b" lispy-bind-variable "bind variable")
  ("u" lispy-unbind-variable "unbind variable"))


;; * Key Bindings

(defvar lispy-mnemonic-mode-map (make-sparse-keymap)
  "Keymap for `lispy-mnemonic-mode'.")

(defvar lispy-mnemonic-mode-map-base
  (let ((map (copy-keymap lispy-mnemonic-mode-map)))
    ;;
    ;; Lispy defaults
    ;;
    ;; navigation
    (define-key map (kbd "C-a") #'lispy-move-beginning-of-line)
    (define-key map (kbd "C-e") #'lispy-move-end-of-line)
    (define-key map (kbd "]") #'lispy-forward)
    (define-key map (kbd "[") #'lispy-backward)
    (define-key map (kbd "C-M-g") #'hydra-lispy-goto/body) ; goto-map
    ;; killing
    (define-key map (kbd "M-@") #'lispy-mark-symbol) ; mark-word
    (define-key map (kbd "C-M-@") #'lispy-mark)      ; mark-sexp
    (define-key map (kbd "M-k") #'lispy-kill-sentence)
    (define-key map (kbd "C-k") #'lispy-kill)
    (define-key map (kbd "C-M-k") #'kill-sexp)
    (define-key map (kbd "C-M-S-k") #'lispy-kill-at-point)
    (define-key map (kbd "M-d") #'lispy-kill-word)
    (define-key map (kbd "M-DEL") #'lispy-backward-kill-word)
    ;; misc
    (define-key map (kbd "M-i") #'lispy-iedit)
    (define-key map (kbd ";") #'lispy-comment)
    (define-key map (kbd "M-q") #'lispy-fill) ; fill-paragraph
    (define-key map (kbd "C-j") #'lispy-newline-and-indent)
    (define-key map (kbd "RET") #'lispy-alt-line)
    ;; pairs
    (define-key map (kbd "(") #'lispy-parens)
    (define-key map (kbd ")") #'lispy-right-nostring)
    (define-key map (kbd "{") #'lispy-braces) ; quirky but frees []
    (define-key map (kbd "}") #'lispy-brackets)
    ;; Basic editing
    (define-key map (kbd "C-y") #'lispy-yank)
    (define-key map (kbd "C-d") #'lispy-delete)
    (define-key map (kbd "DEL") #'lispy-delete-backward)
    ;; insert
    (define-key map (kbd "M-\"") #'lispy-meta-doublequote)
    (define-key map (kbd "\"") #'lispy-quotes)
    (define-key map (kbd ":") #'lispy-colon)
    (define-key map (kbd "^") #'lispy-hat)
    (define-key map (kbd "'") #'lispy-tick)
    (define-key map (kbd "`") #'lispy-backtick)
    (define-key map (kbd "#") #'lispy-hash)
    ;; List movements
    (define-key map (kbd "C-M-a") #'lispy-beginning-of-defun)
    (define-key map (kbd "C-M-n") lispy-mnemonic-n)
    (define-key map (kbd "C-M-p") lispy-mnemonic-p)
    (define-key map (kbd "C-M-b") #'backward-sexp)
    (define-key map (kbd "C-M-f") #'forward-sexp)
    (define-key map (kbd "C-M-u") lispy-mnemonic-u)
    (define-key map (kbd "C-M-d") lispy-mnemonic-d)
    (define-key map (kbd "C-M-S-p") #'lispy-outline-prev)
    (define-key map (kbd "C-M-S-n") #'lispy-outline-next)
    ;; Paredit depth changes
    (define-key map (kbd "M-o") #'lispy-parens-down)
    (define-key map (kbd "M-(") #'lispy-wrap-round)
    (define-key map (kbd "M-[") #'lispy-wrap-brackets)
    (define-key map (kbd "M-{") #'lispy-wrap-braces)
    (define-key map (kbd "C-)") #'lispy-forward-slurp-sexp)
    (define-key map (kbd "C-(") #'lispy-backward-slurp-sexp)
    ;; Paredit misc
    (define-key map (kbd "C-M-o") #'lispy-split) ; split-line
    (define-key map (kbd "C-M-j") #'lispy-join)
    (define-key map (kbd "C-M-s") #'lispy-splice) ; overrides isearch regexp
    (define-key map (kbd "C-M-r") #'hydra-lispy-raise/body) ; overrides isearch regexp
    (define-key map (kbd "C-M-c") #'lispy-convolute)

    (define-key map (kbd "C-x C-o") #'lispy-delete-blank-lines)
    ;; (define-key map (kbd "<tab>") #'lispy-indent-adjust-parens)
    ;; (define-key map (kbd "S-<tab>") #'lispy-dedent-adjust-parens)

    ;; (define-key map (kbd "C-.")    'lispy-kill-at-point)
    ;; (define-key map (kbd "C-1")    'lispy-string-oneline)
    map))

(defvar lispy-mnemonic-mode-map-special
  (let ((map (copy-keymap lispy-mnemonic-mode-map-base)))
    ;; digit argument
    (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))
    ;; a-z
    (lispy-define-key map "~" #'lispy-tilde)
    ;; Like M-, but problematic in elisp macros....
    ;; (lispy-define-key map "," #'pop-tag-mark)
    (lispy-define-key map "@" #'lispy-mark) ; M-@ mark-word
    (lispy-define-key map "SPC" #'lispy-space)
    (lispy-define-key map "." #'lispy-goto-symbol)
    (lispy-define-key map "_" #'lispy-underscore)

    (lispy-define-key map "a" #'hydra-lispy-ace/body)
    (lispy-define-key map "b" lispy-mnemonic-b)
    (lispy-define-key map "c" #'lispy-clone)
    (lispy-define-key map "d" lispy-mnemonic-d)
    (lispy-define-key map "e" #'hydra-lispy-eval/lispy-eval)
    (lispy-define-key map "f" lispy-mnemonic-f)
    (lispy-define-key map "g" #'lispy-goto)
    (lispy-define-key map "h" #'lispy-describe) ; help
    (lispy-define-key map "i" #'lispy-tab)      ; indent
    (lispy-define-key map "j" #'lispy-join)
    (lispy-define-key map "k" #'lispy-kill)
    ;; (lispy-define-key map (kbd "l") #'lispy-left)
    (lispy-define-key map "m" #'lispy-mark)
    (lispy-define-key map "n" lispy-mnemonic-n)
    (lispy-define-key map "o" #'lispy-occur)
    (lispy-define-key map "p" lispy-mnemonic-p)

    ;; (lispy-define-key map (kbd "r") #'lispy-right)
    (lispy-define-key map "s" #'lispy-splice)
    (lispy-define-key map "t" #'lispy-teleport)
    (lispy-define-key map "u" lispy-mnemonic-u)
    (lispy-define-key map "w" #'lispy-new-copy)
    (lispy-define-key map "x" #'lispy-different) ; C-x C-x
    (lispy-define-key map "y" #'lispy-paste)     ; yank
    (lispy-define-key map "z" #'lispy-repeat)    ; C-x z
    ;; A-Z
    (lispy-define-key map "D" #'hydra-lispy-debug/body)
    (lispy-define-key map "F" #'hydra-lispy-format/body)
    (lispy-define-key map "G" #'hydra-lispy-goto/body)
    (lispy-define-key map "M" #'hydra-lispy-move/body)
    (lispy-define-key map "O" #'hydra-lispy-outline/body)
    (lispy-define-key map "R" #'hydra-lispy-raise/body)
    (lispy-define-key map "X" #'hydra-lispy-x/body)
    (lispy-define-key map "N" #'lispy-outline-next)
    (lispy-define-key map "P" #'lispy-outline-prev)

    (lispy-define-key map ">" #'lispy-slurp-or-barf-right)
    (lispy-define-key map "<" #'lispy-slurp-or-barf-left)

    (lispy-define-key map "<left>" #'lispy-left)
    (lispy-define-key map "<right>" #'lispy-right)
    (lispy-define-key map "<up>" #'lispy-up)
    (lispy-define-key map "<down>" #'lispy-down)
    map))

;;;###autoload
(define-minor-mode lispy-mnemonic-mode
  "Mnemonic key bindings (and hydras) for Lispy."
  :init-value nil
  :keymap lispy-mnemonic-mode-map-special)

(provide 'lispy-mnemonic)
;;; lispy-mnemonic.el ends here
