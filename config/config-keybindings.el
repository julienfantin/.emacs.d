;;; config-keybindings.el --- User keybindings       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: hardware, convenience

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

;; Note on macOS C-M-d is clobbered at the system level:
;; defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'

;;; Code:
(require 'use-package)
(require 'cl-lib)

;;; Config

(defvar config-keybdings-hyper-to-C-c t)
(defvar config-keybdings-default-setup #'config-keybindings-macbook)

;;; Keyboard
;;;; Modifiers

(defun config-keybindings-space-control ()
  (bind-keys
   ("C-c SPC" . set-mark-command)
   ("C-c M-SPC" . mark-sexp)))

(defun config-keybindings-macbook ()
  (interactive)
  (cl-case system-type
    ('darwin
     (setq
      mac-command-modifier 'control
      mac-control-modifier 'meta
      mac-option-modifier 'super
      mac-function-modifier 'hyper))))

(when config-keybdings-default-setup
  (funcall config-keybdings-default-setup))

;;;; Hyper remapping

(defvar config-keybindings-keys
  (cl-list*
   "<tab>"
   "RET"
   (string-to-list
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()-=[]{};'\\:\"|,./<>?`~+")))

(defun config-keybindings-remap-hyper-to-C-c ()
  "Remap Hyper single-key bindings to C-c.

This allows quicker access to user-defined mappings, and enables
different hardware remapping tricks like treating the spacebar as
hyper when it's used as a modifier."
  (dolist (key config-keybindings-keys)
    (let ((s (if (integerp key) (char-to-string key) key)))
      (define-key input-decode-map
        (kbd (format "H-%s" s))
        (kbd (format "C-c %s" s))))))

(when config-keybdings-hyper-to-C-c
  (config-keybindings-remap-hyper-to-C-c))

;;; Third-party

(use-package general :straight t)

(use-package free-keys :straight t)

(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-show-transient-maps t))

(use-package keyfreq
  :straight t
  :after no-littering
  :hook ((after-init . keyfreq-mode)
         (after-init . keyfreq-autosave-mode))
  :custom
  (keyfreq-excluded-commands
   '(self-insert-command
     org-self-insert-command
     abort-recursive-edit
     forward-char
     backward-char
     previous-line
     next-line)))

;;; Keymaps
;;;; (a) App

(when (equal 'ivy config-completion-system)
  (general-define-key
   :prefix "C-c"
   :infix "a"
   "f"     '(counsel-faces :which-key "faces")
   "t"     '(counsel-load-theme :which-key "theme")
   "p"     '(counsel-package :which-key "package")))

;;;; (b) Buffers

(general-define-key
 :prefix "C-c"
 :infix "b"
 "C-c b" '(-switch-to-last-buffer :which-key "last")
 "TAB"   '(-switch-to-last-buffer :which-key "last")
 "k"     '(kill-current-buffer :which-key "kill")
 "n"     '(next-buffer :which-key "next")
 "p"     '(previous-buffer :which-key "prev")
 "r"     '(revert-buffer :which-key "revert")
 "t"     '(-temp-buffer :which-key "temp"))

;;;; (e) Editing

(general-define-key
 :prefix "C-c"
 :infix "e"
 "s" '(sort-lines :which-key "sort")
 "a" '(align-current :which-key "align")
 "c" '(-cleanup :which-key "cleanup")
 "r" '(align-regexp)
 "i" '(-iedit-ensure-function :which-key "iedit"))

;;;; (f) Find

(general-define-key
 :prefix "C-c"
 :infix "f"
 "C-c f" 'find-file
 "f"     'find-file
 "r"     '(consult-ripgrep :which-key "ripgrep")
 "g"     '(consult-git-grep :which-key "git-grep")
 ;; "t"     '(-counsel-todos :which-key "todos")
 )

;;;; (v) Version control

(general-define-key
 :prefix "C-c"
 :infix "v"
 "C-c v" '(magit-status :which-key "magit")
 "B"     '(magit-branch-popup :which-key ">branch")
 "d"     '(magit-ediff-dwim :which-key "diff dwim")
 "D"     '(magit-diff-popup :which-key ">diff")
 "f"     '(magit-pull :which-key "pull")
 "C-c f" '(magit-pull-popup :which-key ">pull")
 "g"     '(magit-status :which-key "magit")
 "l"     '(magit-log-all :which-key "log")
 "C-c l" '(magit-log-popup :which-key ">log")
 "p"     '(magit-push :which-key "push")
 "C-c p" '(magit-push-popup :which-key ">push")
 "s"     '(magit-stage-file :which-key "stage")
 "t"     '(git-timemachine :which-key "timemachine"))

;;;; (t) Toggles

(general-define-key
 :prefix "C-c"
 :infix "t"
 "a" '(auto-fill-mode :which-key "aufto-fill")
 "d" '(toggle-debug-on-error :which-key "debug")
 "s" '(flyspell-mode :which-key "spell")
 "p" '(flyspell-prog-mode :which-key "spell-prog")
 "r" '(read-only-mode :which-key "read-only")
 "t" '(toggle-truncate-lines :which-key "truncate")
 "v" '(visual-line-mode :which-key "visual-line"))

;;; Keybindings
;;;; Remappings

(bind-keys
 ("C-z"                           . nil)
 ("C-x C-z"                       . nil)
 ([remap exchange-point-and-mark] . -exchange-point-and-mark-no-activate)
 ([remap just-one-space]          . cycle-spacing))

;;;; Overrides

(bind-keys
 ("C-x o" . ace-window)
 ("C-w"   . -backward-kill-word-or-region))

;;; Global map

(when (equal 'ivy config-completion-system)
  (general-define-key
   :keymaps 'global
   "M-Q" '-unfill-paragraph))

;;; Local maps

(general-define-key
 :keymaps 'help-map
 "A"   'info-apropos
 "a"   'counsel-apropos
 ;; Help search
 "b"   'counsel-descbinds
 "e"   'counsel-el
 ;; Find variants
 "C-f" 'find-function
 "C-k" 'find-function-on-key
 "C-v" 'find-variable
 "C-l" 'find-library)

(which-key-add-key-based-replacements
  "C-c &" "shells"
  "C-c a" "apps"
  "C-c b" "buffers"
  "C-c d" "doc"
  "C-c e" "editing"
  "C-c f" "find"
  "C-c n" "notes"
  "C-c t" "toggles"
  "C-c v" "vc ")

(general-define-key
 :prefix "C-c"
 :keymaps 'global
 "j" '(avy-goto-char-timer :which-key "avy-char")
 "k" '(kill-this-buffer :which-key "kill-this-buffer"))

(provide 'config-keybindings)
;;; config-keybindings.el ends here
