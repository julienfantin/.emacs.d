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

;;; Keyboard
;;;; Modifiers

(use-package ns-win
  :straight nil
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'control)
  (mac-control-modifier 'meta)
  (mac-option-modifier 'super)
  (mac-function-modifier 'hyper))

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

;;; Keymaps

;;;; (b) Buffers

(general-define-key
 :prefix "C-c"
 :infix "b"
 "C-c b" '-switch-to-last-buffer
 "TAB"   '-switch-to-last-buffer
 "k"     'kill-current-buffer
 "n"     'next-buffer
 "p"     'previous-buffer
 "r"     'revert-buffer)

;;;; (e) Editing

(general-define-key
 :prefix "C-c"
 :infix "e"
 "s" 'sort-lines
 "a" 'align-current
 "c" '-cleanup
 "r" 'align-regexp
 "i" '-iedit-ensure-function)

;;;; (f) Find

(general-define-key
 :prefix "C-c"
 :infix "f"
 "C-c f" 'find-file
 "f"     'find-file
 "r"     'consult-ripgrep
 "g"     'consult-git-grep
 "t"     'consult-todos)

;;;; (v) Version control

(general-define-key
 :prefix "C-c"
 :infix "v"
 "C-c v" 'magit-status
 "B"     'magit-branch-popup
 "d"     'magit-ediff-dwim
 "D"     'magit-diff-popup
 "f"     'magit-pull
 "C-c f" 'magit-pull-popup
 "g"     'magit-status
 "l"     'magit-log-all
 "C-c l" 'magit-log-popup
 "p"     'magit-push
 "C-c p" 'magit-push-popup
 "s"     'magit-stage-file
 "t"     'git-timemachine)

;;; Keybindings
;;;; Remappings

(bind-keys
 ("C-x C-z"              . nil)
 ([remap just-one-space] . cycle-spacing))

;;; Global map

(general-define-key
 :keymaps 'global
 "M-Q" '-unfill-paragraph)

;;; Local maps

(general-define-key
 :keymaps 'help-map
 "A"   'info-apropos
 "C-f" 'find-function
 "C-k" 'find-function-on-key
 "C-v" 'find-variable
 "C-l" 'find-library)

(general-define-key
 :prefix "C-c"
 :keymaps 'global
 "j" 'avy-goto-char-timer
 "k" 'kill-this-buffer)

(provide 'config-keybindings)
;;; config-keybindings.el ends here
