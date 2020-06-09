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

;; * TODO more maps
;; mark map (pop marks etc)
;; eval map
;; search map
;; spell-check map
;; * TODO config-bind macro
;; like bind-keys, but:
;; - allow inline hydra defs
;; - allow selectively mapping hydra keys to the parent map
;; - provide names to single keys and hook into which key display var
;; - see next point
;; * TODO look into binding C-c `*` C-c `*` to a "default" map command
;;

;;; Code:
(require 'use-package)
(require 'cl-lib)


;; * Customs

(defvar config-keybdings-hyper-to-C-c t)
(defvar config-keybdings-default-setup #'config-keybindings-macbook)


;; * Keyboard
;; ** Modifiers

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

;; ** Hyper remapping

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


;; * Packages

(use-package general :straight t)

(use-package free-keys :straight t)

(use-package which-key
  :straight t
  :init (after-init #'which-key-mode)
  :custom
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-side-window-max-width 0.4)
  (which-key-idle-delay 0.4))

(use-package keyfreq
  :straight t
  :after no-littering
  :init
  (progn
    (after-init #'keyfreq-mode)
    (after-init #'keyfreq-autosave-mode))
  :custom
  (keyfreq-excluded-commands
   '(self-insert-command
     outshine-self-insert-command
     org-self-insert-command
     abort-recursive-edit
     forward-char
     backward-char
     previous-line
     next-line)))

(use-package hydra
  :straight  t
  :config
  (use-package lv
    :custom
    (lv-use-separator t)))


;; * Keymaps

;; ** (a) App

(general-define-key
 :prefix "C-c"
 :infix "a"
 "f"     '(counsel-faces :which-key "faces")
 "t"     '(counsel-load-theme :which-key "theme")
 "p"     '(counsel-package :which-key "package"))

;; ** (b) Buffers

(defhydra hydra-buffers (:color red)
  "Buffers"
  ("TAB"   -switch-to-last-buffer "last")
  ("b"     switch-to-buffer "switch")
  ("h"     bury-buffer "hide")
  ("k"     kill-current-buffer "kill")
  ("K"     kill-buffer-and-window "kill (window)")
  ("n"     next-buffer "next")
  ("p"     previous-buffer "prev")
  ("r"     revert-buffer "revert"))

(hydra-set-property 'hydra-buffers :verbosity 1)

(general-define-key
 :prefix "C-c"
 :infix "b"
 "C-c b" '(-switch-to-last-buffer :which-key "last")
 "TAB"   '(-switch-to-last-buffer :which-key "last")
 "K"     '(hydra-buffers/kill-buffer-and-window :which-key "kill (window)")
 "b"     '(ivy-switch-buffer :which-key "buffers")
 "h"     '(hydra-buffers/bury-buffer :which-key "bury")
 "k"     '(hydra-buffers/kill-current-buffer :which-key "kill")
 "n"     '(hydra-buffers/next-buffer :which-key "next")
 "p"     '(hydra-buffers/previous-buffer :which-key "prev")
 "r"     '(revert-buffer :which-key "revert")
 "t"     '(-temp-buffer :which-key "temp"))

;; ** (e) Editing

(defvar hydra-multiple-cursors-lispy-p nil)

(defhydra hydra-multiple-cursors (:color red :hint nil)
  ("m"   mc/mark-all-like-this-dwim :exit t)
  ("i"   mc/insert-numbers)
  ("n"   mc/mark-next-like-this "next")
  ("N"   mc/unmark-next-like-this "-next")
  ("s"   mc/skip-to-next-like-this "skip next")
  ("p"   mc/mark-previous-like-this "-prev")
  ("P"   mc/unmark-previous-like-this "prev")
  ("x"   mc/mark-more-like-this-extended "more")
  ("h"   mc-hide-unmatched-lines-mode "hide")
  ("C-g" mc/keyboard-quit "quit" :exit t))

(general-define-key
 :prefix "C-c"
 :infix "e"
 "C-c e" '(hydra-multiple-cursors/body :which-key "mc")
 "s" '(sort-lines :which-key "sort")
 "m" '(hydra-multiple-cursors/body :which-key "mc")
 "a" '(align-current :which-key "align")
 "c" '(-cleanup :which-key "cleanup")
 "o" '(outorg-edit-as-org :which-key "outorg")
 "r" '(align-regexp)
 "i" '(iedit-mode :which-key "iedit"))

;;  ** (f) Find

(general-define-key
 :prefix "C-c"
 :infix "f"
 "C-c f" '(counsel-find-file :which-key "find-file")
 "f"     '(counsel-find-file :which-key "find-file")
 "h"     '(helm-hunks :which-key "hunks")
 "e"     '(-counsel-flycheck :which-key "flycheck")
 "r"     '(counsel-rg :which-key "ripgrep")
 "g"     '(counsel-git-grep :which-key "git-grep")
 "p"     '(projectile-find-file :which-key "(projectile) find-file")
 "t"     '(-counsel-todos :which-key "todos"))

;; ** (v) Version control

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

;; ** (t) Toggles

(general-define-key
 :prefix "C-c"
 :infix "t"
 "a" '(auto-fill-mode :which-key "aufto-fill")
 "d" '(toggle-debug-on-error :which-key "debug")
 "s" '(flyspell-mode :which-key "spell")
 "p" '(flyspell-prog-mode :which-key "spell-prog")
 "h" '(hl-line-mode :which-key "hl-line")
 "r" '(read-only-mode :which-key "read-only")
 "t" '(toggle-truncate-lines :which-key "truncate")
 "v" '(visual-line-mode :which-key "visual-line"))

;; ** (n) Notes

(general-define-key
 :prefix "C-c"
 :infix "n"
 "c" '(org-capture :which-key "capture")
 "j" '(org-journal-new-entry :which-key "journal"))

;; ** (t) Term

(general-define-key
 :prefix "C-c"
 :infix "&"
 "C-c &" 'eshell
 "e" 'eshell
 "s" 'shell
 "t" 'term
 "a" 'ansi-term)

;; ** (w) Windows

(defun config-keybindings-ace-switch ()
  "Switch the current window with ace window and restart the hydra."
  (interactive)
  (ace-window 4)
  (add-hook 'ace-window-end-once-hook 'hydra-windows/body))

(defun config-keybindings-ace-delete ()
  "Delete a window with ace window and restart the hydra."
  (interactive)
  (ace-window 4)
  (add-hook 'ace-window-end-once-hook 'hydra-windows/body))

(defun config-keybindings-init-window-modes ()
  (require 'ace-window nil t)
  (when (not (featurep 'windresize))
    (windresize)
    (windresize-cancel-and-quit)))

(defhydra hydra-eyebrowse (:color red :hint nil)
  "Eyebrowse"
  ("<tab>" eyebrowse-last-window-config "last")
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)
  ("w" eyebrowse-switch-to-window-config "switch")
  ("r" eyebrowse-rename-window-config "rename")
  ("c" eyebrowse-new-workspace "new")
  ("n" eyebrowse-next-window-config "next")
  ("p" eyebrowse-prev-window-config "prev")
  ("k" eyebrowse-close-window-config "kill"))

(defhydra hydra-windows
  (:color red :hint nil :pre (config-keybindings-init-window-modes))
  ("<tab>" -switch-to-last-window "last" :exit t)
  ("C-c w" -switch-to-last-window "last" :exit t)
  ("w"   hydra-eyebrowse/body :exit t)
  ("p"   windmove-up)
  ("n"   windmove-down)
  ("f"   windmove-right)
  ("b"   windmove-left)
  ("P"   windresize-up)
  ("N"   windresize-down)
  ("F"   windresize-right)
  ("B"   windresize-left)
  ("M-p" buf-move-up)
  ("M-n" buf-move-down)
  ("M-f" buf-move-right)
  ("M-b" buf-move-left)
  ("/"   -window-split-toggle "split")
  ("o"   ace-window "ace")
  ("k"   config-keybindings-ace-delete "ace delete")
  ("s"   config-keybindings-ace-switch "ace switch")
  ("u"   winner-undo "undo")
  ("r"   winner-redo "redo")
  ("q"   ignore :exit t))

(dolist (n (number-sequence 1 10))
  (eval
   `(defun ,(intern (format "aw-switch-to-window-%s" n)) ()
      (interactive)
      ,(format "Switch to window at index %s" n)
      (when-let (window (nth (- ,n 1) (aw-window-list)))
        (aw-switch-to-window window)))))

(general-define-key
 :prefix "C-c"
 "TAB" '(-switch-to-last-window :which-key "window-last")
 "1" '(aw-switch-to-window-1 :which-key "window-1")
 "2" '(aw-switch-to-window-2 :which-key "window-2")
 "3" '(aw-switch-to-window-3 :which-key "window-3")
 "4" '(aw-switch-to-window-4 :which-key "window-4")
 "5" '(aw-switch-to-window-5 :which-key "window-5")
 "6" '(aw-switch-to-window-6 :which-key "window-6")
 "7" '(aw-switch-to-window-7 :which-key "window-7")
 "8" '(aw-switch-to-window-8 :which-key "window-8")
 "9" '(aw-switch-to-window-9 :which-key "window-9")
 "p" '(projectile-command-map :which-key "projectile"))

;; ** (g) Goto

(defvar hydra-goto-pre-pos nil)

(defun hydra-goto-init ()
  (setq hydra-goto-pre-pos (point)))

(defun hydra-goto-reset ()
  (interactive)
  (goto-char hydra-goto-pre-pos))

(defhydra hydra-goto
  (:color blue :body-pre (hydra-goto-init))
  ;; Positions
  ("C-g" hydra-goto-reset :exit t)
  ("M-g" goto-line "line")
  ("g" avy-goto-line "avy-line")
  ("/" link-hint-open-link "link")
  ("i" counsel-imenu "imenu")
  ("I" ivy-imenu-anywhere "imenu-anywhere")
  ("e" -counsel-flycheck "flycheck")
  ;; Pages
  ("p" ivy-pages "pages")
  ("[" backward-page "back-page" :exit nil)
  ("]" forward-page "forw-page" :exit nil))

;; ** (d) Documentation

(general-define-key
 :prefix "C-c"
 :infix "d"
 "SPC" 'counsel-dash)

;; ** (o) Outlines

(defhydra hydra-outline
  (:body-pre (outline-minor-mode 1) :color pink)
  "Outline"
  ;; Outshine
  ("C-c o" -swiper-outlines "swiper")
  ("o" -swiper-outlines "swiper")
  ("TAB" outline-cycle)

  ("^" outshine-sort-entries "sort entries")
  ("C-j" outshine-insert-heading "insert" :exit t)
  ("C-M-j" -insert-sub-heading "insert>" :exit t)
  ("i" outshine-imenu "imenu")
  (":" outshine-set-tags-command "tags")
  ("t" outshine-todo "todo")
  ;; Move
  ("u" outline-up-heading "up")
  ("n" outline-next-visible-heading "next")
  ("p" outline-previous-visible-heading "prev")
  ("f" outline-forward-same-level "forward-same")
  ("b" outline-backward-same-level "backward-same")
  ("q" nil))


;; * Keybindings
;; ** Helpers

(defun config-keybindings-unbind-keymap (keymap)
  "Unbind all keys in 'KEYMAP'."
  (map-keymap
   (lambda (key f)
     (define-key keymap (vector key) nil))
   keymap))

;; ** Remappings

(bind-keys
 ("C-z"                           . nil)
 ("C-x C-z"                       . nil)
 ([remap exchange-point-and-mark] . -exchange-point-and-mark-no-activate)
 ([remap just-one-space]          . cycle-spacing))

;; ** Overrides

(bind-keys
 ("M-g"   . hydra-goto/body)
 ("C-x o" . ace-window)
 ("C-w"   . -backward-kill-word-or-region)
 ("C-x 1" . zygospore-toggle-delete-other-windows))


;; * Global map

(general-define-key
 :keymaps 'global
 "M-Q" '-unfill-paragraph
 "C-x C-r" 'ivy-recentf)


;; * Local maps

(general-define-key
 :keymaps 'help-map
 ;; Add useful builtins
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

(use-package org
  :bind
  ((:map org-mode-map
         ("C-M-<return>"   . org-insert-subheading)
         ("C-M-S-<return>" . org-insert-todo-subheading)
         ("C-M-u"          . org-up-heading-safe))))


;; * Keymaps

(which-key-add-key-based-replacements
  "C-c &" "shells"
  "C-c a" "apps"
  "C-c b" "buffers"
  "C-c d" "doc"
  "C-c e" "editing"
  "C-c f" "find"
  "C-c g" "goto"
  "C-c n" "notes"
  "C-c o" "outlines"
  "C-c t" "toggles"
  "C-c v" "vc "
  "C-c w" "windows")

(general-define-key
 :prefix "C-c"
 :keymaps 'global
 "r" 'ivy-resume
 "g" '(hydra-goto/body :which-key "goto")
 "i" '(counsel-imenu :which-key "imenu")
 "I" '(ivy-imenu-anywhere :which-key "imenu anywhere")
 "j" '(avy-goto-char-timer :which-key "avy-char")
 "k" '(kill-this-buffer :which-key "kill-this-buffer")
 "o" '(hydra-outline/body :which-key "hydra-outline")
 "s" '(swiper-all :which-key "swiper-all")
 "S" '(-swiper-at-point :which-key "-swiper-at-point")
 "w" '(hydra-windows/body :which-key "hydra-windows"))

(provide 'config-keybindings)
;;; config-keybindings.el ends here
