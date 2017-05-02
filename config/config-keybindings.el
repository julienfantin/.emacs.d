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
(require 'use-config)
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
    ('darwin (validate-setq mac-command-modifier 'control
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

(use-package general :ensure t)

(use-package free-keys :ensure t :defer t)

(use-package which-key
  :ensure t
  :defer t
  :init (after-init #'which-key-mode)
  :config
  (validate-setq
   which-key-sort-order 'which-key-key-order-alpha
   which-key-side-window-max-width 0.4
   which-key-idle-delay 0.4))

(use-package keyfreq
  :ensure t
  :defer t
  :init
  :preface
  (defvar keyfreq-file (user-var-file "keyfreq"))
  :init
  (progn
    (after-init #'keyfreq-mode)
    (after-init #'keyfreq-autosave-mode))
  :config
  (validate-setq
   keyfreq-excluded-commands
   '(self-insert-command
     outshine-self-insert-command
     org-self-insert-command
     abort-recursive-edit
     forward-char
     backward-char
     previous-line
     next-line)))

(use-package hydra
  :ensure t
  :defer t
  :config
  (use-package lv
    :config
    (validate-setq lv-use-separator t)))

(use-package interaction-log :ensure t :defer t :commands interaction-log-mode)


;; * Hydras
;; ** (b)uffers

(defhydra hydra-buffers (:color red)
  "Buffers"
  ("C-c b" -switch-to-last-buffer "last")
  ("TAB"   -switch-to-last-buffer "last")
  ("`"     -switch-to-last-buffer "last")
  ("h"     bury-buffer "hide")
  ("k"     kill-this-buffer "kill")
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
 "k"     '(hydra-buffers/kill-this-buffer :which-key "kill")
 "n"     '(hydra-buffers/next-buffer :which-key "next")
 "p"     '(hydra-buffers/previous-buffer :which-key "prev")
 "r"     '(revert-buffer :which-key "revert")
 "t"     '(-temp-buffer :which-key "temp"))

;; ** (e)diting

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
 "r" '(align-regexp))

;;  ** (f)ind

(general-define-key
 :prefix "C-c"
 :infix "f"
 "C-c f" '(counsel-find-file :which-key "find-file")
 "f"     '(counsel-find-file :which-key "find-file")
 "r"     '(counsel-rg :which-key "ripgrep")
 "g"     '(counsel-git-grep :which-key "git-grep")
 "p"     '(projectile-find-file :which-key "(projectile) find-file")
 "s"     '(-find-file-as-sudo :which-key "sudo"))

;; ** (v)ersion control

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

;; ** (t)oggle modes

(general-define-key
 :prefix "C-c"
 :infix "m"
 "a" 'auto-fill-mode
 "c" 'centered-window-mode
 "d" 'toggle-debug-on-error
 "s" 'flyspell-mode
 "f" 'focus-mode
 "h" 'hl-line-mode
 "l" 'linum-mode
 "n" 'linum-mode
 "p" 'flyspell-prog-mode
 "r" 'read-only-mode
 "t" 'toggle-truncate-lines
 "v" 'visual-line-mode)

;; ** (n)otes

(general-define-key
 :prefix "C-c"
 :infix "n"
 "c" '(org-capture :which-key "capture")
 "d" '(deft :which-key "deft")
 "p" '(org-projectile:project-todo-completing-read :which-key "project todo"))

;; ** Term

(general-define-key
 :prefix "C-c"
 :infix "&"
 "C-c &" 'eshell
 "e" 'eshell
 "s" 'shell
 "t" 'term
 "a" 'ansi-term)

;; ** (w)indows

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

(defhydra hydra-windows
  (:color blue :hint nil :pre (config-keybindings-init-window-modes))
  ("<tab>" -switch-to-last-window "last" :exit t)
  ("C-c w" -switch-to-last-window "last" :exit t)
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
  ("C-_" winner-undo "undo")
  ("M-_" winner-redo "redo")
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
 "9" '(aw-switch-to-window-9 :which-key "window-9"))

(defhydra hydra-eyebrowse (:color red)
  "Eyebrowse"
  ("<tab>" eyebrowse-last-window-config "last")
  ("w" eyebrowse-switch-to-window-config "switch")
  ("r" eyebrowse-rename-window-config "rename")
  ("c" eyebrowse-new-workspace "new")
  ("n" eyebrowse-next-window-config "next")
  ("p" eyebrowse-prev-window-config "prev")
  ("k" eyebrowse-close-window-config "kill"))

(general-define-key
 ;; Eyeberowse: Space+Ctrl+NumRow
 ;;
 "C-c l"    '(hydra-eyebrowse/body :which-key "eye")
 "C-H-1"    '(eyebrowse-switch-to-window-config-1 :which-key "eye-1")
 "C-H-2"    '(eyebrowse-switch-to-window-config-2 :which-key "eye-2")
 "C-H-3"    '(eyebrowse-switch-to-window-config-3 :which-key "eye-3")
 "C-H-4"    '(eyebrowse-switch-to-window-config-4 :which-key "eye-4")
 "C-H-5"    '(eyebrowse-switch-to-window-config-5 :which-key "eye-5")
 "C-H-6"    '(eyebrowse-switch-to-window-config-6 :which-key "eye-6")
 "C-H-7"    '(eyebrowse-switch-to-window-config-7 :which-key "eye-7")
 "C-H-8"    '(eyebrowse-switch-to-window-config-8 :which-key "eye-8")
 "C-H-9"    '(eyebrowse-switch-to-window-config-9 :which-key "eye-9"))

;; ** (g)oto

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
  ("M-g" goto-line)
  ("g" avy-goto-line)
  ("/" link-hint-open-link)
  ("i" counsel-imenu)
  ("e" counsel-flycheck)
  ;; Pages
  ("p" ivy-pages)
  ("[" backward-page)
  ("]" forward-page)
  ;; Edits
  ("l" goto-last-change)
  ("<" diff-hl-previous-hunk)
  (">" diff-hl-next-hunk))

(general-define-key
 :keymaps '(help-mode-map)
 "/" 'link-hint-open-link)

;; ** Symbols

(advice-add
 #'ahs-edit-mode :before
 (lambda (arg &optional temporary)
   (unless (bound-and-true-p auto-highlight-symbol-mode)
     (auto-highlight-symbol-mode 1))))

(advice-add
 #'ahs-edit-mode-off :after
 (lambda (nomsg interactive)
   (auto-highlight-symbol-mode -1)))

(defun config-keybindings-ahs-pre ()
  "Start `auto-highlight-symbol-mode` and highlight."
  (unless (bound-and-true-p auto-highlight-symbol-mode)
    (auto-highlight-symbol-mode 1))
  (ahs-highlight-now))

(defun config-keybindings-ahs-post ()
  "Start `auto-highlight-symbol-mode` and highlight."
  (when (bound-and-true-p auto-highlight-symbol-mode)
    (auto-highlight-symbol-mode -1)))

(defhydra hydra-ahs
  (:color red :pre (config-keybindings-ahs-pre) :post (config-keybindings-ahs-post))
  "Symbol"
  ("M-n" ahs-forward "next")
  ("n" ahs-forward "next")
  ("N" ahs-forward-definition "next def")
  ("M-p" ahs-backward "prev")
  ("p" ahs-backward "prev")
  ("P" ahs-backward-definition "prev def")
  ("r" ahs-change-range "range")
  ("<" ahs-back-to-start "back")
  ("e" ahs-edit-mode "edit" :exit t))

(comment
 (defhydra hydra-iedit (:color red)
   "Symbol"
   ("M-n" iedit-next-occurrence "next")
   ("n" iedit-next-occurrence "next")
   ("M-p" iedit-prev-occurrence "prev")
   ("p" iedit-prev-occurrence "prev")
   ("f" iedit-restrict-function "function")
   ("r" iedit-restrict-region "region")
   ("e" ahs-edit-mode "edit" :exit t)))

;; ** (tab)navigation

;; (defhydra hydra-pop (:color blue)
;;   ("n" pop-mark)
;;   ("C-c" pop-global-mark))

;; ** (d)ocumentation

(general-define-key
 :prefix "C-c"
 :infix "d"
 "SPC" 'counsel-dash)

;; ** (o)utlines

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

(after 'persp-mode (config-keybindings-unbind-keymap persp-key-map))

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
 "M-Q" '-unfill-paragraph)


;; * Local maps

(general-define-key
 :keymaps 'help-map
 ;; Add useful builtins
 "A"   'info-apropos
 ;; Help search
 "b"   'counsel-descbinds
 "e"   'counsel-el
 ;; Find variants
 "C-f" 'find-function
 "C-k" 'find-function-on-key
 "C-v" 'find-variable
 "C-l" 'find-library)

(general-define-key
 :keymaps 'prog-mode-map
 "M-n" '(hydra-ahs/ahs-forward :which-key "ahs-forward")
 "M-p" '(hydra-ahs/ahs-backward :which-key "ahs-backward")
 "M-i" '(ahs-edit-mode :which-key "ahs-edit-mode"))

(comment
 (defun config-keybindings-iedit (&optional arg)
   (interactive)
   (if (or (bound-and-true-p lispy-mode)
           (bound-and-true-p lispy-mnemonic-mode))
       (funcall-interactively 'lispy-iedit arg)
     (funcall-interactively 'iedit arg)))

 (general-define-key
  :keymaps 'prog-mode-map
  "M-n" '(hydra-iedit/iedit-next-occurrence :which-key "iedit-next")
  "M-p" '(hydra-ahs/ahs-backward :which-key "iedit-prev")
  "M-i" '(config-keybindings-iedit :which-key "iedit")))

(after 'org
  (bind-keys
   :map org-mode-map
   ("C-M-g"          . worf-goto)
   ("C-M-<return>"   . org-insert-subheading)
   ("C-M-S-<return>" . org-insert-todo-subheading)
   ("C-M-u"          . org-up-heading-safe)))


;; * User bindings

(defhydra hydra-text (:color red)
  ("+" -text-scale-increase)
  ("-" -text-scale-decrease));; Mark

(general-define-key
 :prefix "C-c"
 :keymaps 'global
 "r" 'ivy-resume
 "+"   '(hydra-text/-text-scale-increase :which-key "text-+")
 "-"   '(hydra-text/-text-scale-decrease :which-key "text--")
 "g"   '(hydra-goto/body :which-key "goto")
 "j"   '(avy-goto-char-timer :which-key "avy-char")
 "k"   '(kill-this-buffer :which-key "kill-this-buffer")
 "o"   '(hydra-outline/body :which-key "hydra-outline")
 "s"   '(swiper-all :which-key "swiper-all")
 "S"   '(-swiper-at-point :which-key "-swiper-at-point")
 "w"   '(hydra-windows/body :which-key "hydra-windows"))

(provide 'config-keybindings)
;;; config-keybindings.el ends here
