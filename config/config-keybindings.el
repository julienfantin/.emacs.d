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
    ('darwin (setq mac-command-modifier 'control
                   mac-control-modifier 'meta
                   mac-option-modifier 'super
                   mac-function-modifier 'hyper))))

(when config-keybdings-default-setup
  (funcall config-keybdings-default-setup))

;; ** Hyper remapping

(defvar config-keybindings-keys
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()-=[]{};'\\:\"|,./<>?`~+")

(defun config-keybindings-remap-hyper-to-C-c ()
  "Remap Hyper single-key bindings to C-c.

This allows quicker access to user-defined mappings, and enables
different hardware remapping tricks like treating the spacebar as
hyper when it's used as a modifier."
  (dolist (char (string-to-list config-keybindings-keys))
    (let ((s (char-to-string char)))
      (define-key input-decode-map
        (kbd (format "H-%s" s))
        (kbd (format "C-c %s" s))))))

(when config-keybdings-hyper-to-C-c
  (config-keybindings-remap-hyper-to-C-c))


;; * Packages

(use-package free-keys :ensure t :defer t)
(use-package which-key
  :ensure t
  :defer t
  :init (after-init #'which-key-mode)
  :config (setq which-key-popup-type 'minibuffer))

(use-package keyfreq
  :ensure t
  :defer t
  :init
  :preface
  (defvar keyfreq-file (user-var-file "keyfreq"))
  :init
  (progn
    (after-init #'keyfreq-mode)
    (after-init #'keyfreq-autosave-mode)))

(use-package hydra
  :ensure t
  :defer t
  :config (setq hydra-lv nil))

(use-package interaction-log :ensure t :defer t :commands interaction-log-mode)


;; * Hydras
;; ** Bookmarks

(defhydra hydra-bm
    (:color red :hint nil
     :body-pre
     (progn
       (require 'bm)
       (when (not (use-region-p))
         (push-mark))))
  "
Bookmarks

_s_: ?s? persistence

    ^Mark^                            ^Navigation^
^^^^------------------------------------------------
_C-c m_: toggle                       _n_: next
    _a_: annotate                     _N_: next (lifo)
    _d_: delete all (current buffer)  _p_: prev
    _D_: delete ALL                   _P_: prev (lifo)"
  ("C-c m" bm-toggle)
  ("a"   bm-bookmark-annotate :color blue)
  ("n"   bm-common-next)
  ("N"   bm-lifo-next)
  ("p"   bm-common-previous)
  ("P"   bm-lifo-previous)
  ("s"   bm-toggle-buffer-persistence (if bm-buffer-persistence "[x]" "[ ]"))
  ("d"   bm-remove-all-current-buffer :color blue)
  ("D"   bm-remove-all-all-buffers :color blue)
  ("SPC" pop-to-mark-command)
  ("q"   :color blue))

;; ** Buffers

(defhydra hydra-buffers (:color blue :hint nil)
  "
Buffers

^Switch^             ^Kill
^^^---------------------------------------
_TAB_: last buffer   _k_: kill this buffer
  _o_: last window   _K_: kill window too
  _b_: switch
  _n_: next
  _p_: prev
  _h_: bury
  _t_: temp buffer
"
  ("C-c b" -switch-to-last-buffer)
  ("TAB" -switch-to-last-buffer)
  ("w" hydra-windows/body :exit t)
  ("b" ivy-switch-buffer)
  ("k" kill-this-buffer :color red)
  ("K" kill-buffer-and-window :color red)
  ("t" -temp-buffer)
  ("o" -switch-to-last-window)
  ("n" next-buffer :color red)
  ("p" previous-buffer :color red)
  ("h" bury-buffer :color red)
  ("u" revert-buffer "revert"))

;; ** Editing

(defvar hydra-multiple-cursors-lispy-p nil)

(defhydra hydra-multiple-cursors (:color red :hint nil)
  "
Multiple Cursors

Mark^       ^Unmark^           ^Insert
^^^^^---------------------------------------
_n_: next   _N_: next           _i_: numbers
_p_: prev   _P_: prev
_m_: all    _h_: hide unmarked
_s_: skip
_x_: more"
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

(defhydra hydra-undo (:color red)
  "Undo"
  ("p" undo-tree-undo "undo")
  ("u" undo-tree-undo "undo")
  ("n" undo-tree-redo "redo")
  ("r" undo-tree-redo "redo")
  ("SPC" undo-tree-visualize "tree" :exit t))

(defhydra hydra-edit (:color blue)
  "Edit"
  ("C-c e" hydra-multiple-cursors/body "MC")
  ("s" sort-lines "sort lines")
  ("m" hydra-multiple-cursors/body "MC")
  ("a" align-current "align")
  ("c" -cleanup "cleanup")
  ("o" outorg-edit-as-org "as org"))

;; ** Find

(defhydra hydra-find (:color blue :hint nil)
  "
Find

^Find files^      ^Project^
^^^^----------------------------
_f_: find file    _p_: find-file
_d_: dired        _a_: ag
^^                _g_: git grep"
  ("C-c f" counsel-find-file)
  ("f"     counsel-find-file)
  ("p"     projectile-find-file)
  ("g"     counsel-git-grep)
  ("a"     counsel-ag)
  ("d"     dired-jump))

;; ** Project

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-project (:color blue :hint nil)
  "
Project: %(projectile-project-root)

    _p_: switch project
_C-c p_: switch project
_<tab>_: <other window>


     ^^Find File          ^^Search/Tags         ^^Buffers              ^^Cache
-----------------------------------------------------------------------------------------
_C-c f_: file            _a_: ag               _i_: Ibuffer           _c_: cache clear
   _ff_: file dwim       _o_: multi-occur      _b_: switch to buffer  _x_: remove known project
   _fd_: file curr dir   ^^                _C-c k_: Kill all buffers  _X_: cleanup non-existing
    _r_: recent file                                              ^^^^_z_: cache current
    _d_: dir
"
  ("SPC" projectile-command-map)
  ("C-c p" -layouts-projectile-switch)
  ("C-c f" projectile-find-file)
  ("C-c k" projectile-kill-buffers)
  ("a" projectile-ag)
  ("b" projectile-switch-to-buffer)
  ("c" projectile-invalidate-cache)
  ("d" projectile-find-dir)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("i"   projectile-ibuffer)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("<tab>" hydra-projectile-other-window/body :color blue)
  ("q"   nil "cancel" :color blue))

;; ** VC

(defhydra hydra-vc (:color blue :hint nil)
  "
Version control

   ^^Actions          ^^Popups
------------------------------
_f_: pull          _F_: pull
_p_: push          _P_: push
_l_: log           _L_: log
_d_: diff          _D_: diff
_s_: stage         _B_: branch
_t_: timemachine
_g_: status"
  ("C-c v" magit-status)
  ("f" magit-pull :color red)
  ("F" magit-pull-popup)
  ("p" magit-push :color red)
  ("P" magit-push-popup)
  ("l" magit-log-all)
  ("L" magit-log-popup)
  ("d" magit-ediff-dwim)
  ("D" magit-diff-popup)
  ("s" magit-stage-file :color red)
  ("B" magit-branch-popup)
  ("g" magit-status)
  ("t" git-timemachine))

;; ** Modes

(defun toggle-doc (mode)
  "Return the hydra toggle docstring for mode"
  (if (and (boundp mode) (symbol-value mode)) "[x]" "[ ]"))

(defhydra hydra-toggle (:color red :hint nil)
  "
Minor modes

_a_: ?a? auto-fill-mode
_d_: ?d? debug-on-error
_s_: ?s? flyspell
_f_: ?f? focus
_l_: ?l? hl-line
_n_: ?n? linum
_p_: ?p? flyspell-prog
_r_: ?r? read-only
_v_: ?v? visual-line"
  ("a" auto-fill-mode (toggle-doc 'auto-fill-mode))
  ("d" toggle-debug-on-error (toggle-doc 'toggle-debug-on-error))
  ("s" flyspell-mode (toggle-doc 'flyspell-mode))
  ("f" focus-mode (toggle-doc 'focus-mode))
  ("l" hl-line-mode (toggle-doc 'hl-line-mode))
  ("n" linum-mode (toggle-doc 'linum-mode))
  ("p" flyspell-prog-mode (toggle-doc 'flyspell-prog-mode))
  ("r" read-only-mode (toggle-doc 'read-only-mode))
  ("t" toggle-truncate-lines (toggle-doc 'toggle-truncate-lines))
  ("v" visual-line-mode (toggle-doc 'visual-line-mode))
  ("q" :exit t))

;; ** Notes

(defhydra hydra-notes (:color blue)
  "Notes"
  ("c" org-capture "capture")
  ("d" deft "deft"))

;; ** Org

(after 'org
  (bind-keys
   :map org-mode-map
   ("C-M-g"          . worf-goto)
   ("C-M-<return>"   . org-insert-subheading)
   ("C-M-S-<return>" . org-insert-todo-subheading)
   ("C-M-u"          . org-up-heading-safe)))

;; ** Term

(defhydra hydra-shell (:color blue)
  ("C-c t" eshell "eshell")
  ("e"     eshell "eshell")
  ("s"     shell "shell")
  ("t"     term "term")
  ("a"     ansi-term "ansi-term"))

;; ** Movement

(defhydra hydra-move (:color red)
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("v" scroll-up-command)
  ("s" swiper "swiper")
  ("j" avy-goto-char-timer "avy char")
  ;; Converting M-v to V here by analogy.
  ("M-v" scroll-down-command)
  ("l" recenter-top-bottom))

;; ** Misc

(defhydra hydra-text (:color red)
  ("+" -text-scale-increase)
  ("-" -text-scale-decrease))

;; ** Windows map

(defun aw-switch-window (n)
  "Switch to window at index 'N'."
  (when-let (window (nth n (aw-window-list)))
    (aw-switch-to-window window)))

(defhydra hydra-windows (:color red :hint nil)
  "
Windows
_TAB_: ⥃
  _/_:  ↺

^Switch^    ^Move^       ^Resize^     ^Move buffer^   ^Ace^                  ^Undo
-----------------------------------------------------------------------------------
_1_: 1      _p_: up      _P_: up      _M-p_: up       _o_: switch          _u_: undo
_2_: 2      _n_: down    _N_: down    _M-n_: down     _s_: swap            _r_: redo
_3_: 3      _b_: left    _B_: left    _M-b_: left     _d_: delete
_4_: 4      _f_: right   _F_: right   _M-f_: right
...9"
  ("C-c w" -switch-to-last-window)
  ("1" (aw-switch-window 0) :exit t)
  ("C-c 1" (aw-switch-window 0) :exit t)
  ("2" (aw-switch-window 1) :exit t)
  ("C-c 2" (aw-switch-window 1) :exit t)
  ("3" (aw-switch-window 2) :exit t)
  ("C-c 3" (aw-switch-window 2) :exit t)
  ("4" (aw-switch-window 3) :exit t)
  ("C-c 4" (aw-switch-window 3) :exit t)
  ("5" (aw-switch-window 4) :exit t)
  ("C-c 5" (aw-switch-window 4) :exit t)
  ("6" (aw-switch-window 5) :exit t)
  ("C-c 6" (aw-switch-window 5) :exit t)
  ("7" (aw-switch-window 6) :exit t)
  ("C-c 7" (aw-switch-window 6) :exit t)
  ("8" (aw-switch-window 7) :exit t)
  ("C-c 8" (aw-switch-window 7) :exit t)
  ("9" (aw-switch-window 8) :exit t)
  ("C-c 9" (aw-switch-window 8) :exit t)
  ("p" windmove-up)
  ("n" windmove-down)
  ("f" windmove-right)
  ("b" windmove-left)
  ("P" windresize-up)
  ("N" windresize-down)
  ("F" windresize-right)
  ("B" windresize-left)
  ("M-p" buf-move-up)
  ("M-n" buf-move-down)
  ("M-f" buf-move-right)
  ("M-b" buf-move-left )
  ("/" -window-split-toggle)
  ("TAB" -switch-to-last-window )
  ("o" ace-window)
  ("d" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook 'windows-hydra/body)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook 'windows-hydra/body)))
  ("u" winner-undo)
  ("r" winner-redo)
  ("<left>" winner-undo)
  ("<right>" winner-redo))

;; ** Layouts

(defhydra hydra-layouts (:color red :hint nil :body-pre (require 'config-layouts))
  "
Layouts %(config-layouts--current-layout-name)

_C-c l_: switch persp _b_: switch buffer

    ^Persp^              ^Layouts^
^^^^--------------------------------
_C-c <tab>_: last    _<tab>_: last
    _C-c n_: next        _n_: next
    _C-c p_: prev        _p_: prev
    _C-c r_: rename      _r_: rename
    _C-c k_: kill        _k_: kill
    _C-c 1_: 1           _1_: 1
    _C-c 2_: 2           _2_: 2
    _C-c 3_: 3           _3_: 3
        ^^...9           ^^...9"
  ("C-c l" -layouts :exit t)
  ("b" -layouts-current-layout-buffers)
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("C-c 1" -layouts-switch-to-1)
  ("C-c 2" -layouts-switch-to-2)
  ("C-c 3" -layouts-switch-to-3)
  ("C-c 4" -layouts-switch-to-4)
  ("C-c 5" -layouts-switch-to-5)
  ("<tab>" eyebrowse-last-window-config)
  ("C-c <tab>" -layouts-jump-to-last-layout)
  ("n" eyebrowse-next-window-config)
  ("C-c n" persp-next)
  ("p" eyebrowse-prev-window-config)
  ("C-c p" persp-prev)
  ("r" eyebrowse-rename-window-config)
  ("C-c r" persp-rename)
  ("k" eyebrowse-close-window-config)
  ("C-c k" (persp-kill (config-layouts--current-layout-name))))

;; ** Go to

(defhydra hydra-goto (:color red :hint nil)
  "
Goto

^Positions^    ^Edit^             ^Errors
-------------------------------------------------
_g_: line      _l_: last-change   _n_: next-error
_i_: imenu     _[_: prev-hunk     _p_: prev-error
_/_: link      _]_: next-hunk     _e_: errors"
  ;; Positions
  ("M-g" goto-line)
  ("g" avy-goto-line)
  ("/" link-hint-open-link)
  ("i" counsel-imenu)
  ;; Edits
  ("l" goto-last-change)
  ("[" diff-hl-previous-hunk :color blue)
  ("]" diff-hl-next-hunk :color blue)
  ;; Errors
  ("p" hydra-flycheck/flycheck-previous-error :color blue)
  ("n" hydra-flycheck/flycheck-next-error :color blue)
  ("e" flycheck-list-errors))

;; ** Flycheck

(defhydra hydra-flycheck
  (:pre
   (progn
     (setq hydra-lv t)
     (flycheck-list-errors))
   :post
   (progn
     (setq hydra-lv nil)
     (quit-windows-on "*Flycheck errors*")))
  "Errors

_a_: first _f_: filter
_n_: next  _l_: list
_p_: prev
"
  ("f" flycheck-error-list-set-filter)
  ("l" flycheck-list-errros :exit t)
  ("n" flycheck-next-error)
  ("p" flycheck-previous-error)
  ("a" flycheck-first-error))

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

(defun config-ahs-pre ()
  "Start `auto-highlight-symbol-mode` and highlight."
  (unless (bound-and-true-p auto-highlight-symbol-mode)
    (auto-highlight-symbol-mode 1))
  (ahs-highlight-now))

(defhydra hydra-ahs (:color red :pre (config-ahs-pre))
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

;; ** Outlines

(defhydra hydra-outline
  (:body-pre
   (outline-minor-mode 1)
   :color pink
   :hint nil)
  "
^Hide^                ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels        _a_: all         _u_: up
_t_: body             _e_: entry       _n_: next visible
_o_: other            _i_: children    _p_: previous visible
_c_: entry            _k_: branches    _f_: forward same level
_l_: leaves           _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Outshine
  ("TAB" outline-cycle)
  ("o" -swiper-outlines "swiper")
  ("^" outshine-sort-entries "sort entries")
  ("M-RET" outshine-insert-heading "insert")
  ("C-M-RET" -insert-sub-heading "insert>")
  ("i" outshine-imenu "imenu")
  (":" outshine-set-tags-command "tags")
  ("t" outshine-todo "todo")
  ;; Hide
  ("q" hide-sublevels)        ; Hide everything but the top-level headings
  ("t" hide-body)             ; Hide everything but headings (all body lines)
  ("o" hide-other)            ; Hide other branches
  ("c" hide-entry)            ; Hide this entry's body
  ("l" hide-leaves)           ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)          ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)              ; Show (expand) everything
  ("e" show-entry)            ; Show this heading's body
  ("i" show-children)         ; Show this heading's immediate child sub-headings
  ("k" show-branches)         ; Show all sub-headings under this heading
  ("s" show-subtree)          ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)               ; Up
  ("n" outline-next-visible-heading)     ; Next
  ("p" outline-previous-visible-heading) ; Previous
  ("f" outline-forward-same-level)       ; Forward - same level
  ("b" outline-backward-same-level)      ; Backward - same level
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
 ;; Minimizing emacs? What nonsense!
 ("C-z"                           . nil)
 ("C-x C-z"                       . nil)
 ;; Mark
 ([remap exchange-point-and-mark] . -exchange-point-and-mark-no-activate)
 ([remap just-one-space]          . cycle-spacing))

;; ** Overrides

(bind-keys
 ("M-g"   . hydra-goto/body)
 ("C-x o" . ace-window)
 ("C-w"   . -backward-kill-word-or-region)
 ("C-x 1" . zygospore-toggle-delete-other-windows))


;; * Local maps

(bind-keys
 :map     help-map
 ;; Add useful builtins
 ("A"   . info-apropos)
 ;; Help search
 ("b"   . counsel-descbinds)
 ("e"   . counsel-el)
 ;; Bind find variants on
 ("C-f" . find-function)
 ("C-k" . find-function-on-key)
 ("C-v" . find-variable)
 ("C-l" . find-library))

(bind-keys
 :map prog-mode-map
 ("M-n" . hydra-ahs/ahs-forward)
 ("M-p" . hydra-ahs/ahs-backward)
 ("M-i" . ahs-edit-mode))


;; * User bindings

(defvar config-map (make-sparse-keymap))
(define-prefix-command 'config-map)

(bind-keys*
 :prefix-map config-map
 :prefix "C-c"
 ("C-r" . ivy-resume)
 ;; Quick window switching
 ("1" . hydra-windows/lambda-1-and-exit)
 ("2" . hydra-windows/lambda-2-and-exit)
 ("3" . hydra-windows/lambda-3-and-exit)
 ("4" . hydra-windows/lambda-4-and-exit)
 ("5" . hydra-windows/lambda-5-and-exit)
 ("6" . hydra-windows/lambda-6-and-exit)
 ("7" . hydra-windows/lambda-7-and-exit)
 ("8" . hydra-windows/lambda-8-and-exit)
 ("9" . hydra-windows/lambda-9-and-exit)
 ;; User maps
 ("&" . hydra-shell/body)
 ("_" . hydra-undo/undo-tree-undo)
 ("+" . hydra-text/-text-scale-increase)
 ("-" . hydra-text/-text-scale-decrease)
 ("b" . hydra-buffers/body)
 ("e" . hydra-edit/body)
 ("f" . hydra-find/body)
 ("h" . hydra-move/body)
 ("j" . avy-goto-char-timer)
 ("k" . kill-this-buffer)
 ("l" . hydra-layouts/body)
 ("m" . hydra-bm/body)
 ("n" . hydra-notes/body)
 ("o" . hydra-outline/body)
 ("s" . swiper)
 ("S" . -swiper-at-point)
 ("t" . hydra-toggle/body)
 ("u" . hydra-undo/undo-tree-undo)
 ("v" . hydra-vc/body)
 ("w" . hydra-windows/body))

(provide 'config-keybindings)
;;; config-keybindings.el ends here
