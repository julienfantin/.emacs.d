(require 'hydra)

;; ---------------------------------------------------------------------
;; * Keyboard setup

(setq mac-command-modifier 'control
      mac-control-modifier 'meta
      mac-option-modifier 'super
      mac-pass-command-to-system nil
      mac-pass-control-to-system nil)

;; ---------------------------------------------------------------------
;; * Global Hydras
;; ** Toggles

(defhydra hydra-toggle (:color blue :columns 3)
  "Toggles"
  ("f" auto-fill-mode "auto-fill-mode")
  ("l" toggle-truncate-lines "toggle-truncate-lines")
  ("r" read-only-mode "read-only-mode"))

(defun hydra-fonts ()
  (interactive)
  (when (window-system)
    (hydra-fonts/body)))

;; ** Fonts

(defhydra hydra-frame (:color red :columns 3)
  "Frame"
  ("+" inc-transparency "Increase transparency")
  ("-" dec-transparency "Decrease transparency"))

(defhydra hydra-fonts (:color red :columns 3)
  "Fonts"
  ("+" (set-font-height 10) "Increase font height")
  ("-" (set-font-height -10) "Decrease font height")
  ("c" (set-font "Consolas") "Consolas")
  ("m" (set-font "Monaco") "Monaco")
  ("M" (set-font "Menlo") "Menlo")
  ("d" (set-font "DejaVu Sans Mono") "DejaVu Sans Mono")
  ("D" (set-font "Droid Sans Mono") "Droid Sans Mono")
  ("f" (set-font "Fira Code") "Fira Code")
  ("l" (set-font "Latin Modern Mono") "Latin modern")
  ("u" (set-font "Ubuntu Mono") "Ubuntu Mono")
  ("s" (set-font "Source Code Pro") "Source Code Pro")
  ("p" (set-font "PragmataPro") "Pragmata")
  ("i" (set-font "Inconsolata") "Inconsolata"))

;; ** Git

(defhydra git-hydra (:color blue :columns 3)
  "Git"
  ("s" magit-stage-file "stage file")
  ("g" magit-status "status")
  ("t" git-timemachine "time machine")
  ("d" magit-ediff-dwim "diff"))

;; ** Navigation

;; TODO	Make navigation	hydras context-aware:
;; http://oremacs.com/2015/06/30/context-aware-hydra/

(key-chord-define-global
 "hh"
 (defhydra hydra-error (:color red :columns 3)
   "goto-error"
   ("h" first-error "first")
   ("j" next-error "next")
   ("k" previous-error "prev")))

(defhydra hydra-goto (:color blue :columns 3)
  ("h" helm-org-in-buffer-headings "org headings")
  ("g" goto-line "goto-line")
  ("c" avy-goto-char-2 "avy-goto-char2")
  ("C" avy-goto-char "avy-goto-char")
  ("n" flycheck-next-error "flycheck-next-error" :color red)
  ("p" flycheck-prev-error "flycheck-previous-error" :color red)
  ("e" helm-flycheck "helm-flycheck")
  ("b" helm-bookmarks "helm-bookmarks")
  ("m" helm-all-mark-rings "helm mark rings")
  ("i" helm-semantic-or-imenu "helm imenu")
  ("[" diff-hl-previous-hunk "diff previous hunk" :color red)
  ("]" diff-hl-next-hunk "diff next hunk" :color red)
  ("s" swiper-helm "swiper")
  ("o" hydra-outline/body "navigate outline" :exit t))

;; ** Windows/screens/perspectives

(defhydra hydra-persp (:color red :columns 3 :hint "")
  "Screens, perspectives"
  ("1" simple-screen-0 "1")
  ("2" simple-screen-1 "2")
  ("3" simple-screen-2 "3")
  ("4" simple-screen-3 "4")
  ("s" persp-switch "switch")
  ("n" persp-next "next")
  ("f" persp-next "next")
  ("p" persp-prev "prev")
  ("b" persp-prev "prev")
  ("r" persp-rename "rename")
  ("c" persp-kill "kill")
  ("k" persp-remove-buffer "remove buffer")
  ("a" persp-add-buffer "add buffer")
  ("i" persp-import "import buffer"))

;; ** Clocking

(defhydra hydra-org-clock (:color blue :columns 3 :hint nil)
  "Clocking"
  ("i" org-clock-in "clock-in")
  ("o" org-clock-out "clock-out")
  ("c" org-clock-in-last "clock-in-last")
  ("e" org-clock-modify-effort-estimate "modify-effort-estimate")
  ("q" org-clock-cancel "cancel")
  ("g" org-clock-goto "clock-goto")
  ("d" org-clock-display "clock-display")
  ("r" org-clock-report "clock-report")
  ("?" (org-info "Clocking commands") "help"))

;; ** Outlines

(defhydra hydra-outline (:color pink :columns 3 :hint nil)
  "Outline navigation"
  ;; Hide
  ("q" outline-hide-sublevels "hide-sublevels")
  ("t" outline-hide-body "hide-body")
  ("o" outline-hide-other "hide-other")
  ("c" outline-hide-entry "hide-entry")
  ("l" outline-hide-leaves "hide-leaves")
  ("d" outline-hide-subtree "hide-subtree")
  ;; Show
  ("a" outline-show-all "show-all")
  ("e" outline-show-entry "show-entry")
  ("i" outline-show-children "show-children")
  ("k" outline-show-branches "show-branches")
  ("s" outline-show-subtree "show-subtree")
  ;; Move
  ("h" helm-org-in-buffer-headings "helm-org")
  ("u" outline-up-heading "up-heading")
  ("n" outline-next-visible-heading "next-visible-heading")
  ("p" outline-previous-visible-heading "previous-visible-heading")
  ("f" outline-forward-same-level "forward-same-level")
  ("b" outline-backward-same-level "backward-same-level")
  ;; Edit
  ("i" outline-insert-heading "insert-heading" :exit t)
  ("<left>" orgstruct-hijacker-outline-promote-1 "promote")
  ("<right>" orgstruct-hijacker-outline-demote-1 "demote")
  ("z" nil "leave"))

;; ** Editing

(defvar hydra-multiple-cursors-lispy-p nil)

(defhydra hydra-multiple-cursors
  ;; Disable lispy while running the hydra, but make sure to
  ;; re-enable it on exit
  (:columns 3
	    :body-pre
	    (progn
	      (setq hydra-multiple-cursors-lispy-p (bound-and-true-p lispy-mode))
	      (when hydra-multiple-cursors-lispy-p (lispy-mode -1)))
	    :after-exit
	    (progn
	      (when hydra-multiple-cursors-lispy-p (lispy-mode))
	      (setq hydra-multiple-cursors-lispy-p nil)))
  "Multiple cursors"
  ("a"   mc/mark-all-like-this-dwim "mark all" :exit t)
  ("n"   mc/mark-next-like-this "mark next")
  ("N"   mc/skip-to-next-like-this "skip next")
  ("M-n" mc/unmark-next-like-this "unmark next")
  ("p"   mc/mark-previous-like-this "mark previous")
  ("P"   mc/skip-to-previous-like-this "skip previous")
  ("M-p" mc/unmark-previous-like-this "unmark previous")
  ("g"   mc/mark-all-in-region-regexp "mark all in region" :exit t)
  ("x"   mc/mark-more-like-this-extended "mark more")
  ("C-g" mc/keyboard-quit "quit" :exit t)
  ("q"   mc/keyboard-quit "quit" :exit t))

;; ---------------------------------------------------------------------
;; * Global keychords

(defun outline-up-cycle ()
  (interactive)
  (ignore-errors
    (outline-up-heading)
    (outline-cycle)))

(bind-keys
 ("C-o"       . aya-open-line)
 ;; Hydras
 ("C-z"       . hydra-persp/body)
 ("C-c t"     . hydra-toggle/body)
 ("C-c f"     . hydra-fonts/body)
 ("C-c g"     . git-hydra/body)
 ("M-g"       . hydra-goto/body)
 ;; Helm
 ("M-x"       . helm-M-x)
 ("C-x C-m"   . execute-extended-command)
 ("C-x b"     . helm-mini)
 ("C-x C-f"   . helm-find-files)
 ("M-y"       . helm-show-kill-ring)
 ("C-c C-b"   . helm-descbinds)
 ("C-c SPC"   . helm-browse-project)
 ("C-x o"     . ace-window)
 ;; Editing
 ("C-w"       . esk/backward-kill-word)
 ("C-x u"     . undo-tree)
 ("C-c m"     . hydra-multiple-cursors/body)
 ("M-z"       . avy-zap-to-char-dwim)
 ("M-Z"       . avy-zap-up-to-char-dwim)
 ("C-c s"     . avy-goto-char-2)
 ("M-<tab>"   . esk-alternate-buffer)
 ("S-<tab>"   . esk-alternate-window)
 ("C-x 1"     . zygospore-toggle-delete-other-windows)
 ("C-c C-n"   . org-capture)
 ("s-<tab>"   . outline-cycle)
 ("M-s-<tab>" . outline-up-cycle))

;; ** Super

(bind-keys
 ;; Buffers
 ("s-c" . kill-this-buffer)
 ("s-h" . bury-buffer)
 ("s-b" . previous-buffer)
 ("s-n" . next-buffer)
 ;; Window movements
 ("s-h" . windmove-left)
 ("s-j" . windmove-down)
 ("s-k" . windmove-up)
 ("s-l" . windmove-right))

;; ---------------------------------------------------------------------
;; * Prog mode

(bind-keys
 :map prog-mode-map
 ("C-c d" . helm-dash-at-point)
 ("M-n"   . highlight-symbol-next)
 ("M-p"   . highlight-symbol-prev)
 ("M-;"   . evilnc-comment-or-uncomment-lines)
 ("C-c e" . poporg-dwim)
 ("C-o"   . aya-open-line)
 ("M-i"   . mc/mark-all-symbols-like-this-in-defun)
 ("C-;"   . mc/mark-all-symbols-like-this))

(defhydra hydra-next-error
  (global-map "C-x")
  "
Compilation errors:
_n_: next error        _u_: first error    _q_uit
_p_: previous error    _l_: last error
"
  ("`" next-error     nil)
  ("n" next-error     nil :bind nil)
  ("p" previous-error nil :bind nil)
  ("u" first-error    nil :bind nil)
  ("l" (condition-case err
           (while t
             (next-error))
         (user-error nil))
   nil :bind nil)
  ("q" nil            nil :color blue))

(provide 'keybindings)
