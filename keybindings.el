(require 'use-package)
(require 'hydra)
(require 'commands)


;; * Packages

(use-package hydra :ensure t :defer t)
(use-package free-keys :ensure t :defer t)
(use-package keyfreq
  :ensure t
  :defer t
  :init
  (progn
    (after-init #'keyfreq-mode)
    (after-init #'keyfreq-autosave-mode)))


;; * Keyboard setup

(defun esk-laptop-bindings ()
  (interactive)
  (setq mac-command-modifier 'control
        mac-control-modifier 'meta
        mac-option-modifier 'super
        mac-pass-command-to-system nil
        mac-pass-control-to-system nil))

(defun esk-mech-bindings ()
  (interactive)
  (setq mac-command-modifier 'meta
        mac-control-modifier 'control
        mac-option-modifier 'super
        mac-pass-command-to-system nil
        mac-pass-control-to-system nil))

(esk-laptop-bindings)


;; * Hydras

;; ** Toggles

(defhydra hydra-toggle (:color blue :columns 3)
  "Toggles"
  ("n" linum-mode "linum-mode")
  ("l" hl-line-mode "hl-line-mode")
  ("f" auto-fill-mode "auto-fill-mode")
  ("t" toggle-truncate-lines "toggle-truncate-lines")
  ("r" read-only-mode "read-only-mode"))

;; ** Git

(defhydra hydra-git (:color blue :columns 3)
  "Git"
  ("s" magit-stage-file "stage file")
  ("g" magit-status "status")
  ("t" git-timemachine "time machine")
  ("d" magit-ediff-dwim "diff"))

;; ** Screens & perspectives

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

(comment
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
   ("?" (org-info "Clocking commands") "help")))

;; ** Outlines

(defhydra hydra-outline (:color red :columns 4)
  ;; Navigation
  ("n" outline-next-visible-heading     "next-visible-heading")
  ("p" outline-previous-visible-heading "previous-visible-heading")
  ("f" outline-forward-same-level       "forward-same-level")
  ("u" outline-up-heading               "up-heading")
  ("b" outline-backward-same-level      "backward-same-level")
  ("F" outshine-next-block              "next-block")
  ("B" outshine-previous-block          "previous-block")
  ("j" outshine-navi                    "navi" :exit t)
  ("i" outshine-imenu                   "imenu" :exit t)
  ;; Visibility
  ("a" outline-show-all                 "show-all")
  ("c" outline-cycle                    "cycle")
  ("C" outshine-cycle-buffer            "cycle-buffer")
  ("'" (lambda () (interactive) (outshine-use-outorg (quote org-display-outline-path) (quote WHOLE-BUFFER-P))) "outorg")
  ("r" outshine-narrow-to-subtree       "narrow-to-subtree")
  ("w" widen                            "widen")
  ;; Structure Editing
  ("U" outline-move-subtree-up          "move-subtree-up")
  ("D" outline-move-subtree-down        "move-subtree-down")
  ("+" outline-demote                   "demote")
  ("-" outline-promote                  "promote")
  ("M-RET" outshine-insert-heading      "insert-heading")
  ("C-RET" outshine-insert-heading      "insert-heading")
  ("^" outshine-sort-entries            "sort-entries")
  ("m" outline-mark-subtree             "mark-subtree")
  ("#" outshine-toggle-comment          "toggle-comment")
  ;; Clock Commands
  ("I" outshine-clock-in                "clock-in")
  ("O" outshine-clock-out               "clock-out")
  ;; Date & Time Commands
  ("." outshine-time-stamp              "time-stamp")
  ("!" outshine-time-stamp-inactive     "time-stamp-inactive")
  ("d" outshine-deadline                "deadline")
  ("s" outshine-schedule                "schedule")
  ;; Meta Data Editing
  ("t" outshine-todo                    "todo")
  ("," outshine-priority                "priority")
  ("0" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 32)))) "priority 32")
  ("1" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 65)))) "priority 65")
  ("2" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 66)))) "priority 66")
  ("3" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 67)))) "priority 67")
  (":" outshine-set-tags-command        "set-tags-command")
  ;; Misc
  ("o" outshine-open-at-point))

(defun esk-hydra-outline ()
  (interactive)
  (cond
   ((not outline-regexp) (message "outline-regexp isn't set"))
   ((looking-at-p outline-regexp)
    (call-interactively #'hydra-outline/body))
   (t (progn
        (call-interactively #'outline-previous-visible-heading)
        (call-interactively #'hydra-outline/body)))))

;; ** Multiple cursors

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
  ("p"   mc/mark-previous-like-this "mark previous")
  ("g"   mc/mark-all-in-region-regexp "mark all in region" :exit t)
  ("x"   mc/mark-more-like-this-extended "mark more")
  ("C-g" mc/keyboard-quit "quit" :exit t)
  ("q"   mc/keyboard-quit "quit" :exit t))


;; * Prog mode

(bind-keys
 :map       prog-mode-map
 ("C-c a" . esk-align-current)
 ("C-c c" . esk-cleanup)
 ("C-c d" . helm-dash-at-point)
 ("C-c e" . poporg-dwim)
 ("C-c '" . outorg-edit-as-org)
 ("C-c o" . esk-hydra-outline)
 ("M-n"   . highlight-symbol-next)
 ("M-p"   . highlight-symbol-prev)
 ("M-;"   . evilnc-comment-or-uncomment-lines)
 ("M-i"   . esk-iedit))


;; * Global bindings

;; ** C-c map

(bind-keys
 ("C-c t" . hydra-toggle/body)
 ("C-c g" . hydra-git/body)
 ("C-c u" . undo-tree)
 ("C-c m" . hydra-multiple-cursors/body)
 ("C-c n" . org-capture))

;; ** Helm map

(after helm
  (bind-keys
   :map helm-command-map
   ("b" . helm-descbinds)
   ("m" . helm-all-mark-rings)))

;; ** Globals and overrides

(bind-keys
 ;; Mark
 ([remap exchange-point-and-mark] . exchange-point-and-mark-no-activate)
 ;; Hydras
 ("C-z"                           . hydra-persp/body)
 ;; Replace defaults with Helm
 ("M-x"                           . helm-M-x)
 ("C-x C-m"                       . execute-extended-command)
 ("C-x b"                         . helm-mini)
 ("C-x C-f"                       . helm-find-files)
 ("M-y"                           . helm-show-kill-ring)
 ("C-x o"                         . ace-window)
 ;; Editing
 ([remap just-one-space]          . cycle-spacing)
 ("C-w"                           . esk-backward-kill-word)
 ("M-g M-g"                       . avy-goto-line)
 ("M-g g"                         . avy-goto-line)
 ("M-z"                           . avy-zap-to-char-dwim)
 ("M-Z"                           . avy-zap-up-to-char-dwim)
 ("M-<tab>"                       . esk-alternate-buffer)
 ("S-<tab>"                       . esk-alternate-window)
 ("C-x 1"                         . zygospore-toggle-delete-other-windows))

;; ** Super

(bind-keys
 ;; Buffers
 ("s-s"        . save-buffer)
 ("s-p"        . previous-buffer)
 ("s-h"        . bury-buffer)
 ("s-n"        . next-buffer)
 ("s-u"        . revert-buffer)
 ("s-x"        . kill-this-buffer)
 ("s-<return>" . helm-browse-project)
 ("s-<tab>"    . esk-alternate-buffer)
 ;; Window movements
 ("s-h"        . windmove-left)
 ("s-j"        . windmove-down)
 ("s-k"        . windmove-up)
 ("s-l"        . windmove-right)
 ("s-H"        . move-border-left)
 ("s-J"        . move-border-down)
 ("s-K"        . move-border-up)
 ("s-L"        . move-border-right)
 ;; Navigation
 ("s-`"        . push-mark-no-activate)
 ("s-SPC"      . jump-to-mark)
 ("s-<"        . goto-last-change)
 ("s->"        . goto-last-change-undo)
 ("s-["        . diff-hl-previous-hunk)
 ("s-]"        . diff-hl-next-hunk)
 ("s-i"        . helm-semantic-or-imenu)
 ("s-I"        . helm-imenu-anywhere)
 ("s-w"        . avy-goto-word-1)
 ("s-c"        . avy-goto-char)
 ("s-;"        . link-hint-open-link)
 ("s-o"        . outshine-imenu)
 ("s-e"        . esk-flycheck-list-errors)
 ("s-b"        . helm-mini)
 ("s-f"        . helm-find-files)
 ("s-g"        . lispy-goto)
 ("s-G"        . lispy-goto-local)
 ("s-m"        . helm-all-mark-rings)
 ("s-/"        . swiper-helm)
 ;; Text
 ("s--"        . esk-text-scale-decrease)
 ("s-+"        . esk-text-scale-increase))

(provide 'keybindings)
