(require 'use-package)
(require 'hydra)
(require 'commands)


;; * Hydras

;; ** Toggles
;; ** Git
;; ** Screens
;; ** Clocking


;; ** Outlines

;; (defhydra hydra-outline (:color red :columns 4)
;;   ;; Navigation
;;   ("n" outline-next-visible-heading     "next-visible-heading")
;;   ("p" outline-previous-visible-heading "previous-visible-heading")
;;   ("f" outline-forward-same-level       "forward-same-level")
;;   ("u" outline-up-heading               "up-heading")
;;   ("b" outline-backward-same-level      "backward-same-level")
;;   ("F" outshine-next-block              "next-block")
;;   ("B" outshine-previous-block          "previous-block")
;;   ("j" outshine-navi                    "navi" :exit t)
;;   ("i" outshine-imenu                   "imenu" :exit t)
;;   ;; Visibility
;;   ("a" outline-show-all                 "show-all")
;;   ("c" outline-cycle                    "cycle")
;;   ("C" outshine-cycle-buffer            "cycle-buffer")
;;   ("'" (lambda () (interactive) (outshine-use-outorg (quote org-display-outline-path) (quote WHOLE-BUFFER-P))) "outorg")
;;   ("r" outshine-narrow-to-subtree       "narrow-to-subtree")
;;   ("w" widen                            "widen")
;;   ;; Structure Editing
;;   ("U" outline-move-subtree-up          "move-subtree-up")
;;   ("D" outline-move-subtree-down        "move-subtree-down")
;;   ("+" outline-demote                   "demote")
;;   ("-" outline-promote                  "promote")
;;   ("M-RET" outshine-insert-heading      "insert-heading")
;;   ("C-RET" outshine-insert-heading      "insert-heading")
;;   ("^" outshine-sort-entries            "sort-entries")
;;   ("m" outline-mark-subtree             "mark-subtree")
;;   ("#" outshine-toggle-comment          "toggle-comment")
;;   ;; Clock Commands
;;   ("I" outshine-clock-in                "clock-in")
;;   ("O" outshine-clock-out               "clock-out")
;;   ;; Date & Time Commands
;;   ("." outshine-time-stamp              "time-stamp")
;;   ("!" outshine-time-stamp-inactive     "time-stamp-inactive")
;;   ("d" outshine-deadline                "deadline")
;;   ("s" outshine-schedule                "schedule")
;;   ;; Meta Data Editing
;;   ("t" outshine-todo                    "todo")
;;   ("," outshine-priority                "priority")
;;   ("0" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 32)))) "priority 32")
;;   ("1" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 65)))) "priority 65")
;;   ("2" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 66)))) "priority 66")
;;   ("3" (lambda () (interactive) (outshine-use-outorg (lambda () (interactive) (org-priority 67)))) "priority 67")
;;   (":" outshine-set-tags-command        "set-tags-command")
;;   ;; Misc
;;   ("o" outshine-open-at-point))

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



;; * Help




;; * Prog mode



;; * Lisps mode




;; * Global bindings

;; ** C-c map



;; ** Helm map



;; ** Globals and overrides




;; ** Super

;; (bind-keys
;;  ;; Windows
;;  ("s-w"        . windresize)
;;  ("s-p"        . windmove-up)
;;  ("s-n"        . windmove-down)
;;  ("s-f"        . windmove-right)
;;  ("s-b"        . windmove-left)
;;  ("s-P"        . windresize-up)
;;  ("s-N"        . windresize-down)
;;  ("s-F"        . windresize-right)
;;  ("s-B"        . windresize-left)
;;  ("s-C-p"      . buf-move-up)
;;  ("s-C-n"      . buf-move-down)
;;  ("s-C-f"      . buf-move-right)
;;  ("s-C-b"      . buf-move-left)
;;  ("s-|"        . esk-window-split-toggle)
;;  ("s-M-u"      . esk-switch-to-last-window)
;;  ;; Buffers
;;  ("s-M-p"      . previous-buffer)
;;  ("s-M-n"      . next-buffer)
;;  ("s-M-d"      . esk-alternate-buffer)
;;  ("s-t"        . temp-buffer)
;;  ("s-h"        . bury-buffer)
;;  ("s-u"        . revert-buffer)
;;  ("s-k"        . kill-this-buffer)
;;  ;; Commands
;;  ("s-s"        . eshell)
;;  ("s-<return>" . helm-projectile)
;;  ;; Workspace
;;  ("s-M-1 "     . simple-screen-0)
;;  ("s-M-2 "     . simple-screen-1)
;;  ("s-M-3 "     . simple-screen-2)
;;  ("s-M-4 "     . simple-screen-3)
;;  ("s-M-5 "     . simple-screen-4)
;;  ("s-M-6 "     . simple-screen-5)
;;  ;; Navigation
;;  ("s-`"        . push-mark-no-activate)
;;  ("s-SPC"      . jump-to-mark)
;;  ("s-<"        . goto-last-change)
;;  ("s-["        . diff-hl-previous-hunk)
;;  ("s-]"        . diff-hl-next-hunk)
;;  ("s-i"        . helm-semantic-or-imenu)
;;  ("s-M-i"      . helm-imenu-anywhere)
;;  ("s-h"        . avy-goto-word-or-subword-1)
;;  ("s-j"        . avy-goto-char-timer)
;;  ("s-/"        . link-hint-open-link)
;;  ("s-o"        . helm-org-in-buffer-headings)
;;  ("s-e"        . esk-flycheck-list-errors)
;;  ("s-g"        . lispy-goto)
;;  ("s-G"        . lispy-goto-local)
;;  ("s-m"        . helm-all-mark-rings)
;;  ("s-;"        . swiper-helm)
;;  ;; Text
;;  ("s--"        . esk-text-scale-decrease)
;;  ("s-+"        . esk-text-scale-increase))


;; * Hyper





;; * Keychords

;; Quickly jump to a lispy position
(key-chord-define-global "jk" #'paredit-backward-up)


(comment
 (bind-key "H-w" 'ace-window)
 (use-package ace-window
   :ensure t
   :defer 1
   :config
   (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
   (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
   (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
         aw-dispatch-always t
         aw-dispatch-alist
         '((?x aw-delete-window     "Ace - Delete Window")
           (?s aw-swap-window       "Ace - Swap Window")
           (?n aw-flip-window)
           (?v aw-split-window-vert "Ace - Split Vert Window")
           (?h aw-split-window-horz "Ace - Split Horz Window")
           (?m delete-other-windows "Ace - Maximize Window")
           (?g delete-other-windows)
           (?= balance-windows)
           (?u winner-undo)
           (?r winner-redo)))

   (when (package-installed-p 'hydra)
     (defhydra hydra-window-size (:color red)
       "Windows size"
       ("h" shrink-window-horizontally "shrink horizontal")
       ("j" shrink-window "shrink vertical")
       ("k" enlarge-window "enlarge vertical")
       ("l" enlarge-window-horizontally "enlarge horizontal"))
     (defhydra hydra-window-frame (:color red)
       "Frame"
       ("f" make-frame "new frame")
       ("x" delete-frame "delete frame"))
     (defhydra hydra-window-scroll (:color red)
       "Scroll other window"
       ("n" joe-scroll-other-window "scroll")
       ("p" joe-scroll-other-window-down "scroll down"))
     (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
     (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
     (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
   (ace-window-display-mode t)))

;; * Towards a binding language
;; ** Verbs
;; open/find
;; search
;; replace
;; delete
;; kill
;; yank
;; move/drag
;; transpose
;; describe
;; list
;; ** Subjects
;; char
;; subword
;; thing?
;; word
;; sexp
;; defun
;; region
;; paragraph
;; phrase
;; buffer
;; all opened buffers
;; file
;; all files in /
;; window
;; frame
;; context
;; ** Modifiers
;; all
;; up/down
;; previous/next
;; outer/inner
;; beginning/end
;;
;; * Transient contexts
;; outline
;; window
;; symbol
;; search
;; replacement


;; * Major mode

(defvar hyperspace-eval-defun)

(defvar hypersapce-eval-last-sexp)
(defvar hyperspace-eval-last-sexp-and-replace)
(defvar hyperspace-eval-last-sexp-and-insert-comment)

(defvar hyperspace-pp-eval-last-sexp)
(defvar hyperspace-pp-eval-last-sexp-and-replace)
(defvar hyperspace-pp-eval-last-sexp-and-insert-comment)

(define-prefix-command 'hyperspace-pp-eval-map)
(define-prefix-command 'hyperspace-eval-map)

(bind-keys
 :map hyperspace-eval-map
 ("c" . hyperspace-eval-last-sexp-and-insert-comment)
 ("e" . hyperspace-eval-last-sexp)
 ("p" . hyperspace-pp-eval-map)
 ("r" . hyperspace-eval-last-sexp-and-replace)
 ("x" . hyperspace-eval-defun))

(bind-keys
 :map hyperspace-pp-eval-map
 ("c" . hyperspace-pp-eval-last-sexp-and-insert-comment)
 ("e" . hyperspace-pp-eval-last-sexp)
 ("r" . hyperspace-pp-eval-last-sexp-and-replace))

(define-prefix-command 'hyperspace-major-mode-map)

(bind-keys
 :map hyperspace-major-mode-map
 ("e" . hyperspace-eval-map))

(setq hyperspace-eval-bindings-alist
      '((emacs-lisp-mode
         . ((hyperspace-eval-defun . eval-defun)
            (hyperspace-eval-last-sexp . eval-last-sexp)
            (hyperspace-pp-eval-last-sexp . pp-eval-last-sexp)))
        (clojure-mode
         . ((hyperspace-eval-defun . cider-eval-defun-at-point)
            (hyperspace-eval-last-sexp . cider-eval-last-sexp)
            (hyperspace-pp-eval-last-sexp . pp-eval-last-sexp)))))

(define-prefix-command 'hyperspace-buffers-mode-map)

;; (require 'smartrep)
;; (smartrep-define-key
;;     hyperspace-buffers-mode-map nil
;;   '(("w" . esk-switch-to-last-window)
;;     ("o" . esk-alternate-buffer)
;;     ("n" . next-buffer)
;;     ("p" . previous-buffer)
;;     ("h" . bury-buffer)
;;     ("k" . kill-this-buffer)
;;     ("u" . revert-buffer)))

;; (bind-keys
;;  :map hyperspace-buffers-mode-map
;;  ("t" . temp-buffer)
;;  ("w" . esk-switch-to-last-window)
;;  ("o" . esk-alternate-buffer)
;;  ("b" . helm-mini)
;;  ("n" . next-buffer)
;;  ("p" . previous-buffer)
;;  ("h" . bury-buffer)
;;  ("k" . kill-this-buffer)
;;  ("u" . revert-buffer))

;; (defun hyperspace-bbind-major-mode ()
;;   (when-let (bindings (alist-get major-mode hyperspace-major-mode-bindings-alist))
;;     (bind-key "m" hyperspace-major-mode-map hyperspace-mode-map)))

;; (add-hook 'change-major-mode-hook #'hyperspace-bind-major-mode)


(bind-key "H-b" hyperspace-buffers-mode-map)

(define-prefix-command 'hyperspace-mode-map)

(bind-keys
 :map hyperspace-mode-map
 ("m" . hyperspace-major-mode-map))

(defvar hyperspace-mode nil)

(keymap-canonicalize hyperspace-mode-map)



(define-minor-mode hyperspace-mode
  "Consistent user bindings for the hyper key"
  hyperspace-mode
  "HSPC"
  hyperspace-mode-map
  (if hyperspace-mode
      (bind-key "H-m" hyperspace-mode-map)
    (unbind-key "H")))



;; (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
;;       aw-dispatch-always t
;;       aw-dispatch-alist
;;       '((?x aw-delete-window     "Ace - Delete Window")
;;         (?t aw-swap-window       "Ace - Swap Window")
;;         (?o aw-flip-window)
;;         ;; (?v aw-split-window-vert "Ace - Split Vert Window")
;;         ;; (?h aw-split-window-horz "Ace - Split Horz Window")
;;         ;; (?m delete-other-windows "Ace - Maximize Window")
;;         (?g delete-other-windows)
;;         (?= balance-windows)
;;         (?u winner-undo)
;;         (?r winner-redo)))


;; * TODO Bind

;; flyspell-check-previous-highlighted-word
;; flyspell-auto-correct-word


(defhydra hydra-outshine (:color red)
  ("n" (outshine-speed-move-safe 'outline-next-visible-heading))
  ("p" (outshine-speed-move-safe 'outline-previous-visible-heading))
  ("f" (outshine-speed-move-safe 'outline-forward-same-level))
  ("u" (outshine-speed-move-safe 'outline-up-heading))
  ("b" (outshine-speed-move-safe 'outline-backward-same-level))
  ("F" outshine-next-block)
  ("B" outshine-previous-block)
  ("j" outshine-navi)
  ("J" outshine-imenu)
  ("g" outshine-imenu)
  ("c" outline-cycle)
  ("C" outshine-cycle-buffer)
  (" " (outshine-use-outorg 'org-display-outline-path 'WHOLE-BUFFER-P))
  ("r" outshine-narrow-to-subtree)
  ("w" widen)
  ("U" outline-move-subtree-up)
  ("D" outline-move-subtree-down)
  ("+" outline-demote)
  ("-" outline-promote)
  ("i" outshine-insert-heading)
  ("^" outshine-sort-entries)
  ("m" outline-mark-subtree)
  ("#" outshine-toggle-comment)
  ("." outshine-time-stamp)
  ("!" outshine-time-stamp-inactive)
  ("d" outshine-deadline)
  ("s" outshine-schedule)
  ("t" outshine-todo)
  ("," outshine-priority)
  ("0" (outshine-use-outorg (lambda () (interactive) (org-priority ?\ ))))
  ("1" (outshine-use-outorg (lambda () (interactive) (org-priority ?A))))
  ("2" (outshine-use-outorg (lambda () (interactive) (org-priority ?B))))
  ("3" (outshine-use-outorg (lambda () (interactive) (org-priority ?C))))
  (":" outshine-set-tags-command)
  ("y" outshine-set-property)
  ("Y" outshine-set-property-and-value)
  ("e" outshine-set-effort)
  ("E" outshine-inc-effort)
  ("v" outshine-agenda)
  ("<" (outshine-agenda-set-restriction-lock))
  (">" (outshine-agenda-remove-restriction-lock))
  ("o" outshine-open-at-point)
  ("C-_" undo))

(provide 'keybindings)
