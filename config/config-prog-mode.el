;;; config-prog-mode.el --- Generic programming mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: languages

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

;;; Built-ins

(use-package prog-mode
  :hook ((prog-mode . auto-fill-mode)
         (prog-mode . display-line-numbers-mode)))

(use-package conf-mode
  :preface (defun config-prog-run-hooks () (run-hooks 'prog-mode-hook))
  :after prog-mode
  :config (add-hook 'conf-mode-hook #'config-prog-run-hooks))

(use-package autoinsert
  :hook (after-init . auto-insert-mode))

;;; Third-party

(use-package smart-jump
  :straight t
  :config
  ;; TODO register elisp-def...
  (smart-jump-setup-default-registers))

(use-package smart-jump
  :after (smart-jump lispy)
  :bind (:map lispy-mode-map
              ([remap lispy-goto] . smart-jump-go)))

(use-package iedit
  :straight t
  :bind
  (("M-i" . -iedit-ensure-function)
   ("M-n" . -iedit-next-occurrence)
   ("M-p" . -iedit-prev-occurrence)
   :map iedit-mode-occurrence-keymap
   ("M-n" . iedit-next-occurrence)
   ("M-p" . iedit-prev-occurrence)
   ("C-g" . -iedit-quit)
   :map isearch-mode-map
   ("C-c e i" . iedit-mode-from-isearch))
  :custom
  (iedit-toggle-key-default nil)
  ;; Messes with undo if turned on...
  (iedit-auto-buffering nil)
  :config
  (defvar --iedit-occurrence-timer nil)

  (defun --iedit-occurrence-stop-timer ()
    (when --iedit-occurrence-timer
      (setq-local
       --iedit-occurrence-timer
       (cancel-timer --iedit-occurrence-timer))))

  (defun -iedit-quit ()
    (interactive)
    (--iedit-occurrence-stop-timer)
    (iedit-quit))

  (defun --iedit-at-occurrence-p ()
    (or (get-char-property (point) 'iedit-occurrence-overlay-name)
        ;; Check 1 char to the left when moving at end of occurrence
        (get-char-property (- (point) 1) 'iedit-occurrence-overlay-name)))

  (defun --iedit-occurrence-exit ()
    (condition-case nil
        (unless (--iedit-at-occurrence-p)
          (-iedit-quit))
      (error (--iedit-occurrence-stop-timer))))

  (defun --iedit-occurrence-start-timer ()
    (unless --iedit-occurrence-timer
      (setq-local
       --iedit-occurrence-timer
       (run-with-timer 0 0.25 #'--iedit-occurrence-exit))))

  (defun -iedit-ensure (&optional arg)
    (interactive "P")
    (unless iedit-mode
      (when (and lispy-mode (lispy-left-p))
        (forward-char 1))
      (if arg
          (iedit-mode 0)
        (iedit-mode))
      (--iedit-occurrence-start-timer)))

  (defun -iedit-next-occurrence (&optional arg)
    (interactive "P")
    (progn
      (-iedit-ensure arg)
      (iedit-next-occurrence)))

  (defun -iedit-prev-occurrence (&optional arg)
    (interactive "P")
    (progn
      (-iedit-ensure arg)
      (iedit-prev-occurrence)))

  (defun -iedit-ensure-function (&optional arg)
    (interactive "P")
    (if iedit-mode
        (-iedit-quit)
      (-iedit-ensure (not arg)))))

(use-package iedit
  :after (iedit lispy)
  :bind
  ((:map lispy-mode-map
         ([remap lispy-iedit] . -iedit-ensure-function))
   (:map lispy-mode-map-lispy
         ([remap lispy-iedit] . -iedit-ensure-function))))

(use-package emr
  :straight t
  :after prog-mode
  :bind ((:map prog-mode-map
               ("C-M-<return>" . emr-show-refactor-menu)))
  :init (emr-initialize))

(use-package poporg
  :straight t
  :bind ("C-c e o" . poporg-dwim))

(use-package outshine
  :straight t
  :hook ((lispy-mode . outshine-lispy))
  :custom
  (outshine-use-speed-commands t)
  (outshine-fontify-whole-heading-line t)
  (outshine-cycle-emulate-tab t)
  :config
  ;; Keybindings get clobbered by lispy and company-mode, so make sure we enable
  ;; this mode last...
  (add-hook 'prog-mode-hook 'outshine-mode 100)
  (defun outshine-lispy ()
    "Lispy hard-codes a non-standard outline-regexp in its
navigation and editing code. We fix the read path by using
outshine to recompute a correct regexp, and we'll disable the
lispy editing commands to replace them with the outshine editing
commands."
    ;; Lispy already implements this
    (setq-local outshine-use-speed-commands nil)
    (set (make-local-variable 'lispy-outline) (outshine-calc-outline-regexp)))
  :config
  ;; Adapted from https://github.com/tarsius/backline/blob/master/backline.el
  (defun outshine-backline-update (from to _hide)
    "When hidings, add an overlay to extend header's appearance to window edge."
    (when outline-minor-mode
      ;; `outline-hide-sublevels' tries to hide this range, in which case
      ;; `outline-back-to-heading' somehow concludes that point is before
      ;; the first heading causing it to raise an error.  Luckily we don't
      ;; actually have to do anything for that range, so we can just skip
      ;; ahead to the calls that hide the subtrees individually.
      (unless (and (= from   (point-min))
                   (= to (1- (point-max))))
        (ignore-errors        ; Other instances of "before first heading" error.
          (remove-overlays from
                           (save-excursion
                             (goto-char to)
                             (outline-end-of-subtree)
                             (1+ (point)))
                           'backline-heading t)
          (dolist (ov (overlays-in (max (1- from) (point-min))
                                   (min (1+ to)   (point-max))))
            (when (eq (overlay-get ov 'invisible) 'outline)
              (let ((end (overlay-end ov)))
                (unless (save-excursion
                          (goto-char end)
                          (outline-back-to-heading)
                          ;; If we depended on `bicycle', then we could use:
                          ;; (bicycle--code-level-p)
                          (= (funcall outline-level)
                             (or (bound-and-true-p outline-code-level) 1000)))
                  (let ((o (make-overlay end
                                         (min (1+ end) (point-max))
                                         nil 'front-advance)))
                    (overlay-put o 'evaporate t)
                    (overlay-put o 'backline-heading t)
                    (overlay-put o 'face
                                 (save-excursion
                                   (goto-char end)
                                   (outline-back-to-heading)
                                   (outshine-faces--get-face))))))))))))

  (defun outshine-faces--level ()
    (save-excursion
      (beginning-of-line)
      (and (looking-at outline-regexp)
           (funcall outline-level))))

  (defvar outshine-faces--top-level nil)

  (defun outshine-faces--top-level ()
    (or outshine-faces--top-level
        (save-excursion
          (goto-char (point-min))
          (let ((min (or (outshine-faces--level) 1000)))
            (while (outline-next-heading)
              (setq min (min min (outshine-faces--level))))
            (setq outshine-faces--top-level min)))))

  (defun outshine-faces--get-face ()
    (save-excursion
      (goto-char (match-beginning 0))
      (nth (% (- (outshine-faces--level)
                 (outshine-faces--top-level))
              (length outshine-level-faces))
           outshine-level-faces))))

(use-package lispy
  ;; Unbind lispy commands that insert non-standard headings, outshine already
  ;; binds those keys to the right commands...
  :bind ((:map lispy-mode-map
               ("M-<return>" . nil))
         (:map lispy-mode-map-lispy
               ("M-<return>" . nil))))

(provide 'config-prog-mode)
;;; config-prog-mode.el ends here
