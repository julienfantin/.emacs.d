;;; config-wip.el --- Temporary init code for quick testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: convenience

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

;;; Defaults

(add-to-list 'exec-path "~/bin")

;;; UI tweaks
;;;; Show paren expression transparency hook

(defun fix-show-parent-match (theme &optional _no-confirm no-enable)
  (unless (or no-enable (equal 'duotone theme))
    (let* ((face 'show-paren-match)
           (face-bg (face-attribute face :background))
           (bg (face-attribute 'default :background))
           (new-color (chroma-blend
                       (chroma-hex :hex face-bg)
                       (chroma-hex :hex bg)
                       0.95)))
      (set-face-background face (chroma-to-string new-color)))
    (set-face-foreground 'show-paren-match nil)
    ;; (set-face-attribute 'show-paren-match nil)
    ))

(comment
 (advice-add #'load-theme :after #'fix-show-parent-match))

;;; Keybindings

(global-set-key (kbd "C-x =") 'balance-windows-area)

(use-package hercules :straight t)

;;; Document annotation workflow

(use-package pdf-tools
  ;; Documents are still super blurry even after switching to the hi-dpi
  ;; imagemagick backend, docview actually looks better, but it's all pretty
  ;; terrible compared to preview.app :(
  :disabled t
  :straight t
  :hook (pdf-view-mode .  cua-mode)
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t)
  :init
  (pdf-tools-install))

(use-package org-noter
  ;; Couldn't get a working session to start from inside a document. Combined
  ;; with the poor rendering and the code complexity that one seems like a
  ;; non-starter...
  :straight t)

(use-package nov
  :straight t)

(use-package org
  :custom
  (org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     ;; was find-file-other-window
     (file . find-file)
     (wl . wl-other-frame))))

;;; Performance

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))

(use-package restart-emacs :straight t)

(use-package gcmh
  :straight t
  :init (gcmh-mode 1))

;;; Prose

(use-package freeze-it :straight t)

(use-package spell-fu
  :disabled t                           ;; too many false positives
  :ensure-system-package aspell
  :straight t
  :hook (after-init . global-spell-fu-mode))

;;; Utils

(use-package literate-calc-mode :straight t)

;;; Clojure auto-indent

(use-package cljstyle-mode
  :disabled t
  :ensure-system-package (cljstyle . "brew cask install cljstyle")
  :straight (cljstyle-mode :type git :host github :repo "jstokes/cljstyle-mode")
  :hook (clojure-mode-hook . cljstyle-mode))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :ensure-system-package (cljstyle . "brew cask install cljstyle")
  :hook ((after-init . apheleia-global-mode))
  :config
  (add-to-list 'apheleia-formatters '(cljstyle . ("cljstyle" "pipe") ))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle)))

(use-package flymake-kondor
  :straight t)

;;; Outline-minor mode in code

(defvar config-wip-code-outlines-backend 'tarsius)

;; Outshine:
;; - Fontification of headlines starts with the text which looks ugly,
;; especially with nested outlines levels (there's a workaround here repurposing
;; some functions from backline but it's not pretty)
;; + There's a handy function to compute the ouline-regexp which is convenient
;; to override lispy's weird defautls
;; + Implements speed commands (though that kinda conflicts with lispy)

;; Tarsius' packages:
;; - Other than bicycle, does not help with outline-minor-mode awkward keybindings
;; + Seems leaner overall

;;;; Tarsius packages

(use-package outline-minor-faces
  :if (eq config-wip-code-outlines-backend 'tarsius)
  :straight t
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package backline
  :if (eq config-wip-code-outlines-backend 'tarsius)
  :straight t
  :after outline
  :init (advice-add 'outline-flag-region :after 'backline-update))

(use-package bicycle
  :if (eq config-wip-code-outlines-backend 'tarsius)
  :straight t
  :after outline
  :preface
  (defun config-prog-bicycle-cycle-or-tab-local (arg)
    (interactive "P")
    (if (outline-on-heading-p)
        (call-interactively #'bicycle-cycle)
      (company-indent-or-complete-common arg)))
  :bind (:map outline-minor-mode-map
              ([tab] . config-prog-bicycle-cycle-or-tab-local)
              ([S-tab] . bicycle-cycle-global)))

(use-package outshine
  :if (eq config-wip-code-outlines-backend 'outshine)
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

(use-package outline
  :hook (prog-mode . outline-minor-mode)
  :preface
  (defun config-prog-override-lispy-outline ()
    "Lispy hard-codes non-idiomatic outline regexp for lisp modes, revert that."
    (setq-local lispy-outline ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|(")
    (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\|###autoload\\)\\|(")
    (setq-local outline-level 'lisp-outline-level))
  :config
  (add-hook 'lispy-mode-hook #'config-prog-override-lispy-outline))
(provide 'config-wip)
;;; config-wip.el ends here
