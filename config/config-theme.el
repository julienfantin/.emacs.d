;;; config-theme.el --- Theming                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin(require 'use-config) <julienfantin@gmail.com>
;; Keywords: faces

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
(require 'use-config)
(require 'config-path)


;; * Customs

(defcustom config-theme-default 'duotone
  "Default theme."
  :type 'symbol
  :group 'config-theme)

(defun config-theme-load-default-theme ()
  "Load the default theme."
  (when config-theme-default
    (load-theme config-theme-default t)))


;; * Built-inss

(use-package custom
  :defer t
  :commands load-theme
  :init
  (progn
    (setq custom-theme-directory
          (expand-file-name "themes/" user-emacs-directory))
    (config-path-add-to-load-path custom-theme-directory)
    (after-init #'config-theme-load-default-theme)))


;; * Themes

(use-package theme-sync
  :disabled t
  :defer t
  :commands theme-sync-mode
  :init (after-init #'theme-sync-mode))

(use-package duotone-theme)
(use-package duotone-reload)

(use-package doom-themes           :ensure t :defer t)
(use-package dracula-theme         :ensure t :defer t)
(use-package darktooth-theme       :ensure t :defer t)
(use-package zerodark-theme        :ensure t :defer t)
(use-package gruvbox-theme         :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package rebecca-theme         :ensure t :defer t)

(use-package customize-theme
  :disabled t
  :load-path "./lib"
  :config
  (validate-setq
   customize-theme-alist
   `((doom-one
      . ((show-paren-match
          . ((t (:foreground nil :inverse-video nil
                             :background
                             ,(chroma-to-string
                               (chroma-blend
                                (chroma-hex :hex "#51afef")
                                (chroma-hex :hex "#282c34")
                                0.8)))))))))))


;; * Theme definition helpers

;; Copied from
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-kurecolor.el

(use-package kurecolor
  :ensure t
  :defer t
  :commands (hydra-kurecolor/body)
  :init
  (defhydra hydra-kurecolor (:color pink :hint  nil :body-pre (require 'kurecolor))
    "
Inc/Dec      _j_/_J_ brightness      _k_/_K_ saturation      _l_/_L_ hue
Set          _sj_ ^^ brightness      _sk_ ^^ saturation      _sl_ ^^ hue
Get          _gj_ ^^ brightness      _gk_ ^^ saturation      _gl_ ^^ hue
             _rh_ ^^ RGB → Hex       _hr_ ^^ Hex → RGB       _hR_ ^^ Hex → RGBA
"
    ("j"  kurecolor-decrease-brightness-by-step)
    ("J"  kurecolor-increase-brightness-by-step)
    ("k"  kurecolor-decrease-saturation-by-step)
    ("K"  kurecolor-increase-saturation-by-step)
    ("l"  kurecolor-decrease-hue-by-step)
    ("L"  kurecolor-increase-hue-by-step)
    ("sj" kurecolor-set-brightness :color blue)
    ("sk" kurecolor-set-saturation :color blue)
    ("sl" kurecolor-set-hue :color blue)
    ("gj" kurecolor-hex-val-group :color blue)
    ("gk" kurecolor-hex-sat-group :color blue)
    ("gl" kurecolor-hex-hue-group :color blue)
    ("rh" kurecolor-cssrgb-at-point-or-region-to-hex :color blue)
    ("hr" kurecolor-hexcolor-at-point-or-region-to-css-rgb :color blue)
    ("hR" kurecolor-hexcolor-at-point-or-region-to-css-rgba :color blue)
    ("q"  nil "cancel" :color blue)))

(provide 'config-theme)
;;; config-theme.el ends here
