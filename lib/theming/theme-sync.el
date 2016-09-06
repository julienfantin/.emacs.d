;;; theme-sync.el --- Automatically adjust uncustomized faces colors to the current theme(s)   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Keywords: faces, convenience

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

;; Theme sync obtains a list of colors used in the currenly enabled themes and
;; provides mechanisms for enforcing those colors on uncustomized faces
;; (i.e. those not defined in the currently enabled themes)

;; If a face defines a :background attribute, the algorithm will replace it with
;; the closest color in the current theme(s) according to the 'color-cie-de2000'
;; distance function.

;; If a face defines a :foreground attribute it will apply the same logic, while
;; ensuring that the chosen foreground color is visible over the previously
;; selected background color.  If the face is not visible it'll pick the n'th
;; closest, or eventually default to an arbitrary :foreground color that is
;; visible when overlaid over the background .In cases where a face does not
;; have a :background attribute, its visibility is checked against the default
;; face's background.

;; 'theme-sync' can also preform its synchronization dynamically as new packages
;; are loaded thanks to 'loadhist.el' as well as hook into 'load-theme' to
;; ensure a consistent look.

;; TODO list:

;; * Handle overlaid faces
;;   - Add an alist of (face-fg . face-bg) for faces that overlay
;;   - Do a topological sort that will sync bg faces first
;;   - Use the background face's background in the visibility check

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'color)
(require 'shr-color)
(require 'loadhist)


;; * Customs

(defvar theme-sync-load-history t
  "Automatically sync faces when files are loaded.")

(defvar theme-sync-load-theme t
  "Automatically sync faces when loading are loaded.")

(defvar theme-sync-on-enable t
  "Sync faces when the mode is enabled.")

(defvar theme-sync-fallback-to-default-bg nil)

(defvar theme-sync-color-visible-distance-min 10
  "Like `shr-color-visible-distance-min' default was 5.")

(defvar theme-sync-color-visible-luminance-min 50
  "Like `shr-color-visible-luminance-min' default was 40.")


;; * Internal state

(defvar theme-sync--original-faces nil)

(defun theme-sync--remember-original-face (face attr)
  "Add `FACE' to the `theme-sync--original-faces' alist and put
`ATTR` and `VALUE' in a plist at that key."
  (let ((value (face-attribute face attr))
        (plist (map-elt theme-sync--original-faces face)))
    (map-put theme-sync--original-faces face (plist-put plist attr value))))

(defun theme-sync--restore-all-faces ()
  "Restore faces in `theme-sync--original-faces' and set it to
nil."
  (setq theme-sync--original-faces
        (dolist (elt theme-sync--original-faces)
          (let ((face  (car elt))
                (props (cdr elt)))
            (apply #'set-face-attribute face nil props)))))


;; * Faces helpers

(defun theme-sync--theme-face-p (spec)
  "Return t if `SPEC' is a theme-face spec."
  (eq 'theme-face (car spec)))

(defun theme-sync--get-face-attribute (face attr)
  "Return `FACE' `ATTR' unless it is 'unspecified."
  (let ((value (face-attribute face attr)))
    (unless (equal 'unspecified value) value)))


;; * Colors

(defun theme-sync--color-to-cielab (color)
  "Return the cielab representation of `COLOR'."
  (cl-destructuring-bind (r g b) (color-name-to-rgb color)
    (color-srgb-to-lab r g b)))

(defun theme-sync--cielab-to-color (cielab)
  "Return an hex representation of `CIELAB'."
  (cl-destructuring-bind (l a b) cielab
    (cl-destructuring-bind (r g b) (color-lab-to-srgb l a b)
      (color-rgb-to-hex r g b))))

(defun theme-sync--get-face-cielab-color (face attr)
  "Return `FACE' `ATTR' in cielab unless it is 'unspecified."
  (when-let (color (theme-sync--get-face-attribute face attr))
    (theme-sync--color-to-cielab color)))

(defun theme-sync--current-theme-colors ()
  "Return all colors defined in `custom-enabled-themes'.

Colors are lists in the Cielab color space."
  (thread-last custom-enabled-themes
    (cl-mapcan
     (lambda (theme)
       (let* ((settings (get theme 'theme-settings))
              (faces (cl-remove-if-not #'theme-sync--theme-face-p settings)))
         (cl-reduce
          (lambda (acc spec)
            (let ((plist (cl-cadar (cl-cadddr spec))))
              (push (plist-get plist :foreground) acc)
              (push (plist-get plist :background) acc)))
          faces
          :initial-value '()))))
    (seq-uniq)
    (cl-remove-if (apply-partially 'eq 'unspecified))
    (cl-remove-if #'null)
    (cl-mapcar #'theme-sync--color-to-cielab)))

(defun theme-sync--make-visible (fg-cielab bg-cielab)
  "Adaptation of `shr-color-visible' with fixed background and no
color space conversions."
  (unless (or (null fg-cielab) (null bg-cielab))
    (let* ( ;; Compute color distance using CIE DE 2000
           (fg-bg-distance (color-cie-de2000 fg-cielab bg-cielab))
           ;; Compute luminance distance (subtract L component)
           (luminance-distance (abs (- (car fg-cielab) (car bg-cielab)))))
      (if (and (>= fg-bg-distance theme-sync-color-visible-distance-min)
               (>= luminance-distance theme-sync-color-visible-luminance-min))
          fg-cielab
        ;; Not visible, try to change luminance to make them visible
        (let ((Ls (shr-color-set-minimum-interval
                   (car bg-cielab) (car fg-cielab) 0 100
                   theme-sync-color-visible-luminance-min t)))
          (cons (cadr Ls) (cdr fg-cielab)))))))

(defun theme-sync--visible-p (fg bg)
  "Return t if `FG' is visible over `BG'."
  (let ((visible (theme-sync--make-visible fg bg)))
    (and (equal fg (car visible)) (equal bg (cadr visible)))))

(defun theme-sync--colors-ordered-p (reference a b)
  "Return t if `REFERENCE' is closer to `A' than it is to `B'."
  (<= (color-cie-de2000 a reference) (color-cie-de2000 b reference)))

(defun theme-sync--closest-color (palette cielab)
  "Return the color in `PALETTE' that minimizes the distance to `CIELAB'.

The color distance function used is `color-cie-de2000', all
colors should be in the cielab color space as lists of (l a b)."
  (car (sort palette (apply-partially #'theme-sync--colors-ordered-p cielab))))

(defun theme-sync--set-face-attribute (face attr value)
  (progn
    (theme-sync--remember-original-face face attr)
    (set-face-attribute face nil attr value)))

(defun theme-sync--quantize-bg (face palette)
  "Quantize the :background attribute of `FACE' to the closest color in `PALETTE'."
  (when-let ((original (theme-sync--get-face-cielab-color face :background))
             (closest (theme-sync--closest-color palette original))
             (hex (theme-sync--cielab-to-color closest)))
    (theme-sync--set-face-attribute face :background hex)
    closest))

(defun theme-sync--quantize-fg-1 (fg-cielab bg-cielab palette)
  "Recursive search for the closest color in `PALETTE' for
`FG-CIELAB' that's visible over `BG-CIELAB'."
  (if (null palette)
      ;; Couldn't find a visible fg in palette, just let shr decide
      (theme-sync--make-visible fg-cielab bg-cielab)
    (let* ((fg-cielab* (theme-sync--closest-color palette fg-cielab)))
      (if (theme-sync--visible-p fg-cielab* bg-cielab)
          fg-cielab*
        ;; Closest pick isn't visible, recurse with other colors in palette
        (theme-sync--quantize-fg-1 fg-cielab bg-cielab (remove fg-cielab* palette))))))

(defun theme-sync--quantize-fg (face palette bg-cielab)
  "Quantize `FACE' :foreground to the closest color in `PALETTE'
visible over `BG-CIELAB."
  (when-let ((fg-cielab (theme-sync--get-face-cielab-color face :foreground))
             (fg-cielab* (theme-sync--quantize-fg-1 fg-cielab bg-cielab palette))
             (hex (theme-sync--cielab-to-color fg-cielab*)))
    (theme-sync--set-face-attribute face :foreground hex)))

(defun theme-sync--quantize-face (face palette default-bg-cielab)
  (when palette
    (let ((bg-cielab (or (theme-sync--quantize-bg face palette) default-bg-cielab)))
      (theme-sync--quantize-fg face palette bg-cielab))))

(defun theme-sync--customized-p (face)
  "Return t if `FACE' is customized by the user or a theme."
  (or (get face 'theme-face)
      (get face 'customized-face)
      (get face 'saved-face)))

(defun theme-sync--sync-faces (faces)
  "Sync `FACES' workhorse."
  (when-let
      ((uncustomized-faces (cl-remove-if #'theme-sync--customized-p faces))
       (palette (theme-sync--current-theme-colors))
       (default-bg-cielab (theme-sync--get-face-cielab-color 'default :background)))
    (dolist (face uncustomized-faces)
      (theme-sync--quantize-face face palette default-bg-cielab))))


;; * Loadhist integration

(defun theme-sync--face-symbol (elt)
  "Retur `ELT' if it's a face symbol."
  (when (and elt (symbolp elt) (facep elt)) elt))

(defun theme-sync--defface-symbol (elt)
  "Return the cdr of `ELT' if it's a `deffface' form."
  (when (and elt (listp elt) (equal (car elt) 'defface)) (cdr elt)))

(defun theme-sync--file-faces (file)
  "Return a list of faces defined in `FILE'.

This relies on `loadhist.el' so `FILE' needs to be loaded."
  (thread-last (file-loadhist-lookup file)
    (mapcar
     (lambda (elt)
       (or (theme-sync--face-symbol elt)
           (theme-sync--defface-symbol elt))))
    (cl-remove-if #'null)))

(defun theme-sync--sync-file-faces (file)
  "Sync faces defined in `FILE' when `theme-sync-load-history' is t."
  (when theme-sync-load-history
    (theme-sync--sync-faces (theme-sync--file-faces file))))


;; * Load theme advice

(defun theme-sync--load-theme-advice (_theme &optional _ _)
  "Sync faces after loading `THEME'."
  (theme-sync--sync-faces (face-list)))

(defun theme-sync--unload-theme-advice (_theme &optional _ _)
  "Restore faces before loading `THEME'."
  (theme-sync--restore-all-faces))


;; * Minor mode

(defun theme-sync-mode--enable ()
  "Internal, use `theme-sync-mode' instead."
  (when theme-sync-on-enable
    (theme-sync--sync-faces (face-list)))
  (when theme-sync-load-history
    (add-hook 'after-load-functions #'theme-sync--sync-file-faces))
  (when theme-sync-load-theme
    (advice-add #'load-theme :before #'theme-sync--unload-theme-advice)
    (advice-add #'load-theme :after #'theme-sync--load-theme-advice)))

(defun theme-sync-mode--disable ()
  "Internal, use `theme-sync-mode' instead."
  (theme-sync--restore-all-faces)
  (remove-hook 'after-load-functions #'theme-sync--sync-file-faces)
  (advice-remove #'load-theme #'theme-sync--unload-theme-advice)
  (advice-remove #'load-theme #'theme-sync--load-theme-advice))

;;;###autoload
(define-minor-mode theme-sync-mode
  "Synchronize uncustomized face colors with the currently loaded themes."
  :lighter ""
  :keymap nil
  :global t
  (if theme-sync-mode
      (theme-sync-mode--enable)
    (theme-sync-mode--disable)))

(provide 'theme-sync)
;;; theme-sync.el ends here
