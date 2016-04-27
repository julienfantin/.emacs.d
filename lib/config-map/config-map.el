;;; config-map.el --- User bindings utility          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Julien Fantin

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

;; TODO Add warnings on override
;; TODO Add :major-modes option to config-map
;; TODO Add :minor-modes option to config-map
;; TODO Reuse hydra doc for promoted keys

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'subr-x)
(require 'seq)
(require 'bind-key)
;; * Customs

(defvar config-map-log-fn 'warn)

;; * Keybinding helpers

(defun config-map--ensure-key-string (key)
  "Convert 'KEY' to a string if it's a char."
  (cond
   ((stringp key) key)
   ((integerp key) (char-to-string key))))

(defun config-map--read-key (key)
  ;; wut.gif
  ;; When reading "M-RET" like bindings (kbd) returns an array, and I don't know
  ;; how these can be combined with ht prefix...
  (let ((macro (cl-typecase key
                 (string (kbd key))
                 (integer (kdb (char-to-string key)))
                 (vector key))))
    (if (stringp macro) macro (config-map--ensure-key-string key))))

(defun config-map--join-keys (prefix key)
  "Join 'PREFIX' and 'KEY' into a string suitable for 'kbd'."
  (cond
   ((and prefix key)
    (kbd (concat (config-map--read-key prefix) (config-map--read-key key))))
   ((null prefix) (config-map--read-key key))
   ((null key) (config-map--read-key prefix))))

(defun config-map--bind-key (keymap key def)
  (let ((kbd-key (config-map--read-key key)))
    (when-let ((bound-fn (lookup-key keymap kbd-key)))
      (let ((msg (format "Key-binding %s for %s is already taken by %s" kbd-key def bound-fn)))
        (funcall config-map-log-fn msg)))
    (define-key keymapkey def))
  ;; (bind-key key def keymap)
  )


;; * Mode helpers

(defvar config-map-major-mode-keymap-alist '())

(defun config-map-register-major-keymap (mode prefix keymap)
  (let ((current (alist-get mode config-map-major-mode-keymap-alist))
        (keymap-binding (cons major-mode (list prefix keymap))))
    (add-to-list
     'config-map-major-mode-keymap-alist
     (cons keymap-binding current))))

(defun config-map-mode-change-hook ()
  (when-let (mode-binding (alist-get major-mode config-map-major-mode-keymap-alist))
    (destructuring-bind (prefix keymap) mode-binding
      (local-set-key (config-map--read-key prefix) keymap))))

;; Will let config-map bindings be overriden by major-modes, ok???
(add-hook
 'change-major-mode-after-body-hook
 #'config-map-mode-change-hook)

;; * Arguments

(defun config-map--args-options (args)
  "Return a copy of 'ARGS' containing only the options."
  (let ((args* (cl-copy-list args))
        (options))
    (while args*
      (cl-typecase (cl-first args*)
        (keyword
         (progn
           (push (cl-second args*) options)
           (push (cl-first args*) options)
           (pop args*)
           (pop args*)))
        (t
         (pop args*))))
    options))

(defalias 'config-map--binding-options 'config-map--args-options)

(defun config-map--args-bindings (args)
  "Return a copy of 'ARGS' containing only the binding forms."
  ;; Is the copy necessary?
  (let ((args* (cl-copy-list args))
        (bindings))
    (while args*
      (cl-typecase (cl-first args*)
        (keyword
         (progn
           (pop args*)
           (pop args*)))
        (list
         (let* ((binding (cl-first args*))
                (cmd (cl-second binding)))
           (when cmd (push binding bindings))
           (pop args*)))
        (t
         (pop args*))))
    bindings))

(defun config-map--args-prefix (args)
  "Return the prefix key found in 'ARGS'."
  (plist-get (config-map--args-options args) :prefix))

(defun config-map--args-root-prefix (args)
  "Return the root prefix key found in 'ARGS'."
  (plist-get (config-map--args-options args) :root-prefix))

(defun config-map--args-full-prefix (args)
  "Return the full prefix key found in 'ARGS'.
Will concatenate :full-prefix to the current :prefix."
  (config-map--join-keys
   (plist-get (config-map--args-options args) :full-prefix)
   (config-map--args-prefix args)))

(defun config-map--args-docstring (name args)
  (let ((docstring (cl-first args)))
    (if (and docstring (stringp docstring))
        docstring
      (symbol-name name))))

;; * Commands handling

(defun config-map--binding-cmd-hydra-p (cmd)
  "Return t if 'BINDING' has a defhydra for commnand."
  (and cmd (listp cmd) (eq 'defhydra (cl-first cmd))))

(defun config-map--binding-cmd-config-map-p (cmd)
  "Return t if 'CMD' is a 'config-map' form."
  (and cmd (listp cmd) (eq 'config-map (cl-first cmd))))

(defun config-map--inject-prefixes (root-prefix full-prefix config-map-form)
  "Inject 'PREFIX' as a :full-prefix option in 'CONFIG-MAP-FORM'."
  (cl-flet
      ((before-options-p (elt) (not (or (keywordp elt) (listp elt)))))
    (let ((pre (seq-take-while #'before-options-p config-map-form))
          (post (seq-drop-while #'before-options-p config-map-form)))
      (append
       pre
       (list :full-prefix full-prefix :root-prefix root-prefix)
       post))))

(defun config-map--binding-inject-prefix (prefix key config-map-form)
  "Inject 'PREFIX' and 'KEY' into 'CONFIG-MAP-FORM'."
  (let* ((full-prefix (config-map--join-keys prefix key)))
    (config-map--inject-prefixes prefix full-prefix config-map-form)))

(defun config-map--binding-command (prefix binding)
  "Return an expanded 'PREFIX' 'BINDING'.

When the 'BINDING' command is a nested 'config-map' form, inject
the given prefix."
  (let ((key (cl-first binding))
        (cmd (cl-second binding)))
    (cond
     ((config-map--binding-cmd-config-map-p cmd)
      (config-map--binding-inject-prefix prefix key cmd))
     ((config-map--binding-cmd-hydra-p cmd) `(function ,(eval cmd)))
     (t `(function ,cmd)))))

;; * Bindings
;; ** Docstring

(defun config-map--binding-cmd-docstring-hydra (form)
  "Return a formatted docstring for an hydra 'FORM'."
  (let ((hydra-symbol (cl-second form)))
    (format "%s ⋔" (symbol-name hydra-symbol))))

(defun config-map--binding-cmd-docstring-function (f)
  "Return a formatted docstring for a 'function' command 'F'."
  ;; dontknowwhatimdoing.jpg
  (when-let
      ((plist (symbol-plist f))
       (elems (plist-get plist 'event-symbol-elements))
       (sym (cl-first elems)))
    (symbol-name sym)))

(defun config-map--binding-docstring (binding)
  "Return the documentation for 'BINDING'."
  (let ((cmd (cl-second binding))
        (doc (cl-third binding)))
    (cond
     ;; Docstring provided in binding definition
     ((and doc (stringp doc)) doc)
     ;; Derive from defhydra form
     ((config-map--binding-cmd-hydra-p cmd)
      (config-map--binding-cmd-docstring-hydra cmd))
     ;; Workaround for byte-compiled functions
     ((functionp cmd)
      (config-map--binding-cmd-docstring-function cmd))
     ;; Fallback
     ((symbolp cmd)
      (symbol-name cmd)))))

;; ** Map default

(defun config-map--binding-default-p (binding)
  "Return t if :default is t in 'BINDING'."
  (plist-get (config-map--binding-options binding) :default))

(defun config-map--default-bindings (root-prefix bindings)
  "Return t if :default is t in 'BINDING'."
  (thread-last bindings
    (mapcar
     (lambda (binding)
       (when (config-map--binding-default-p binding)
         (let ((key (cl-first binding))
               (cmd (cl-second binding))
               (doc (cl-third binding)))
           `(,(config-map--join-keys root-prefix key)
             ,cmd
             ,doc)))))
    (delq nil)))

;; ** Promoted hydra keys

(defun config-map--binding-promoted-keys (binding)
  (let ((keys (plist-get binding :promote)))
    (cond
     ((null keys) nil)
     ((stringp keys) (list keys))
     ((listp keys) keys))))

(defun config-map--hydra-lookup-cmd (key form)
  (let* ((hydra-name (cl-second form))
         (hydra-keymap (intern (format "%s/keymap" hydra-name))))
    (lookup-key (symbol-value hydra-keymap) (config-map--read-key key))))

(defun config-map--hydra-lookup-doc (key form)
  (let* ((hydra-name (cl-second form))
         (hydra-heads (intern (format "%s/heads" hydra-name))))
    (cl-some
     (lambda (head)
       ;; Make sure this is our head...
       (when (equal key (cl-first head))
         (let ((cmd (cl-second head))
               (doc (cl-third head)))
           (or doc (symbol-name cmd)))))
     (symbol-value hydra-heads))))

(defun config-map--binding-promoted-hydra-bindings (binding)
  "Return promoted hydra bindings found in 'BINDING'."
  (let ((form (cl-second binding)))
    ;; HACK handle hydra introspection without eval???
    (eval form)
    (mapcar
     (lambda (key)
       (let ((cmd (config-map--hydra-lookup-cmd key form))
             (doc (config-map--hydra-lookup-doc key form)))
         `(,key ,cmd ,doc)))
     (config-map--binding-promoted-keys binding))))

(defun config-map--promoted-hydra-bindings (bindings)
  (thread-last bindings
    (cl-remove-if-not #'config-map--binding-promoted-keys)
    (cl-mapcan #'config-map--binding-promoted-hydra-bindings)))

;; * Forms emission
;; ** Keymap forms

(defun config-map--emit-keymap-forms (name args)
  (let* ((docstring (config-map--args-docstring name args))
         (deffer `(defvar ,name (make-sparse-keymap) ,docstring))
         (setter `(setq ,name (make-sparse-keymap)))
         (parent (plist-get (config-map--args-options args) :inherit)))
    `(,(if (fboundp name) setter deffer)
      (when ,parent (set-keymap-parent ,name ,parent))
      ,name
      (define-prefix-command (quote ,name) (quote ,name)))))

;; ** Top-level prefix form

(defun config-map--emit-prefix-keybinding (name prefix)
  "Emit a 'define-key' form binding 'PREFIX' to prefix command
'NAME'."
  (when prefix
    `(config-map--bind-key global-map ,(config-map--read-key prefix) (function ,name))))

;; ** Keybinding forms

(defun config-map--emit-keybindings (name prefix bindings)
  "Emit a list of 'define-key' forms.
Will bind with 'PREFIX' in keymap 'NAME' the commands found in 'BINDINGS'."
  (mapcar
   (lambda (binding)
     (let ((key (cl-first binding))
           (cmd (config-map--binding-command prefix binding)))
       `(config-map--bind-key ,name ,(config-map--read-key key) ,cmd)))
   bindings))

;; ** Mode-local forms

;; ** Which key integration

(defun config-map-emit-which-key-prefix-replacements (name prefix)
  `(add-to-list 'which-key-prefix-name-alist '(,prefix . ,(symbol-name name))))

(defun config-map-emit-which-key-key-replacements (full-prefix bindings)
  (mapcar
   (lambda (binding)
     (let* ((key (cl-first binding))
            (full-key (config-map--join-keys full-prefix key))
            (doc (config-map--binding-docstring binding)))
       (when (and full-key doc)
         `(add-to-list
           'which-key-key-based-description-replacement-alist
           (cons ,full-key ,doc)))))
   bindings))

(defun config-map-emit-which-key-replacements (name full-prefix bindings)
  `(with-eval-after-load 'which-key
     ,(config-map-emit-which-key-prefix-replacements name full-prefix)
     ,@(config-map-emit-which-key-key-replacements full-prefix bindings)))

;; * Keymaps

(defun config-map--major-mode-local-p (args)
  (listp (plist-get (config-map--args-options args) :major-modes)))


;; * Entry point

(defmacro config-map (name &rest args)
  (declare (indent 1))
  (let* (;; prefixes
         (prefix (config-map--args-prefix args))
         (root-prefix (config-map--args-root-prefix args))
         (full-prefix (config-map--args-full-prefix args))
         ;; bindings (config-map style)
         (base-bindings (config-map--args-bindings args))
         (promoted-bindings (config-map--promoted-hydra-bindings base-bindings))
         (default-bindings (config-map--default-bindings root-prefix base-bindings))
         (all-bindings (append default-bindings base-bindings promoted-bindings))
         ;; Forms
         (keymap-forms (config-map--emit-keymap-forms name args))
         ;; keybinding forms
         (prefix-keybinding (config-map--emit-prefix-keybinding name prefix))
         (all-keybindings (config-map--emit-keybindings name prefix all-bindings))
         ;; which-key forms
         (which-doc (config-map-emit-which-key-replacements name full-prefix all-bindings)))
    `(progn
       ,@`(,@keymap-forms
           ,prefix-keybinding
           ,@all-keybindings
           ,which-doc
           (function ,name)))))

;; (config-map--binding-docstring '("f" #'find-file))
;; (macroexpand-all
;;  '(config-map user-map
;;     "User map"
;;     :prefix "C-c"
;;     ("x" -switch-to-last-window "last window")))

(macroexpand
 '(config-map user-map
    "User map"
    :prefix "C-c"
    ;; ("x" -switch-to-last-window "last window")

    ("a" (config-map nested-map
           "Nested"
           ("f" -switch-to-last-buffer "last buffer" :default t)
           ("g" -switch-to-last-window "last window")))
    ;; ("?"
    ;;  (defhydra hydra-h ()
    ;;    "Hydra h"
    ;;    ("k" counsel-find-file "find file")
    ;;    ("j" counsel-describe-function "desc fun")))
    ))

;; (config-map user-map
;;   "User map"
;;   :prefix "C-c"
;;   ("x" -switch-to-last-window "last window")
;;   ("a"
;;    (config-map nested-map
;;      "Nested"
;;      ("f" -switch-to-last-buffer "last buffer" :default t)
;;      ("g" -switch-to-last-window "last window")))
;;   ("?"
;;    (defhydra hydra-h ()
;;      "Hydra h"
;;      ("k" counsel-find-file "find file")
;;      ("j" counsel-describe-function "desc fun"))
;;    :promote ("k" "j")))

;; (config-map--emit-promoted-hydra-bindings
;;  nil
;;  '("User map"
;;    :prefix "C-c"
;;    ("x" -switch-to-last-window "last window")
;;    ("a"
;;     (config-map nested-map
;;       "Nested"
;;       ("f" -switch-to-last-buffer "last buffer" :default t)
;;       ("g" -switch-to-last-window "last window")))
;;    ("?"
;;     (defhydra hydra-h ()
;;       "Hydra h"
;;       ("k" counsel-find-file "find file")
;;       ("j" counsel-describe-function "desc fun"))
;;     :promote ("k" "j"))))

;; (macroexpand
;;  '(config-map user-map
;;     "User map"
;;     :prefix "C-c"
;;     ("f" (defhydra hydra-foo (:color red)
;;            "Foo hydra"
;;            ("f" find-file)))))

;; (setq which-key-key-based-description-replacement-alist nil)
(provide 'config-map)
;;; config-map.el ends here
