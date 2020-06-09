;;; package --- prose-minor-mode.el
;;; Commentary:
;;
;;; Code:
(require 'subr-x)
(require 'cl-lib)
(require 'flyspell)


;; * Customs

(defcustom prose-minor-mode-line-spacing 0.25
  "Line spacing."
  :type 'float
  :group 'prose)

(defvar prose-minor-mode-line-spacing-default line-spacing)

(defcustom prose-minor-mode-variable-pitch nil
  "Variable pitch flag."
  :type 'boolean
  :group 'prose)

(defcustom prose-minor-mode-fixed-pitch-faces
  '((org-mode
     . (org-table
        org-code
        org-block
        org-block-begin-line
        org-block-end-line
        org-verbatim))
    (markdown-mode
     . (markdown-pre-face
        markdown-markup-face)))
  "Faces that should inherit from fixed-pitch when 'variable-pitch-mode' is enabled."
  :type 'list
  :group 'prose)

(defcustom prose-minor-mode-spell-check t
  "Spell checking flag."
  :type 'boolean
  :group 'prose)

(defcustom prose-minor-mode-spell-check-prose-predicates
  '((org-mode . prose-minor-mode-org-mode-spell-check-predicate)
    (markdown-mode . prose-minor-mode-markdown-mode-spell-check-predicate))
  "Alist of major mode to spell-check predicate function."
  :type 'alist
  :group 'prose)

(defcustom prose-minor-mode-spell-check-function
  'flyspell-mode
  "Function to toggle spell-checking."
  :type 'function
  :group 'prose)

(defcustom prose-minor-mode-spell-check-predicate
  'flyspell-generic-check-word-predicate
  "Spell-checking Predicate function."
  :type 'function
  :group 'prose)

(defvar prose-minor-mode)


;; * Spell checking predicates

(declare-function markdown-code-block-at-point "markdown-mode.el")
(declare-function org-in-src-block-p "org.el")

(defun prose-minor-mode-spell-checking-predicate ()
  "Function associated with 'major-mode' in 'prose-minor-mode-spell-check-prose-predicates'."
  (alist-get major-mode prose-minor-mode-spell-check-prose-predicates))

(defun prose-minor-mode-markdown-mode-spell-check-predicate ()
  "Spell-check predicate function for 'markdown-mode'."
  (null (markdown-code-block-at-point)))

(defun prose-minor-mode-org-mode-spell-check-predicate ()
  "Spell-check predicate function for 'org-mode'."
  (not (org-in-src-block-p)))


;; * Functions

(defun prose-minor-mode-toggle-line-spacing ()
  "Toggle 'line-spacing' when enabled."
  (if prose-minor-mode
      (setq-local line-spacing prose-minor-mode-line-spacing)
    (setq-local line-spacing prose-minor-mode-line-spacing-default)))

(defun prose-minor-mode-toggle-variable-pitch ()
  "Toggle 'variable-pitch-mode' when enabled."
  (when prose-minor-mode-variable-pitch
    (if (not prose-minor-mode)
        (variable-pitch-mode -1)
      ;; Ensure some faces inherit fixed pitch without overriding the initially
      ;; inherited faces
      (set-face-attribute 'variable-pitch nil :height (+ 1.0 prose-minor-mode-line-spacing))
      (dolist (face (alist-get major-mode prose-minor-mode-fixed-pitch-faces))
        (let* ((old-inherit (face-attribute face :inherit))
               (new-inherit (if (listp old-inherit)
                                (cl-adjoin 'fixed-pitch old-inherit)
                              (list 'fixed-pitch))))
          (set-face-attribute face nil :inherit new-inherit)))
      (variable-pitch-mode 1))))

(defun prose-minor-mode-toggle-spell-check ()
  "Toggle spell-checking when enabled."
  (when (and prose-minor-mode-spell-check prose-minor-mode-spell-check-function)
    (if (not prose-minor-mode)
        (funcall prose-minor-mode-spell-check-function -1)
      (when-let (pred (prose-minor-mode-spell-checking-predicate))
        (set prose-minor-mode-spell-check-predicate pred))
      (funcall prose-minor-mode-spell-check-function 1))))


;; * Minor mode

(defvar prose-minor-mode-map (make-sparse-keymap))

(define-minor-mode prose-minor-mode
  "Minor mode for writing prose"
  :lighter "Prose"
  :keymap prose-minor-mode-map
  (progn
    (prose-minor-mode-toggle-line-spacing)
    (prose-minor-mode-toggle-variable-pitch)
    (prose-minor-mode-toggle-spell-check)))


;; * Integrations

(use-package text-mode
  :hook ((text-mode . prose-minor-mode)))

(use-package org
  :hook ((org-mode . prose-minor-mode)))

(use-package markdown-mode
  :hook ((markdown-mode . prose-minor-mode)))

(provide 'prose-minor-mode)
;;; prose-minor-mode.el ends here
