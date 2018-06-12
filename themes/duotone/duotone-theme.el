;;; duotone-theme.el --- Duotone themes -*- lexical-binding: t ; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cl-extra)
(require 'chroma)
(require 'theme-helpers)
(require 'duotone-palette)
(require 'duotone-presets)


;; * Customs

(defcustom duotone-background-comments nil
  "Blend the background of comments."
  :group 'duotone
  :type 'boolean)

(defcustom duotone-background-doc nil
  "Blend the background of documentation strings."
  :group 'duotone
  :type 'boolean)

(defcustom duotone-hierarchical-chroma-mapping
  '(uno-1 duo-1 uno-2 duo-2 uno-3 duo-3 uno-4)
  "Palette slots ordering for hierarchical faces."
  :group 'duotone
  :type 'list)

(defcustom duotone-default-palette duotone-palette-duotone-light
  "Default duotone palette."
  :group 'duotone
  :type 'symbol)


;; * Commands

(defun duotone--set-palette (palette)
  "(Re)-enable duotone with `PALETTE'."
  (disable-theme 'duotone)
  (setq duotone-default-palette palette)
  (load-theme 'duotone t))

(defun duotone-select-palette (&optional _)
  "Select a palette and (re)load the theme.
Remap all faces when called with a prefix argument."
  (interactive "P")
  (ivy-read
   "Palette:" duotone-palette-default-palettes
   :action
   (lambda (selection)
     (duotone--set-palette (symbol-value (intern selection ))))))


;; * Hierarchical faces mapping

(defun duotone-hierarchical-face-index (face)
  "Return a zero-based index for the first digit found in 'FACE''s name."
  (when-let* ((s (symbol-name face))
              (index (string-match "\\([0-9]\\)" s)))
    (max 0 (- (string-to-number (match-string 0 s)) 1))    ))

(defun duotone-hierarchical-chroma-mapping (palette function faces &optional colors-list)
  (let ((colors-list (or colors-list duotone-hierarchical-chroma-mapping)))
    (cl-mapcar
     (lambda (face)
       (when-let* ((index (duotone-hierarchical-face-index face))
                   (slot (or (nth index colors-list) (car (last colors-list))))
                   (chrom (funcall slot palette)))
         (funcall function face chrom)))
     faces)))


;; * Theme definition

(deftheme duotone "Duotone theme")

;; ** Custom set faces

(defun duotone-theme-apply ()
  "Apply the current `duotone-default-palette'."
  (let ((palette duotone-default-palette))
    (with-slots
        (
         uno-1 uno-1-fade uno-1-mfade uno-1-sfade
         uno-2 uno-2-fade uno-2-mfade uno-2-sfade
         uno-3 uno-3-fade uno-3-mfade uno-3-sfade
         uno-4 uno-4-fade uno-4-mfade uno-4-sfade
         duo-1 duo-1-fade duo-1-mfade duo-1-sfade
         duo-2 duo-2-fade duo-2-mfade duo-2-sfade
         duo-3 duo-3-fade duo-3-mfade duo-3-sfade
         fg fg-fade fg-mfade fg-sfade
         bg bg-fade bg-mfade bg-sfade
         accent
         renamed added modified removed
         blend mblend sblend
         fade mfade sfade) palette
      ;;
      ;; Variables
      ;;
      (custom-theme-set-variables
       'duotone
       `(pos-tip-foreground-color ,(chroma-to-string fg-sfade))
       `(pos-tip-background-color ,(chroma-to-string bg-sfade))
       `(ansi-color-names-vector ,(apply 'vector (mapcar #'chroma-to-string (list fg duo-2 uno-2 modified uno-1 duo-1 uno-3 bg)))))
      ;;
      ;; Faces
      ;;
      (theme-custom-set-faces
       'duotone
       `(;;
         ;; Hierarchies:
         ;;
         ,@(duotone-hierarchical-chroma-mapping
            palette
            #'(lambda (face chrom)
                `(,face ((t (:fg ,chrom :bg nil)))))
            (theme-faces-match '("[0-9]" "swiper" "match")))

         ;; Underline in mini-buffer
         ,@(duotone-hierarchical-chroma-mapping
            palette
            #'(lambda (face chrom)
                `(,face ((t (:underline `(:style line :color ,chrom))))))
            (theme-faces-match-all "match" "[0-9]"))

         ;; Stand-out highlighting in buffer
         ,@(duotone-hierarchical-chroma-mapping
            palette
            #'(lambda (face chrom)
                `(,face ((t (:f ,(chroma-blend chrom fg mblend) :b ,(chroma-blend chrom bg mblend))))))
            (append
             (theme-faces-match-all '"match" "[0-9]")
             (theme-faces-match-all "mark" "[0-9]")
             (theme-faces-match-all "lead")))
         ;;
         ;; Face groups
         ;;
         ;; Links
         (,(theme-faces-match '("link" "visited"))
          ((t (:f ,duo-2 :box nil :underline t))))
         (,(theme-faces-match-all "link" "visited")
          ((t (:f ,duo-3 :box nil :underline t))))
         ;;
         ;; Gui
         (,(theme-faces-match "button")
          ((t (:f ,uno-3 :b ,bg :box nil :underline t))))
         ;;
         ;; Colors
         (,(theme-faces-match "black")
          ((t (:f ,fg))))
         (,(theme-faces-match "blue")
          ((t (:f ,duo-2))))
         (,(theme-faces-match "cyan")
          ((t (:f ,duo-3))))
         (,(theme-faces-match "green")
          ((t (:f ,duo-1))))
         (,(theme-faces-match "magenta" "pink" "purple")
          ((t (:f ,uno-3))))
         (,(theme-faces-match "red" "maroon")
          ((t (:f ,uno-1))))
         (,(theme-faces-match "white" "silver")
          ((t (:f ,uno-4))))
         (,(theme-faces-match "yellow" "orange")
          ((t (:f ,uno-2))))
         ;;
         ;; Structure formatting
         (,(theme-faces-match "-header$" "heading")
          ((t (:bg ,bg-sfade))))
         (,(theme-faces-match '("separat"))
          ((t (:f ,duo-1 :b ,bg))))
         (,(theme-faces-match '("delimiter" "rainbow") "break")
          ((t (:f ,uno-4))))
         (,(theme-faces-match "whitespace" "trailing")
          ((t (:f ,removed :b ,(chroma-blend bg removed sblend)))))
         ;;
         ;; Errors etc
         (,(theme-faces-match '("error" "fail" "info" "warning" "list") "\\(mis\\|un\\)match" "in\\(correct\\|valid\\)" "missing")
          ((t (:f ,removed :underline `(:style line :color ,(chroma-blend bg removed sblend))))))
         (,(theme-faces-match "warning")
          ((t (:f ,renamed :underline `(:style line :color ,(chroma-blend bg renamed sblend))))))
         (,(theme-faces-match '("success" "info" "visited" "list" "[0-9]"))
          ((t (:f ,added :underline `(:style line :color ,(chroma-blend bg added sblend))))))
         (,(theme-faces-match '("stacktrace"))
          ((t (:f ,added :underline nil))))
         ;; Marks
         ;;
         (,(theme-faces-match '("mark" "marked" "book" "bm" "markdown" "1" "2"))
          ((t (:b ,uno-3))))
         ;;
         ;; Matches
         (,(theme-faces-match-all "match" "current")
          ((t (:f ,duo-1 :b ,(chroma-blend bg duo-1 sblend)))))
         (,(cl-concatenate
            'list
            (theme-faces-match '("match" "\\(mis\\|un\\)match"  "[0-9]"))
            (theme-faces-match '("required" "common" "current")))
          ((t (:f ,duo-1 :b ,(chroma-blend bg duo-2 mblend)))))
         ((show-paren-match show-paren-match-expression)
          ((t (:f nil :b ,(chroma-blend bg duo-3 sblend)))))
         ;;
         ;; Occurrences
         (,(theme-faces-match-all "occur")
          ((t (:f nil :b ,duo-1))))
         ((lazy-highlight secondary-selection)
          ((t (:f ,bg :b ,uno-2))))
         ((isearch)
          ((t (:f ,bg :b ,duo-1))))
         (,(theme-faces-match "occur")
          ((t (:b ,bg-fade))))
         ;;
         ;; Selection
         (,(cl-concatenate
            'list
            '(hl-line helm-selection line-number-current-line)
            (theme-faces-match "selection" "selected"))
          ((t (:b ,(chroma-blend bg uno-4 sblend)))))
         ;;
         ;; Filesystem
         (,(theme-faces-match '("tag" "file" "current" "label" "task"))
          ((t (:f ,uno-1 :b ,(chroma-blend bg duo-3 sblend)))))
         (,(theme-faces-match "dir" "directory" "subdir")
          ((t (:f ,duo-2))))
         (,(theme-faces-match "exec")
          ((t (:f ,uno-3))))
         ;;
         ;; Interactive
         (,(theme-faces-match "prompt" "required")
          ((t (:f ,duo-1))))
         ;;
         ;; Highlights
         (,(theme-faces-match '(("highlight" "magit" "match") "warning" "error" "mode-line" "lazy" "number"))
          ((t (:b ,(chroma-blend bg uno-1 sblend)))))
         ;;
         ;; Fringe elements
         (,(theme-faces-match "diff-hl" "git-gutter" "border")
          ((t (:f ,uno-3 :b ,bg))))
         ;;
         ;; Todos
         (,(theme-faces-match "todo")
          ((t (:f ,duo-1 :b nil :w bold))))
         (,(theme-faces-match "done")
          ((t (:f ,uno-4 :w bold))))
         (,(theme-faces-match "quote" "verbatim")
          ((t (:f ,uno-1))))
         ;;
         ;; Font lock
         (,(theme-faces-match "preproc")
          ((t (:f ,duo-3))))
         (,(theme-faces-match "constant")
          ((t (:f ,duo-2))))
         (,(theme-faces-match "properties")
          ((t (:f ,uno-3))))
         (,(theme-faces-match '("variable" "value"))
          ((t (:f ,duo-2))))
         (,(theme-faces-match "value")
          ((t (:f ,duo-1))))
         (,(theme-faces-match "function")
          ((t (:f ,duo-1))))
         (,(theme-faces-match "method")
          ((t (:f ,duo-1))))
         (,(theme-faces-match "type")
          ((t (:f ,duo-2))))
         (,(theme-faces-match "keyword")
          ((t (:f ,duo-3))))
         (,(theme-faces-match "import")
          ((t (:f ,duo-2))))
         (,(theme-faces-match "builtin")
          ((t (:f ,uno-2))))
         (,(theme-faces-match '("comment" "git" "delimiter"))
          ((t (:f ,uno-3 :b ,(when duotone-background-comments (chroma-blend bg uno-2 sblend))))))
         (,(theme-faces-match '("string"))
          ((t (:f ,uno-2))))
         (,(theme-faces-match '("negat"))
          ((t (:f ,duo-2))))
         (,(theme-faces-match '("char" "avy" "negation"))
          ((t (:f ,uno-1))))
         (,(theme-faces-match '("doc" "docview"))
          ((t (:f ,uno-1 :b ,(when duotone-background-doc (chroma-blend bg uno-2 sblend))))))
         ;;
         ;; Faces overrides
         ;;
         (default                                    ((t (:f ,fg :b ,bg))))
         (fixed-pitch                                ((t (:font-family nil))))
         (fringe                                     ((t (:f ,fg :b ,bg))))
         (cursor                                     ((t (:f ,accent :b ,duo-1))))
         (region                                     ((t (:b ,duo-2-fade))))
         (match                                      ((t (:f ,bg :b ,accent))))
         (ansible::task-label-face                   ((t (:f ,duo-2))))
         (hl-sexp-face                               ((t (:b ,bg-sfade))))
         (highlight-numbers-number                   ((t (:f ,duo-1))))
         (highlight-symbol-face                      ((t (:f nil :b nil :underline t))))
         (org-table                                  ((t (:f ,uno-3 :b ,(chroma-blend bg uno-3 0.03)))))
         (org-level-1                                ((t (:weight semibold))))
         (outshine-level-1                           ((t (:weight semibold))))
         (org-document-title                         ((t (:f ,duo-1))))
         (org-document-info-keyword                  ((t (:f ,duo-3))))
         (org-code                                   ((t (:f ,uno-2))))
         (org-block-begin-line                       ((t (:f ,uno-4 :b ,(chroma-blend bg uno-3 0.03)))))
         (org-block                                  ((t (:b ,(chroma-blend bg uno-3 0.03)))))
         (org-block-end-line                         ((t (:f ,uno-4 :b ,(chroma-blend bg uno-3 0.03)))))
         (page-break-lines                           ((t (:f ,uno-4))))
         (aw-leading-char-face                       ((t (:f ,uno-1 :bg ,(chroma-blend bg uno-1 sblend)))))
         (avy-background-face                        ((t (:f ,(chroma-blend uno-4 bg sblend)))))
         (avy-goto-char-timer-face                   ((t (:f nil :b ,(chroma-blend uno-2 bg mblend)))))
         (avy-lead-face-0                            ((t (:f ,duo-1 :bg ,(chroma-blend bg duo-1 sblend)))))
         (avy-lead-face                              ((t (:f ,uno-1 :bg ,(chroma-blend bg uno-1 sblend)))))
         (avy-lead-face-1                            ((t (:f ,duo-2 :bg ,(chroma-blend bg duo-2 sblend)))))
         (avy-lead-face-2                            ((t (:f ,duo-3 :bg ,(chroma-blend bg duo-3 sblend)))))
         (ivy-modified-buffer                        ((t (:f ,uno-2))))
         (ivy-match-required                         ((t (:f ,uno-1))))
         (ivy-confirm-face                           ((t (:f ,duo-3))))
         (ivy-current-match                          ((t (:f ,bg :b ,duo-1))))
         (cider-stacktrace                           ((t (:b ,bg))))
         (cider-enlightened-face                     ((t (:f ,duo-1 :b ,bg-fade))))
         (cider-result-overlay-face                  ((t (:f ,duo-1 :b ,bg-sfade :border nil))))
         (cider-enlightened-local-face               ((t (:f ,duo-3 :b ,bg-mfade))))
         (tooltip                                    ((t (:f ,fg :b ,bg-sfade :inherit default))))
         (company-preview                            ((t (:f ,fg :b ,uno-3))))
         (company-tooltip-search                     ((t (:f ,bg :b ,uno-3))))
         (company-tooltip-search-selection           ((t (:f ,(chroma-fade bg-sfade sfade) :b ,duo-2 ))))
         (company-preview-common                     ((t (:f ,duo-2 :b ,bg))))
         (company-preview-search                     ((t (:f ,bg :b ,uno-2))))
         (company-preview                            ((t (:f ,duo-2 :b ,bg))))
         (company-tooltip                            ((t (:f ,uno-3 :b ,bg-sfade))))
         (company-tooltip-common                     ((t (:f ,(chroma-fade fg-sfade sfade) :b ,bg-sfade))))
         (company-tooltip-common-selection           ((t (:f ,fg :b ,(chroma-fade bg-sfade sfade)))))
         (company-tooltip-selection                  ((t (:f ,fg :b ,(chroma-fade bg-sfade sfade)))))
         (company-scrollbar-fg                       ((t (:b ,uno-4))))
         (company-scrollbar-bg                       ((t (:b ,bg-sfade))))
         (whitespace-line                            ((t (:f nil :b ,bg-sfade))))
         (whitespace-space                           ((t (:f ,uno-4 :b ,bg))))
         (whitespace-newline                         ((t (:f ,uno-4 :b ,bg))))
         (whitespace-tab                             ((t (:f ,uno-4 :b ,bg))))
         (whitespace-trailing                        ((t (:f ,bg :b ,(chroma-blend bg removed sblend)))))
         (whitespace-empty                           ((t (:f nil :b ,(chroma-blend bg modified sblend)))))
         (whitespace-space-after-tab                 ((t (:f nil :b ,(chroma-blend bg modified sblend)))))
         (whitespace-space-before-tab                ((t (:f nil :b ,(chroma-blend bg renamed sblend)))))
         (lisp-extra-font-lock-backquote             ((t (:f ,accent))))
         (lisp-extra-font-lock-quoted                ((t (:f ,duo-2))))
         (lisp-extra-font-lock-quoted-function       ((t (:f ,duo-1))))
         (lisp-extra-font-lock-special-variable-name ((t (:f ,duo-3))))
         (lisp-extra-font-lock-backquote             ((t (:f ,accent))))
         (clojure-keyword-face                       ((t (:f ,duo-2 :weight normal))))
         (anzu-mode-line                             ((t (:f ,bg))))
         (linum                                      ((t (:b ,bg :f ,bg-mfade))))
         (mode-line-buffer-id                        ((t (:f nil :b nil :weight semibold))))
         (mode-line-buffer-id-inactive               ((t (:f nil  :b nil :weight semibold))))
         (mode-line                                  ((t (:f ,duo-1 :b ,bg-sfade :weight normal))))
         (mode-line-inactive                         ((t (:f ,uno-4 :b ,bg-sfade))))
         (powerline-active0                          ((t (:f ,uno-4 :b ,bg-sfade))))
         (powerline-active1                          ((t (:f ,uno-4 :b ,bg-sfade))))
         (powerline-active2                          ((t (:f ,uno-4 :b ,bg-sfade))))
         (powerline-inactive0                        ((t (:f ,uno-4 :b ,bg))))
         (powerline-inactive1                        ((t (:f ,uno-4 :b ,bg))))
         (powerline-inactive2                        ((t (:f ,uno-4 :b ,bg))))
         (line-number                                ((t (:f ,bg-mfade :b nil))))
         (line-number-current-line                   ((t (:f ,bg :b ,duo-1))))
         (eyebrowse-mode-line-active                 ((t (:f ,duo-2 :b ,bg))))
         (eyebrowse-mode-line-inactive               ((t (:f ,uno-4 :b ,bg-sfade))))
         (eyebrowse-mode-line-delimiters             ((t (:f ,bg :b ,bg-sfade))))
         (eyebrowse-mode-line-separator              ((t (:f ,bg :b ,bg-sfade))))
         (iedit-read-only-occurrence                 ((t (:f ,bg :b ,uno-3))))
         (iedit-occurrence                           ((t (:fg ,bg :bg ,removed))))
         (compilation-info                           ((t (:f ,duo-3))))
         (ivy-posframe                               ((t (:b ,bg-sfade :f ,fg))))
         (ivy-posframe-cursor                        ((t (:inherit cursor))))
         (window-divider                             ((t (:f ,bg))))
         (window-divider-first-pixel                 ((t (:f ,bg))))
         (window-divider-last-pixel                  ((t (:f ,bg))))
         (merlin-type-face                           ((t (:f ,bg :b ,duo-2))))
         (pulse-eval-face                            ((t (:f ,bg :b ,duo-2-fade)))))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(duotone-theme-apply)

(provide-theme 'duotone)
;;; duotone-theme.el ends here
