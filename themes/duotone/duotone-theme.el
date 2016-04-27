;;; duotone-theme.el --- Duotone themes -*- lexical-binding: t ; -*-

;;; Commentary:

;; TODO Hook into 'after-load-functions' and remap faces as packages are loaded

;;; Code:

(require 'cl-lib)
(require 'chroma)
(require 'theme-helpers)
(require 'duotone-palette)
(require 'duotone-faces)
(require 'duotone-presets)


;; * Customs

(defcustom duotone-background-comments t
  "Blend the background of comments."
  :group 'duotone
  :type 'boolean)

(defcustom duotone-background-doc t
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

(defun duotone--set-palette (palette &optional remap-faces-p)
  "Set the default 'PALETTE' optionally remapping faces."
  (disable-theme 'duotone)
  (setq duotone-default-palette palette)
  (load-theme 'duotone t))

(defun duotone-invert ()
  (interactive)
  (duotone--set-palette (duotone-palette-invert duotone-default-palette)))

(defun duotone-select-palette (&optional remap-faces-p)
  "Interactively select a duotone palette and (re)load the
theme. Remap all faces when called with a prefix argument."
  (interactive "P")
  (ivy-read
   "Palette:" duotone-palette-default-palettes
   :action
   (lambda (selection)
     (duotone--set-palette (symbol-value (intern selection )) remap-faces-p))))


;; * Hierarchical faces mapping

(defun duotone-hierarchical-face-index (face)
  "Return a zero-based index for the first digit found in 'FACE''s name."
  (when-let ((s (symbol-name face))
             (index (string-match "\\([0-9]\\)" s)))
    (max 0 (- (string-to-number (match-string 0 s)) 1))    ))

(defun duotone-hierarchical-chroma-mapping (palette function faces &optional colors-list)
  (let ((colors-list (or colors-list duotone-hierarchical-chroma-mapping)))
    (cl-mapcar
     (lambda (face)
       (when-let ((index (duotone-hierarchical-face-index face))
                  (slot (or (nth index colors-list) (car (last colors-list))))
                  (chrom (funcall slot palette)))
         (funcall function face chrom)))
     faces)))


;; * Theme definition

(deftheme duotone "Duotone theme")

;; ** Custom set faces

(let ((palette duotone-default-palette))
  (with-slots (uno-1 uno-2 uno-3 uno-4
                     duo-1 duo-2 duo-3
                     fg bg accent
                     renamed added modified removed
                     blend sblend fade sfade) palette
    (let ((mblend 0.7))
      (custom-theme-set-variables
       'duotone
       `(beacon-color ,(chroma-to-string duo-2))
       `(pos-tip-foreground-color ,(chroma-to-string (chroma-fade fg 3)))
       `(pos-tip-background-color ,(chroma-to-string (chroma-fade bg 3)))
       `(ansi-color-names-vector
         ,(apply 'vector
                 (mapcar #'chroma-to-string
                         (list fg duo-2 uno-2 modified uno-1 duo-1 uno-3 bg)))))

      (theme-custom-set-faces
       'duotone
       `(
         ;;
         ;; Hierarchies:
         ;;
         ,@(duotone-hierarchical-chroma-mapping
            palette
            #'(lambda (face chrom)
                `(,face ((t (:fg ,chrom :bg ,(chroma-blend bg chrom sfade))))))
            duotone-hierarchies)

         ;; Subtle highlighting in mini-buffer
         ,@(duotone-hierarchical-chroma-mapping
            palette
            #'(lambda (face chrom)
                `(,face ((t (:underline '(:style line :color chrom))))))
            duotone-mini-buffer-match-hierarchies)

         ;; Stand-out when highlighting in buffer
         ,@(duotone-hierarchical-chroma-mapping
            palette
            #'(lambda (face chrom)
                `(,face ((t (:f ,chrom :b ,(chroma-blend chrom bg mblend))))))
            duotone-in-buffer-match-hierarchies)

         ;;
         ;; Faces
         ;;
         (default                          ((t (:f ,uno-1 :b ,bg))))
         (cursor                           ((t (:b ,accent))))
         (region                           ((t (:b ,(chroma-blend uno-2 bg blend)))))
         (highlight-numbers-number         ((t (:f ,duo-1))))
         (org-code                         ((t (:f ,uno-2))))
         (outline-1                        ((t (:w semibold))))
         (org-level-1                      ((t (:w semibold))))
         (outshine-level-1                 ((t (:= org-level-1))))
         (outshine-level-2                 ((t (:= org-level-2))))
         (pulse-eval-face                  ((t (:b ,(chroma-blend accent bg blend)))))
         (page-break-lines                 ((t (:f ,uno-4 :b ,(chroma-blend bg uno-4 sblend)))))
         (avy-background-face              ((t (:f ,(chroma-fade bg 30)))))
         (avy-lead-face-0                  ((t (:f ,uno-2 :b ,bg :w semibold))))
         (avy-lead-face-1                  ((t (:f ,duo-2 :b ,bg))))
         (avy-lead-face-2                  ((t (:f ,duo-1 :b ,bg :w semibold))))
         (avy-lead-face                    ((t (:f ,duo-2 :b ,bg :w semibold))))
         (ivy-match-required               ((t (:f ,duo-2))))
         (ivy-confirm-face                 ((t (:f ,duo-1))))
         (cider-stacktrace                 ((t (:b ,bg))))
         (tooltip                          ((t (:f ,fg :b ,(chroma-fade bg 3)))))
         (company-preview                  ((t (:f ,fg :b ,duo-3))))
         (company-tooltip-search           ((t (:f ,bg :b ,duo-2))))
         (company-preview-common           ((t (:f ,duo-3 :b ,bg))))
         (company-preview                  ((t (:f ,duo-2 :b ,bg))))
         (company-tooltip                  ((t (:f ,fg :b ,(chroma-fade bg 3)))))
         (company-tooltip-common           ((t (:f ,uno-4 :b ,bg))))
         (company-tooltip-common-selection ((t (:f ,uno-4 :b ,bg))))
         (company-tooltip-selection        ((t (:f ,duo-1 :b ,(chroma-fade bg fade)))))
         (company-scrollbar-fg             ((t (:b ,uno-4))))
         (company-scrollbar-bg             ((t (:b ,(chroma-fade bg sfade)))))
         (magit-section-face               ((t (:b ,(chroma-fade bg sfade)))))
         (magit-section-highlight          ((t (:b ,(chroma-fade bg sfade)))))
         (magit-diff-context-highlight     ((t (:b ,(chroma-fade bg sfade)))))
         (whitespace-line                  ((t (:f nil :b ,(chroma-fade bg sblend)))))
         (whitespace-space                 ((t (:f ,uno-4 :b ,bg))))
         (whitespace-newline               ((t (:f ,uno-4 :b ,bg))))
         (whitespace-tab                   ((t (:f ,uno-4 :b ,bg))))
         (whitespace-trailing              ((t (:f ,bg :b ,(chroma-blend bg removed blend)))))
         (whitespace-empty                 ((t (:f nil :b ,(chroma-blend bg modified blend)))))
         (whitespace-space-after-tab       ((t (:f nil :b ,(chroma-blend bg modified blend)))))
         (whitespace-space-before-tab      ((t (:f nil :b ,(chroma-blend bg renamed blend)))))
         (anzu-mode-line                   ((t (:f ,bg :w semibold))))
         (mode-line                        ((t (:f ,bg :b ,duo-2 :w semibold))))
         (mode-line-buffer-id              ((t (:f ,bg :b nil))))
         (mode-line-inactive               ((t (:f ,bg :b ,uno-4 :w semibold))))
         (spaceline-highlight-face         ((t (:f nil :b ,duo-1))))
         (spaceline-modified               ((t (:f nil :b ,uno-3))))
         (powerline-active1                ((t (:f nil :b ,duo-2))))
         (powerline-active2                ((t (:f nil :b ,(chroma-fade bg sfade)))))
         (powerline-inactive1              ((t (:f nil :b ,uno-4))))
         (powerline-inactive2              ((t (:f nil :b ,(chroma-fade bg sfade)))))
         ;;
         ;; Face groups (defined in duotone-faces.el)
         ;;
         (,duotone-button ((t (:f ,uno-3 :b ,bg :box nil :underline t))))
         (,duotone-black                   ((t (:f ,fg))))
         (,duotone-blue                    ((t (:f ,duo-2))))
         (,duotone-cyan                    ((t (:f ,duo-3))))
         (,duotone-green                   ((t (:f ,duo-1))))
         (,duotone-magenta                 ((t (:f ,uno-3))))
         (,duotone-red                     ((t (:f ,uno-1))))
         (,duotone-white                   ((t (:f ,uno-4))))
         (,duotone-yellow                  ((t (:f ,uno-2))))
         (,duotone-headers                 ((t (:bg ,(chroma-fade bg fade)))))
         (,duotone-punctuations            ((t (:f ,uno-4))))
         (,duotone-errors                  ((t (:b ,(chroma-blend removed bg blend)))))
         (,duotone-warnings                ((t (:b ,(chroma-blend modified bg blend)))))
         (,duotone-infos                   ((t (:b ,(chroma-blend added bg blend)))))
         (,duotone-marks                   ((t (:b ,uno-3))))
         (,duotone-marked-marks            ((t (:b ,uno-4))))
         (,duotone-matches-current         ((t (:f ,duo-1 :b ,(chroma-blend duo-1 bg mblend)))))
         (,duotone-matches                 ((t (:f ,duo-2 :b ,(chroma-blend duo-2 bg mblend)))))
         (,duotone-subtle-matches          ((t (:f nil :b ,(chroma-blend duo-2 bg blend)))))
         (,duotone-editing-occurrences     ((t (:f nil :b ,(chroma-blend duo-1 bg mblend) :w bold))))
         (,duotone-occurrences-current     ((t (:f ,fg :b ,(chroma-blend duo-1 bg mblend)))))
         (,duotone-occurrences             ((t (:f ,fg :b ,(chroma-blend duo-2 bg mblend) :w semibold))))
         (,duotone-constants               ((t (:f ,duo-2))))
         (,duotone-properties              ((t (:f ,uno-3))))
         (,values                          ((t (:f ,duo-1))))
         (,duotone-variables               ((t (:f ,uno-3 :w semibold))))
         (,duotone-functions               ((t (:f ,uno-2 :w semibold))))
         (,duotone-methods                 ((t (:f ,duo-1))))
         (,duotone-types                   ((t (:f ,uno-2))))
         (,duotone-keywords                ((t (:f ,duo-2 :w semibold))))
         (,duotone-tags                    ((t (:f ,uno-1))))
         (,duotone-imports                 ((t (:f ,duo-2))))
         (,duotone-builtins                ((t (:f ,uno-2 :w semibold))))
         (,duotone-comments                ((t (:f ,uno-4 :b ,(when duotone-background-comments (chroma-blend bg uno-4 sblend))))))
         (,duotone-strings                 ((t (:f ,uno-1))))
         (,duotone-doc                     ((t (:f ,duo-1 :b ,(when duotone-background-doc      (chroma-blend bg duo-1 sblend))))))
         (,duotone-tags                    ((t (:f ,uno-1))))
         (,duotone-additions               ((t (:f ,added))))
         (,duotone-deletions               ((t (:f ,removed))))
         (,duotone-changes                 ((t (:f ,modified))))
         (,duotone-current-line            ((t (:b ,(chroma-fade bg sfade)))))
         (,duotone-files                   ((t (:f ,uno-2))))
         (,duotone-directories             ((t (:f ,duo-2))))
         (,duotone-executables             ((t (:f ,uno-3))))
         (,duotone-prompts                 ((t (:f ,duo-1))))
         (,duotone-highlights              ((t (:b ,(chroma-blend uno-2 bg mblend) :w semibold))))
         (,duotone-gutters                 ((t (:f ,uno-4 :b ,(chroma-darken bg sblend)))))
         (,duotone-deletions               ((t (:f ,removed))))
         (,duotone-additions               ((t (:f ,added))))
         (,duotone-changes                 ((t (:f ,modified))))
         (,duotone-todos                   ((t (:f ,duo-1 :w bold))))
         (,duotone-dones                   ((t (:b ,duo-3))))
         (,duotone-verbatims               ((t (:f ,uno-3))))))))

;;;###autoload
  (when load-file-name
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory (file-name-directory load-file-name)))))

(provide-theme 'duotone)
;;; duotone-theme.el ends here
