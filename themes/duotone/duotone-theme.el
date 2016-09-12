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

(defun duotone-theme-apply ()
  (let ((palette duotone-default-palette))
    (with-slots (uno-1 uno-2 uno-3 uno-4
                       duo-1 duo-2 duo-3
                       fg bg accent
                       renamed added modified removed
                       blend sblend mblend fade sfade) palette
      (let ((mfade (* 3 fade)))
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
                  `(,face ((t (:fg ,chrom :bg nil)))))
              duotone-hierarchies)

           ;; Subtle highlighting in mini-buffer
           ,@(duotone-hierarchical-chroma-mapping
              palette
              #'(lambda (face chrom)
                  `(,face ((t (:underline `(:style line :color ,chrom))))))
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
           (cursor                           ((t (:f ,accent :b ,uno-1))))
           (region                           ((t (:b ,(chroma-blend uno-2 bg blend)))))
           (highlight-numbers-number         ((t (:f ,duo-1))))
           (org-document-title               ((t (:f ,duo-1))))
           (org-document-info-keyword        ((t (:f ,duo-3))))
           (org-code                         ((t (:f ,uno-2))))
           (org-block-begin-line             ((t (:f ,uno-4))))
           (org-block-end-line               ((t (:f ,uno-4))))
           (pulse-eval-face                  ((t (:b ,(chroma-blend modified bg mblend)))))
           (page-break-lines                 ((t (:f ,uno-4))))
           (avy-background-face              ((t (:f ,(chroma-fade bg 30)))))
           (avy-lead-face-0                  ((t (:f ,uno-2 :b ,bg))))
           (avy-lead-face-1                  ((t (:f ,duo-2 :b ,bg))))
           (avy-lead-face-2                  ((t (:f ,duo-1 :b ,bg))))
           (avy-lead-face                    ((t (:f ,duo-2 :b ,bg))))
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
           (anzu-mode-line                   ((t (:f ,bg))))
           (mode-line-inactive               ((t (:f ,fg :b ,(chroma-blend bg uno-2 sblend)))))
           (mode-line-buffer-id              ((t (:f nil :b nil :weight bold))))
           (mode-line                        ((t (:f ,bg :b ,duo-2))))
           (powerline-active1                ((t (:f ,bg :b ,duo-2))))
           (powerline-active2                ((t (:f nil :b ,(chroma-fade bg fade)))))
           (mode-line-inactive               ((t (:f ,fg :b ,(chroma-blend bg fg sblend)))))
           (powerline-inactive1              ((t (:f ,fg :b ,(chroma-blend bg uno-1 sblend)))))
           (powerline-inactive2              ((t (:f ,fg :b ,(chroma-fade bg fade)))))
           (ahs-face                         ((t (:b ,(chroma-fade bg fade)))))
           (ahs-plugin-defalt-face           ((t (:b ,(chroma-fade bg mfade)))))
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
           (,duotone-errors                  ((t (:f ,removed :b ,(chroma-blend bg removed sblend)))))
           (,duotone-warnings                ((t (:f ,renamed :b ,(chroma-blend bg renamed sblend)))))
           (,duotone-infos                   ((t (:f ,added :b ,(chroma-blend bg added sblend)))))
           (,duotone-marks                   ((t (:b ,uno-3))))
           (,duotone-marked-marks            ((t (:b ,uno-4))))
           (,duotone-matches-current         ((t (:f ,duo-1 :b ,(chroma-blend duo-1 bg mblend)))))
           (,duotone-matches                 ((t (:f ,duo-2 :b ,(chroma-blend duo-2 bg mblend)))))
           (,duotone-subtle-matches          ((t (:f nil :b ,(chroma-blend bg duo-2 sblend)))))
           (,duotone-editing-occurrences     ((t (:f nil :b ,(chroma-blend duo-1 bg mblend)))))
           (,duotone-occurrences-current     ((t (:b ,(chroma-fade bg mfade)))))
           (,duotone-occurrences             ((t (:b ,(chroma-fade bg fade)))))
           (,duotone-constants               ((t (:f ,duo-2))))
           (,duotone-properties              ((t (:f ,uno-3))))
           (,values                          ((t (:f ,duo-1))))
           (,duotone-variables               ((t (:f ,duo-2))))
           (,duotone-functions               ((t (:f ,uno-2))))
           (,duotone-methods                 ((t (:f ,duo-1))))
           (,duotone-types                   ((t (:f ,duo-1))))
           (,duotone-keywords                ((t (:f ,duo-1))))
           (,duotone-tags                    ((t (:f ,uno-1))))
           (,duotone-imports                 ((t (:f ,duo-2))))
           (,duotone-builtins                ((t (:f ,uno-2))))
           (,duotone-comments                ((t (:f ,uno-2 :b ,(if duotone-background-comments (chroma-blend bg uno-2 sblend) bg)))))
           (,duotone-strings                 ((t (:f ,duo-1))))
           (,duotone-doc                     ((t (:f ,uno-2 :b ,(if duotone-background-doc (chroma-blend bg uno-2 sblend) bg)))))
           (,duotone-tags                    ((t (:f ,uno-1))))
           (,duotone-current-line            ((t (:b ,(chroma-fade bg sfade)))))
           (,duotone-files                   ((t (:f ,uno-2))))
           (,duotone-directories             ((t (:f ,duo-2))))
           (,duotone-executables             ((t (:f ,uno-3))))
           (,duotone-prompts                 ((t (:f ,duo-1))))
           (,duotone-highlights              ((t (:b ,(chroma-blend uno-1 bg mblend)))))
           (,duotone-gutters                 ((t (:f ,uno-4 :b ,bg))))
           (,duotone-separators              ((t (:f ,duo-1 :b ,bg))))
           (,duotone-deletions               ((t (:f ,removed))))
           (,duotone-additions               ((t (:f ,added))))
           (,duotone-changes                 ((t (:f ,modified))))
           (,duotone-todos                   ((t (:f ,duo-1 :w bold))))
           (,duotone-dones                   ((t (:b ,duo-3))))
           (,duotone-verbatims               ((t (:f ,uno-3))))))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(duotone-theme-apply)

(defun duotone-theme-apply-if-enabled (&optional _)
  "Apply the duotone theme if currently enabled."
  (when (member 'duotone custom-enabled-themes)
    (duotone-theme-apply)))


(defun duotone-theme-install-reload-hook ()
  "Install a hook to apply the theme when a file is loaded."
  (add-hook 'after-load-functions #'duotone-theme-apply-if-enabled))

(add-hook 'after-init-hook #'duotone-theme-install-reload-hook)

(provide-theme 'duotone)
;;; duotone-theme.el ends here
