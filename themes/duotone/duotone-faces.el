;;; duotone-faces.el --- Duotone face groups
;;; Commentary:
;;
;;; Code:
(require 'theme-helpers)


;; * Font lock

(defvar duotone-constants '(font-lock-constant-face))

(defvar duotone-strings '(font-lock-string-face))

(defvar duotone-doc '(font-lock-doc-face))

(defvar duotone-variables '(font-lock-variable-name-face))

(defvar duotone-functions '(font-lock-function-name-face))

(defvar duotone-types '(font-lock-type-face))

(defvar duotone-builtins '(font-lock-builtin-face))

(defvar duotone-keywords '(font-lock-keyword-face))

(defvar duotone-comments '(font-lock-comment-face))

(defvar duotone-properties '())

(defvar duotone-methods '())

(defvar duotone-imports
  ;; (theme-faces-matching-regexps "import")
  '(font-lock-preprocessor-face))

(defvar duotone-errors
  (theme-faces-matching-regexps
   '("error" "info" "warning" "list")
   "mismatch" "unmatch"
   "incorrect"
   "invalid"
   "missing")
  '(merlin-compilation-error-face
    flycheck-fringe-error
    flycheck-error
    cider-error-highlight-face
    compilation-error
    cider-test-error-face
    cider-stacktrace-error-message-face
    cider-stacktrace-error-class-face
    next-error
    error
    rainbow-delimiters-mismatched-face
    show-paren-mismatch
    magit-cherry-unmatched
    semantic-unmatched-syntax-face
    rainbow-delimiters-unmatched-face
    flyspell-incorrect
    helm-ff-invalid-symlink
    custom-invalid
    markdown-missing-link-face
    eshell-ls-missing
    vc-missing-state))

(defvar duotone-warnings
  ;; (theme-faces-matching-regexps "warning")
  '(cider-warning-highlight-face
    magit-diff-whitespace-warning
    merlin-compilation-warning-face
    flycheck-error-list-warning
    flycheck-fringe-warning
    flycheck-warning
    compilation-warning
    org-warning
    dired-warning
    font-lock-warning-face
    warning))

(defvar duotone-infos
  ;; (theme-faces-matching-regexps '("info" "visited" "list" "[0-9]"))
  '(flycheck-fringe-info
    flycheck-info
    compilation-info
    org-document-info-keyword
    org-document-info
    Info-quoted
    info-index-match
    info-header-node
    info-header-xref
    info-xref
    info-menu-star
    info-menu-header
    info-node))

(defvar duotone-headers
  ;; (theme-faces-matching-regexps '("-header$"))
  '(git-commit-known-pseudo-header
    git-commit-pseudo-header
    log-edit-unknown-header
    log-edit-header
    diff-hunk-header
    diff-file-header
    diff-header
    calendar-month-header
    calendar-weekend-header
    calendar-weekday-header
    dired-header
    info-menu-header))

(defvar duotone-punctuations
  (append
   '(whitespace-empty whitespace-line whitespace-newline whitespace-tab page-break-lines)
   (theme-faces-matching-regexps '("delimiter" "rainbow"))))

(defvar duotone-black (theme-faces-matching-regexps "black"))
(defvar duotone-blue (theme-faces-matching-regexps "blue"))
(defvar duotone-cyan (theme-faces-matching-regexps "cyan"))
(defvar duotone-green (theme-faces-matching-regexps "green"))
(defvar duotone-magenta (theme-faces-matching-regexps "magenta"))
(defvar duotone-red (theme-faces-matching-regexps "red"))
(defvar duotone-white (theme-faces-matching-regexps "white"))
(defvar duotone-yellow (theme-faces-matching-regexps "yellow"))


(defvar duotone-button (theme-faces-matching-regexps "button") )


;; * Static higlighting

(defvar duotone-highlights
  ;; (theme-faces-matching-regexps '("highlight" "warning" "error" "mode-line" "lazy" "number"))
  '(flx-highlight-face
    paradox-highlight-face
    semantic-idle-symbol-highlight
    yas-field-highlight-face
    highlight-symbol-face
    speedbar-highlight-face
    semantic-highlight-func-current-tag-face
    semantic-highlight-edits-face
    comint-highlight-prompt
    comint-highlight-input
    pulse-highlight-face
    pulse-highlight-start-face
    rmail-highlight
    eldoc-highlight-function-argument
    highlight))

(defvar duotone-prompts
  ;; (theme-faces-matching-regexps "prompt" "required")
  '(cider-debug-prompt-face
    cider-repl-prompt-face
    comint-highlight-prompt
    minibuffer-prompt
    ivy-match-required-face))

(defvar duotone-tags
  ;; (theme-faces-matching-regexps '("tag" "file" "current"))
  '(magit-tag
    custom-group-tag
    custom-group-tag-1
    custom-face-tag
    custom-variable-tag
    custom-comment-tag
    semantic-tag-boundary-face
    paradox-commit-tag-face
    speedbar-tag-face
    org-tag-group
    org-agenda-filter-tags
    org-tag))

(defvar duotone-todos
  ;; (theme-faces-matching-regexps "todo")
  '(hl-todo
    org-agenda-dimmed-todo-face
    org-checkbox-statistics-todo
    org-todo))

(defvar duotone-dones
  ;; (theme-faces-matching-regexps "done")
  '(magit-sequence-done
    org-checkbox-statistics-done
    org-headline-done
    org-agenda-done
    org-done))

(defvar duotone-verbatims '(org-verbatim org-quote))


;; * Interactive highlights

(defvar duotone-current-line '(hl-line helm-selection))

(defvar duotone-matches
  ;; (theme-faces-matching-regexps
  ;;  '("match" "face-[0-9]" "unmatch" "required" "mismatch" "common" "current"))
  '(anzu-match-3 anzu-match-2 anzu-match-1 ido-only-match ido-first-match info-index-match match))

(defvar duotone-subtle-matches '(show-paren-match))

(defvar duotone-matches-common (theme-faces-matching-all-regexps "match" "common"))

(defvar duotone-matches-current
  ;; (theme-faces-matching-all-regexps "match" "current")
  '(ivy-current-match))

(defvar duotone-editing-occurrences
  ;; (theme-faces-matching-all-regexps "occur")
  '(iedit-read-only-occurrence iedit-occurrence))

(defvar duotone-gutters
  ;; (theme-faces-matching-regexps "diff-hl" "git-gutter" "border" "fringe")
  '(diff-hl-dired-ignored
    diff-hl-dired-unknown
    diff-hl-dired-change
    diff-hl-dired-delete
    diff-hl-dired-insert
    diff-hl-change
    diff-hl-delete
    diff-hl-insert
    cider-docview-table-border-face
    border
    vertical-border
    flycheck-fringe-info
    flycheck-fringe-warning
    flycheck-fringe-error
    bm-fringe-persistent-face
    bm-fringe-face
    fringe))


;; * Hierarchies

(defvar duotone-hierarchies
  ;; (theme-faces-matching-regexps '("[0-9]" "powerline" "swiper" "match"))
  '(dired-subtree-depth-6-face
    dired-subtree-depth-5-face
    dired-subtree-depth-4-face
    dired-subtree-depth-3-face
    dired-subtree-depth-2-face
    dired-subtree-depth-1-face
    custom-group-tag-1
    rainbow-delimiters-depth-1-face
    rainbow-delimiters-depth-2-face
    rainbow-delimiters-depth-3-face
    rainbow-delimiters-depth-4-face
    rainbow-delimiters-depth-5-face
    rainbow-delimiters-depth-6-face
    rainbow-delimiters-depth-7-face
    rainbow-delimiters-depth-8-face
    rainbow-delimiters-depth-9-face
    outshine-level-8
    outshine-level-7
    outshine-level-6
    outshine-level-5
    outshine-level-4
    outshine-level-3
    outshine-level-2
    outshine-level-1
    org-level-8
    org-level-7
    org-level-6
    org-level-5
    org-level-4
    org-level-3
    org-level-2
    org-level-1
    outline-8
    outline-7
    outline-6
    outline-5
    outline-4
    outline-3
    outline-2
    outline-1
    imenu-list-entry-subalist-face-3
    imenu-list-entry-face-3
    imenu-list-entry-subalist-face-2
    imenu-list-entry-face-2
    imenu-list-entry-subalist-face-1
    imenu-list-entry-face-1
    imenu-list-entry-subalist-face-0
    imenu-list-entry-face-0
    avy-lead-face-2
    avy-lead-face-1
    avy-lead-face-0
    visible-mark-forward-face2
    visible-mark-forward-face1
    visible-mark-face2
    visible-mark-face1
    info-title-4
    info-title-3
    info-title-2
    info-title-1
    markdown-header-face-6
    markdown-header-face-5
    markdown-header-face-4
    markdown-header-face-3
    markdown-header-face-2
    markdown-header-face-1
    helm-bookmark-w3m))

(defvar duotone-outlines
  ;; (theme-faces-matching-regexps "outline\\-[0-9]" "org-level\\-[0-9]" "outshine.+[0-9]")
  '(outline-8
    outline-7
    outline-6
    outline-5
    outline-4
    outline-3
    outline-2
    outline-1
    org-level-8
    org-level-7
    org-level-6
    org-level-5
    org-level-4
    org-level-3
    org-level-2
    org-level-1
    outshine-level-8
    outshine-level-7
    outshine-level-6
    outshine-level-5
    outshine-level-4
    outshine-level-3
    outshine-level-2
    outshine-level-1))

(defvar duotone-in-buffer-match-hierarchies
  ;; (append
  ;;  (theme-faces-matching-all-regexps '("match" "ivy") "[0-9]")
  ;;  (theme-faces-matching-all-regexps "mark" "[0-9]")
  ;;  (theme-faces-matching-all-regexps "lead" "[0-9]"))
  '(swiper-match-face-4
    swiper-match-face-3
    swiper-match-face-2
    swiper-match-face-1
    anzu-match-3
    anzu-match-2
    anzu-match-1
    visible-mark-forward-face2
    visible-mark-forward-face1
    visible-mark-face2
    visible-mark-face1
    avy-lead-face-2
    avy-lead-face-1
    avy-lead-face-0))

(defvar duotone-mini-buffer-match-hierarchies
  (theme-faces-matching-all-regexps "ivy" "match" "[0-9]"))


;; * Anchors

(defvar duotone-links
  ;; (theme-faces-matching-regexps '("link" "symlink" "visited"))
  '(shr-link custom-link org-link link))

(defvar duotone-links-visited
  ;; (theme-faces-matching-regexps "visited")
  '(info-xref-visited link-visited))


;; * Interactive anchors

(defvar duotone-occurrences-current '(isearch))
(defvar duotone-occurrences
  '(lazy-highlight secondary-selection aw-leading-char-face))


;; * VC

(defvar duotone-additions
  ;; (theme-faces-matching-regexps '("added" "indicator") "copied" "insert" "inserted")
  '(magit-diffstat-added
    magit-diff-added-highlight
    magit-diff-added
    smerge-refined-added
    diff-refine-added
    diff-added
    vc-locally-added-state
    diff-hl-dired-insert
    diff-hl-insert))

(defvar duotone-deletions
  ;; (theme-faces-matching-regexps '("removed" "indicator") "delete")
  '(magit-diffstat-removed
    magit-diff-removed-highlight
    magit-diff-removed
    smerge-refined-removed
    diff-refine-removed
    diff-removed
    vc-removed-state
    diff-hl-dired-delete
    diff-hl-delete))

(defvar duotone-changes
  ;; (theme-faces-matching-regexps '("change" "change-log") '("modified" "unmodified" "added") "saved")
  '(custom-changed
    smerge-refined-changed
    diff-hl-dired-change
    diff-hl-change
    diff-refine-changed
    diff-indicator-changed
    diff-changed
    custom-modified
    ivy-modified-buffer))

(defvar duotone-git-tracked '(helm-ls-git-added-copied-face))
(defvar duotone-git-untracked '(helm-ls-git-untracked-face))
(defvar duotone-git-staged (theme-faces-matching-regexps '("staged" "not" "unstaged")))
(defvar duotone-git-unstaged (theme-faces-matching-regexps "unstaged" "not-staged"))


;; * Filesystem

(defvar duotone-directories
  ;; (theme-faces-matching-regexps "directory" "subdir")
  '(helm-ff-dotted-symlink-directory
    helm-ff-dotted-directory
    helm-ff-directory
    helm-buffer-directory
    helm-bookmark-directory
    eshell-ls-directory
    speedbar-directory-face
    dired-directory
    ido-subdir
    ivy-subdir))

(defvar duotone-files
  ;; (theme-faces-matching-regexps "file")
  '(magit-filename
    magit-diff-file-heading-selection
    magit-diff-file-heading-highlight
    magit-diff-file-heading
    git-commit-comment-file
    change-log-file
    helm-ff-file
    helm-buffer-file
    helm-etags-file
    helm-bookmark-file
    helm-grep-file
    speedbar-file-face
    diff-file-header
    file-name-shadow))

(defvar duotone-executables
  ;; (theme-faces-matching-regexps "exec")
  '(helm-ff-executable sh-quoted-exec eshell-ls-executable))


;; * Marks

(defvar duotone-marks
  ;; (theme-faces-matching-regexps '("mark" "marked" "book" "bm" "markdown" "1" "2"))
  '(helm-visible-mark visible-mark-active dired-mark))

(defvar duotone-marked-marks
  ;; (theme-faces-matching-regexps '("marked"))
  '(dired-marked))

(defvar duotone-bookmarks (theme-faces-matching-regexps '("bookmark" "bm")))


;; * Colors

(defvar duotone-reds
  (theme-faces-matching-regexps "-red"))

(defvar duotone-redish
  (theme-faces-matching-regexps "amaranth"))

(defvar duotone-pinks
  (theme-faces-matching-regexps "pink"))

(defvar duotone-blues
  (theme-faces-matching-regexps "-red"))

(defvar duotone-blueish
  (theme-faces-matching-regexps "teal"))

(provide 'duotone-faces)
;;; duotone-faces.el ends here
