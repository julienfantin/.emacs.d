
;;; customize-theme.el
;;; Code:

(require 'custom)
(require 'theme-helpers)

(defcustom customize-theme-alist '()
  "Alist of theme to custom faces."
  :group 'customize-themes
  :type 'alist)

;;;###autoload
(defun customize-theme (theme &optional _ _)
  "`load-theme' advice for `THEME'."
  (when-let ((faces (alist-get theme customize-theme-alist)))
    (theme-custom-set-faces theme faces)))

;;;###autoload
(advice-add #'load-theme :after #'customize-theme)

(provide 'customize-theme)
