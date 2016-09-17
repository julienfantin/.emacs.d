;;; customize-theme.el

(require 'custom)
(require 'theme-helpers)

(defcustom customize-theme-alist '()
  "Alist of theme to custom faces"
  :group 'theming
  :type 'alist)

;;;###autoload
(defun customize-theme ()
  (dolist (theme custom-enabled-themes)
    (when-let (faces (alist-get theme customize-theme-alist))
      (theme-custom-set-faces theme faces))))

;;;###autoload
(advice-add #'load-theme :after #'customize-theme)

(provide 'customize-theme)
