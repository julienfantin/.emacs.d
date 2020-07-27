(require 'theme-helpers)


;;; Expansion

(assert (null (expand-theme-faces nil)))

(assert (equal (expand-theme-faces '((foo ((t (:foreground nil))))))
               '((foo ((t (:foreground nil)))))))

(assert (equal (expand-theme-faces
                '(((foo bar) ((t (:foreground nil))))))
               '((foo ((t (:foreground nil))))
                 (bar ((t (:foreground nil)))))))

(assert (equal (expand-theme-faces
                '((foo ((t (:foreground nil))))
                  (bar ((t (:foreground nil))))))
               '((foo ((t (:foreground nil))))
                 (bar ((t (:foreground nil)))))))

(assert (equal (expand-theme-faces
                '((foo ((t (:foreground nil))))
                  ((bar baz) ((t (:foreground nil))))))
               '((foo ((t (:foreground nil))))
                 (bar ((t (:foreground nil))))
                 (baz ((t (:foreground nil)))))))

(assert (equal (expand-theme-faces
                '(((foo fooz) ((t (:foreground nil))))
                  ((bar baz) ((t (:foreground nil))))))
               '((foo ((t (:foreground nil))))
                 (fooz ((t (:foreground nil))))
                 (bar ((t (:foreground nil))))
                 (baz ((t (:foreground nil)))))))

(assert
 (equal
  (map-properties-pairs
   #'(lambda (k v)
       (list k 1))
   '(fooz ((t (:foreground nil)))))
  '(fooz ((t (:foreground 1))))))
