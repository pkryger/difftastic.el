((nil . ((ispell-local-dictionary . "american")
         (sentence-end-double-space . t)
         (fill-column . 79)
         (eval . (when (and (boundp 'projectile-project-types)
                            (alist-get 'pk/emacs-package projectile-project-types))
                   (setq projectile-project-type 'pk/emacs-package)))))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (eval . (progn
                               (setq flycheck-emacs-lisp-load-path 'inherit))))))
