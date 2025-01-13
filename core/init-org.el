(use-package ob-go :after org)
(use-package ob-browser :after org)
(use-package ob-mongo :after org)

(use-package ob-redis :after org)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(with-eval-after-load 'org
  (require 'org-tempo)
  (setq org-startup-indented t)
  (org-babel-do-load-languages 'org-babel-load-languages '((browser . t) (C . t) (calc . t) (emacs-lisp . t) (plantuml . t) (go . t) (js . t) (shell . t)))
  (add-hook 'org-mode-hook #'visual-line-mode))

(setq org-plantuml-jar-path
  (concat (file-name-directory load-file-name) "plantuml.jar"))

(provide 'init-org)
