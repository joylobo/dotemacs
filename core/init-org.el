(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-confirm-babel-evaluate nil)

(use-package htmlize :after org)
(use-package ob-go :after org)
(use-package ob-browser :after org)
(use-package ob-mongo :after org)
(use-package ob-redis :after org)

(with-eval-after-load 'org
  (require 'org-tempo)
  (setq org-startup-indented t)
  (org-babel-do-load-languages 'org-babel-load-languages '((browser . t) (C . t) (calc . t) (emacs-lisp . t) (sqlite . t) (plantuml . t) (go . t) (js . t) (shell . t)))
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package ob-mermaid :after org)
(setq ob-mermaid-cli-path "/Users/joylobo/.nvm/versions/node/v22.11.0/bin/mmdc")
(setq org-plantuml-jar-path (concat (file-name-directory load-file-name) "plantuml.jar"))

(provide 'init-org)
