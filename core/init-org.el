(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-confirm-babel-evaluate nil)
(setq org-html-validation-link nil) ;; 导出 html 时不显示验证链接

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
(require 's)
(setq ob-mermaid-cli-path (s-replace "\n" "" (shell-command-to-string "which mmdc")))

(make-directory "~/org-roam" t)
(use-package org-roam :after org
  :config
  (setq org-roam-directory "~/org-roam")
  (setq org-roam-db-location "~/org-roam/org-roam.db")
  (org-roam-db-autosync-mode)
  :bind
   ("C-c n c" . org-roam-capture)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n g" . org-roam-graph))

(use-package org-roam-ui :after org
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start t))

(provide 'init-org)
