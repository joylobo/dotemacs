;;; init.el --- My emacs config for c/c++ javascript and golang.

;;; Commentary:

;;; Code:

(setq user-full-name "Joy Lobo")
(setq user-mail-address "joylobo0528@gmail.com")

(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(require 'init-basic)
(require 'init-gui)
(require 'init-devtools)
(require 'init-org)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(htmlize ob-mermaid crux all-the-icons-dired zenburn-theme yasnippet-snippets window-numbering which-key vscode-icon vscode-dark-plus-theme vscdark-theme undo-tree recentf-ext powerline popwin org-roam ob-redis ob-mongo ob-go ob-browser nyan-mode neotree multiple-cursors material-theme helm exec-path-from-shell doom-themes dired-sidebar copilot company benchmark-init all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
