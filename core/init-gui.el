(use-package vscode-dark-plus-theme :config (load-theme 'vscode-dark-plus t))

(pixel-scroll-precision-mode t)
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(setq-default cursor-type 'bar)
(set-fringe-mode 0)

(use-package all-the-icons)
(require 'awesome-tab)
(setq awesome-tab-dark-active-bar-color "#1e1e1e")
(setq awesome-tab-dark-selected-foreground-color "#ffffff")
(setq awesome-tab-dark-unselected-foreground-color "#969696")
(setq awesome-tab-height 120)
(awesome-tab-mode t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(context-menu-mode)

(use-package window-numbering :defer t :init (window-numbering-mode 1))
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

(use-package vscode-icon)
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :config
  (setq dired-sidebar-theme 'vscode)
  (setq vscode-icon-size 16))

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.3))))

(provide 'init-gui)
