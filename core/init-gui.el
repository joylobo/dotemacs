(pixel-scroll-precision-mode t)
(setq-default cursor-type 'bar)
(set-fringe-mode 0)
(tab-line-mode)
(setq tab-line-new-button-show nil)
(setq tab-line-separator "")

(use-package vscode-dark-plus-theme :config (load-theme 'vscode-dark-plus t))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(context-menu-mode)

;; Disable splash screen and startup message.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(use-package window-numbering :defer t :init (window-numbering-mode 1))
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
	    (lambda ()
	      (unless (file-remote-p default-directory)
		(auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package popwin :init
  (popwin-mode 1)
  (push '("*Warnings*" :position bottom :height 0.3) popwin:special-display-config)
  (push '("*helm find files*" :position bottom :height 0.3) popwin:special-display-config)
  (push '("*helm M-x*" :position bottom :height 0.3) popwin:special-display-config)
  (push '("*Org-Babel Error Output*" :position bottom :height 0.3) popwin:special-display-config))

(provide 'init-gui)
