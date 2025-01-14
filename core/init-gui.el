(use-package vscode-dark-plus-theme :config (load-theme 'vscode-dark-plus t))

(pixel-scroll-precision-mode t)
(setq-default cursor-type 'bar)
(set-fringe-mode 0)

(custom-set-faces
 '(tab-line ((t (:background "#1e1e1e" :foreground "#d4d4d4" :box nil))))
 '(tab-line-tab ((t (:background "#2d2d2d" :foreground "#cccccc" :box (:line-width 1 :color "#2d2d2d")))))
 '(tab-line-tab-current ((t (:background "#007acc" :foreground "#ffffff" :box (:line-width 4 :color "#007acc")))))
 '(tab-line-tab-inactive ((t (:background "#1e1e1e" :foreground "#a6a6a6" :box (:line-width 4 :color "#1e1e1e"))))))
(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)
(setq tab-line-separator " ")
(add-hook 'prog-mode-hook #'tab-line-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(context-menu-mode)

(use-package window-numbering :defer t :init (window-numbering-mode 1))
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :config
  (setq dired-sidebar-theme 'vscode)
  (setq vscode-icon-size 16))

(use-package popwin :init
  (popwin-mode 1)
  (push '("*Warnings*" :position bottom :height 0.3) popwin:special-display-config)
  (push '("*helm find files*" :position bottom :height 0.3) popwin:special-display-config)
  (push '("*helm M-x*" :position bottom :height 0.3) popwin:special-display-config)
  (push '("*Org-Babel Error Output*" :position bottom :height 0.3) popwin:special-display-config))

(provide 'init-gui)
