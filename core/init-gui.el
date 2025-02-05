(use-package all-the-icons)

(setq-default cursor-type 'bar)
(pixel-scroll-precision-mode t)
(set-fringe-mode 0)
(setq-default line-spacing 0.2) ;; 行间距
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(if (functionp 'scroll-bar-mode) (scroll-bar-mode 0))
(global-set-key [mouse-3] (lambda () (interactive))) ;; 点击鼠标右键时不会选中文字
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(use-package window-numbering :defer t :init (window-numbering-mode 1))

(use-package centaur-tabs
  :config
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-type 'all-the-icons)
  (global-set-key (kbd "s-1") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-2") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-3") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-4") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-5") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-6") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-7") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-8") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-9") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-0") 'centaur-tabs-select-visible-tab)
  (centaur-tabs-mode t))

(defun centaur-tabs-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     (string-prefix-p "*" name)
     (string-prefix-p " *" name)
     (string-prefix-p ":" name)
     (string-prefix-p "ellama" name)
     )))

(use-package treemacs
  :config
  (setq treemacs-sorting 'treemacs--sort-alphabetic-asc)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package projectile :config (projectile-mode t))
(use-package treemacs-projectile
  :after (treemacs projectile)
  :config
  (treemacs-project-follow-mode t))

(use-package delight
  :config
  (delight 'treemacs-mode "EXPLORER"))

(use-package popwin :config (popwin-mode))
(setq popwin:special-display-config
      '(
	("*Help*" :dedicated t :position bottom :stick t :height 0.3)
	("*scratch*" :dedicated t :position bottom :stick t :height 0.3)
	("*Compilation*" :dedicated t :position bottom :stick t :height 0.3)
	("*eshell*" :dedicated t :position bottom :stick t :height 0.3)
	("*helm find files*" :dedicated t :position bottom :stick t :height 0.3)
	("*helm M-x*" :dedicated t :position bottom :stick t :height 0.3)
	("ellama.*" :regexp t :dedicated t :position bottom :stick t :height 0.3)
	("*grep*" :dedicated t :position bottom :stick t :height 0.3)))

(recentf-mode t)
(use-package recentf-ext)
(setq recentf-max-menu-items 25)
(setq recentf-save-file "~/.emacs.d/.cache/recentf")

(make-directory "~/.emacs.d/.cache/backup-directory" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/.cache/backup-directory/")))

(make-directory "~/.emacs.d/.cache/auto-save-list" t)
(setq auto-save-list-file-prefix "~/.emacs.d/.cache/auto-save-list/.saves-")

(save-place-mode 1)
(setopt save-place-file "~/.emacs.d/.cache/places")

(make-directory "~/.emacs.d/.cache/desktop-save" t)
(setq desktop-path '("~/.emacs.d/.cache/desktop-save/"))
(desktop-save-mode)
(add-hook 'desktop-after-read-hook
	  (lambda ()
	    (treemacs)))

(global-set-key (kbd "s-w") 'kill-this-buffer)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (treemacs))
(global-set-key (kbd "C-x C-b") 'kill-other-buffers)

(provide 'init-gui)
