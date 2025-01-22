(use-package vscode-dark-plus-theme :config (load-theme 'vscode-dark-plus t))

(pixel-scroll-precision-mode t)
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(setq-default cursor-type 'bar)
(set-fringe-mode 0)
(setq-default line-spacing 0.2) ; 行间距

(use-package all-the-icons)
(use-package centaur-tabs :demand
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
     )))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(context-menu-mode)

(use-package window-numbering :defer t :init (window-numbering-mode 1))
(global-set-key (kbd "s-w") 'kill-this-buffer)

(require 'ls-lisp)
(setq ls-lisp-dirs-first t) ; 优先显示文件夹
(setq ls-lisp-ignore-case t) ; 忽略大小写
(setq ls-lisp-use-string-collate nil) ; 忽略大小写(非 windows)
(setq ls-lisp-use-insert-directory-program nil)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
	      (seq bol "." eol)
	      (seq bol ".." eol)
	      (seq bol ".git" eol)
	      )))
(use-package vscode-icon)
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :config
  (setq dired-sidebar-theme 'vscode)
  (setq vscode-icon-size 16))

(use-package popwin :config (popwin-mode))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (dired-sidebar-toggle-sidebar))
(global-set-key (kbd "C-x C-b") 'kill-other-buffers)

(provide 'init-gui)
