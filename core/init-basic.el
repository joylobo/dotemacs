(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode t)
(setq-default highlight-tabs t)
(setq-default show-trailing-whitespace t)

(setq find-file-visit-truename t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(setq system-time-locale "C")
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package helm
  :config
  (helm-mode)
  (setq helm-split-window-in-side-p t)
  (setq helm-display-header-line nil) ;; 解决当显示 tab-bar 时 helm 不刷新的问题
  (helm-autoresize-mode)
  (helm-ff-icon-mode)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-r" . helm-recentf)
	 ("C-x b" . helm-buffers-list)
	 ("C-x c o" . helm-occur)))

(use-package which-key :defer t :init (which-key-mode))

(use-package crux
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key (kbd "s-r") #'crux-recentf-find-file)
  (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line))

(make-directory "~/.emacs.d/.cache/undo-tree" t)
(use-package undo-tree :init(undo-tree-mode) :bind (("C-x u" . 'undo-tree-visualize))
  :config (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo-tree/"))))

(use-package move-text
  :bind (("M-<up>" . 'move-text-up)
	 ("M-<down>" . 'move-text-down)))

(use-package multiple-cursors :defer t
  :bind (("C->" . 'mc/mark-next-like-this)
	 ("C-<" . 'mc/mark-previous-like-this)
	 ("C-?" . 'mc/mark-all-like-this)))

(provide 'init-basic)
