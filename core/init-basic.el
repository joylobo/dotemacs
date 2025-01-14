(when (version< emacs-version "29.1")
 (message "Please upgrade to Emacs 29.1 or newer.")
 (sit-for 5)
 (kill-emacs))

(setq gc-cons-threshold (* 128 1024 1024))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode 0))
(set-face-attribute 'default nil :background "#1e1e1e")
(when (memq window-system '(mac ns))
  (set-frame-parameter nil 'ns-appearance 'dark)
  (set-frame-parameter nil 'ns-transparent-titlebar nil))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(
  ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Log the startup time.
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode t)
(setq-default highlight-tabs t)
(setq-default show-trailing-whitespace t)

;; Remember cursor position.
(save-place-mode 1)

;; Recently opened files.
(recentf-mode 1)
(use-package recentf-ext)
(setq recentf-max-menu-items 25)

;; Save backup files in a dedicated directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/.bak")))

(setq desktop-path '("~/.emacs.d/.bak"))
(setq desktop-save t)
(setq desktop-auto-save-timeout 300)
(desktop-save-mode 1)

;; Remove useless whitespace before saving a file.
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
  (helm-mode 1)
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

(use-package undo-tree :init(undo-tree-mode) :bind (("C-x u" . 'undo-tree-visualize))
  :config (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.undo"))))

(use-package multiple-cursors :defer t
  :bind (("M-s-<down>" . 'mc/mark-next-like-this)
	 ("M-s-<up>" . 'mc/mark-previous-like-this)
	 ("C-?" . 'mc/mark-all-like-this)))

(provide 'init-basic)
