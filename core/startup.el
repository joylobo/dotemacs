;; (set-face-attribute 'default nil :background "#191919" :foreground "#dcdccc")
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

(when (memq window-system '(mac ns))
  (set-frame-parameter nil 'ns-appearance 'dark)
  (set-frame-parameter nil 'ns-transparent-titlebar nil))

(setq gc-cons-threshold (* 128 1024 1024))

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

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package vscode-dark-plus-theme :config (load-theme 'vscode-dark-plus t))
(use-package doom-modeline :config
  (setq doom-modeline-bar-width 0)
  (setq doom-modeline-buffer-name nil)
  (doom-modeline-mode))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq custom-file "~/.emacs.d/core/custom.el")
(load custom-file)

(provide 'startup)
