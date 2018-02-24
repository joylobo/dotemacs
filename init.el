;;; init.el --- An emacs config for javascript and golang.

;;; Commentary:

;;; Code:

;;
;; ██████╗   █████╗  ███████╗ ██╗  ██████╗
;; ██╔══██╗ ██╔══██╗ ██╔════╝ ██║ ██╔════╝
;; ██████╔╝ ███████║ ███████╗ ██║ ██║
;; ██╔══██╗ ██╔══██║ ╚════██║ ██║ ██║
;; ██████╔╝ ██║  ██║ ███████║ ██║ ╚██████╗
;; ╚═════╝  ╚═╝  ╚═╝ ╚══════╝ ╚═╝  ╚═════╝
;;
(setq user-full-name "Joy Lobo")
(setq user-mail-address "joylobo0528@gmail.com")
(setq gc-cons-threshold 100000000)

;; Packages.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; Install the use-package if needed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Use the desktop library to save the state of Emacs from one session to another.
(desktop-save-mode 1)

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one.
(show-paren-mode t)

;; Insert Parenthesis by Pair: electric-pair-mode
(electric-pair-mode 1)

;; Highlight tabulations.
(setq-default highlight-tabs t)

;; Show trailing white spaces.
(setq-default show-trailing-whitespace t)

;; Save backup files in a dedicated directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/.bak")))

;; Remove useless whitespace before saving a file.
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;; Set locale to UTF8.
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set the global keyboard shortcuts.
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

;; Log the startup time.
(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

;; helm
(use-package helm
  :ensure t
  :config
  (progn
    (helm-mode 1)
    (use-package helm-config
      :config
      (progn
	(global-set-key (kbd "M-x") 'helm-M-x)
	(global-set-key (kbd "C-x C-f") 'helm-find-files)
	(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)))))

;;
;; ██╗   ██╗ ██╗
;; ██║   ██║ ██║
;; ██║   ██║ ██║
;; ██║   ██║ ██║
;; ╚██████╔╝ ██║
;;  ╚═════╝  ╚═╝
;;
;; window.
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode 0))
(unless (display-graphic-p) (menu-bar-mode -1))
(global-linum-mode)
(setq tab-width 4)

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; window-numbering
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1))

;;disable splash screen and startup message.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; nyan mode.
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

;; dracula theme.
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; neotree.
(use-package neotree
  :ensure t
  :config
  (progn
    (global-set-key [f8] 'neotree-toggle)
    (use-package all-the-icons
      :ensure t
      :config
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode)))
(use-package flycheck
  :ensure t
  :config
  (progn
    ;; turn on flychecking globally
    (add-hook 'after-init-hook #'global-flycheck-mode)

    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers	'(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)

    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck")

    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers	'(json-jsonlist)))))

;;
;;      ██╗  █████╗  ██╗   ██╗  █████╗  ███████╗  ██████╗ ██████╗  ██╗ ██████╗  ████████╗
;;      ██║ ██╔══██╗ ██║   ██║ ██╔══██╗ ██╔════╝ ██╔════╝ ██╔══██╗ ██║ ██╔══██╗ ╚══██╔══╝
;;      ██║ ███████║ ██║   ██║ ███████║ ███████╗ ██║      ██████╔╝ ██║ ██████╔╝    ██║
;; ██   ██║ ██╔══██║ ╚██╗ ██╔╝ ██╔══██║ ╚════██║ ██║      ██╔══██╗ ██║ ██╔═══╝     ██║
;; ╚█████╔╝ ██║  ██║  ╚████╔╝  ██║  ██║ ███████║ ╚██████╗ ██║  ██║ ██║ ██║         ██║
;;  ╚════╝  ╚═╝  ╚═╝   ╚═══╝   ╚═╝  ╚═╝ ╚══════╝  ╚═════╝ ╚═╝  ╚═╝ ╚═╝ ╚═╝         ╚═╝
;; js2-mode.
(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)))

;; tern
(use-package tern
  :ensure t
  :config
  (progn
    (setq tern-command (append tern-command '("--no-port-file")))
    (use-package tern-auto-complete
      :ensure t
      :config
	    (tern-ac-setup))
    (add-hook 'js2-mode-hook (lambda ()
			   (tern-mode t)
			   (js2-mode-hide-warnings-and-errors)))))


;;
;;  ██████╗   ██████╗  ██╗       █████╗  ███╗   ██╗  ██████╗
;; ██╔════╝  ██╔═══██╗ ██║      ██╔══██╗ ████╗  ██║ ██╔════╝
;; ██║  ███╗ ██║   ██║ ██║      ███████║ ██╔██╗ ██║ ██║  ███╗
;; ██║   ██║ ██║   ██║ ██║      ██╔══██║ ██║╚██╗██║ ██║   ██║
;; ╚██████╔╝ ╚██████╔╝ ███████╗ ██║  ██║ ██║ ╚████║ ╚██████╔╝
;;  ╚═════╝   ╚═════╝  ╚══════╝ ╚═╝  ╚═╝ ╚═╝  ╚═══╝  ╚═════╝
;;
(use-package go-mode
  :ensure t)
(use-package go-autocomplete
  :ensure t
  :config
  (ac-config-default))

;;
;;  ██████╗ ██╗   ██╗ ███████╗ ████████╗  ██████╗  ███╗   ███╗
;; ██╔════╝ ██║   ██║ ██╔════╝ ╚══██╔══╝ ██╔═══██╗ ████╗ ████║
;; ██║      ██║   ██║ ███████╗    ██║    ██║   ██║ ██╔████╔██║
;; ██║      ██║   ██║ ╚════██║    ██║    ██║   ██║ ██║╚██╔╝██║
;; ╚██████╗ ╚██████╔╝ ███████║    ██║    ╚██████╔╝ ██║ ╚═╝ ██║
;;  ╚═════╝  ╚═════╝  ╚══════╝    ╚═╝     ╚═════╝  ╚═╝     ╚═╝
;;
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun goto-go-path()
  (interactive)
  (find-file (replace-regexp-in-string "\n" "" (shell-command-to-string "go env GOPATH")))
  (neotree))
(global-set-key (kbd "s-G") 'goto-go-path)

(defun goto-workspace-path()
  (interactive)
  (find-file "~/workspace")
  (neotree)
  (switch-to-buffer "workspace"))
(global-set-key (kbd "s-W") 'goto-workspace-path)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (neotree))
(global-set-key (kbd "s-K") 'kill-other-buffers)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit go-autocomplete go-mode tern js2-mode all-the-icons flycheck web-mode neotree dracula-theme nyan-mode window-numbering helm benchmark-init use-package exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
