;;; init.el --- My emacs config for c/c++ javascript and golang.

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
(setq gc-cons-threshold (* 128 1024 1024))
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (version< emacs-version "29.1")
 (message "Please upgrade to Emacs 29.1 or newer.")
 (sit-for 5)
 (kill-emacs))

;; Packages.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(
  ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Install the use-package if needed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Log the startup time.
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one.
(show-paren-mode t)

;; Highlight tabulations.
(setq-default highlight-tabs t)

;; Show trailing white spaces.
(setq-default show-trailing-whitespace t)

;; Remember cursor position.
(save-place-mode 1)

;; Recently opened files.
(recentf-mode 1)
(use-package recentf-ext)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'helm-recentf)

;; Save backup files in a dedicated directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/.bak")))

;; Remove useless whitespace before saving a file.
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(setq system-time-locale "C")
;; Set locale to UTF8.
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Set the global keyboard shortcuts.
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

;; exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

;; helm
(use-package helm
  :config  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("C-x c o" . helm-occur)))

(use-package which-key
  :defer t
  :init (which-key-mode))

(use-package undo-tree
  :bind (("C-x u" . 'undo-tree-visualize)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("M-s-<down>" . 'mc/mark-next-like-this)
	 ("M-s-<up>" . 'mc/mark-previous-like-this)
	 ("C-?" . 'mc/mark-all-like-this)))

(use-package yasnippet
  :init (yas-global-mode))

(use-package yasnippet-snippets :defer t)

(use-package dash :defer t)


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
(global-display-line-numbers-mode t)
(setq tab-width 4)
(pixel-scroll-precision-mode t)

;; window-numbering
(use-package window-numbering
  :defer t
  :init
  (window-numbering-mode 1))

;; Disable splash screen and startup message.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; powerline theme
(use-package powerline
  :config (powerline-default-theme))

;; dracula theme.
(use-package material-theme
  :config
  (load-theme 'material t))

;; nyan-mode
(use-package nyan-mode
  :config (nyan-mode t))

;; neotree.
(use-package neotree
  :defer t
  :init
  (progn
    (global-set-key [f8] 'neotree-toggle)
    (use-package all-the-icons
      :config
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))))

;; company
(use-package company
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode t))

;; org-mode
(use-package ob-go :defer t :after org)
(use-package ob-browser :defer t :after org)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(with-eval-after-load 'org
  (require 'org-tempo)
  (setq org-startup-indented t)
  (org-babel-do-load-languages 'org-babel-load-languages '((browser . t) (C . t) (calc . t) (emacs-lisp . t) (plantuml . t) (go . t) (js . t) (shell . t)))
  (add-hook 'org-mode-hook #'visual-line-mode))

(setq org-plantuml-jar-path
  (concat (file-name-directory load-file-name) "plantuml.jar"))

;;
;;  ██████╗ ██╗   ██╗ ███████╗ ████████╗  ██████╗  ███╗   ███╗
;; ██╔════╝ ██║   ██║ ██╔════╝ ╚══██╔══╝ ██╔═══██╗ ████╗ ████║
;; ██║      ██║   ██║ ███████╗    ██║    ██║   ██║ ██╔████╔██║
;; ██║      ██║   ██║ ╚════██║    ██║    ██║   ██║ ██║╚██╔╝██║
;; ╚██████╗ ╚██████╔╝ ███████║    ██║    ╚██████╔╝ ██║ ╚═╝ ██║
;;  ╚═════╝  ╚═════╝  ╚══════╝    ╚═╝     ╚═════╝  ╚═╝     ╚═╝
;;
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "s-K") 'kill-other-buffers)

(when (memq window-system '(mac ns))
  (defun notify ()
    "Send notification"
    (interactive)
    (let ((title (read-string "Enter the title: "))
	  (message (read-string "Enter the message: ")))
      (send-notification title message)))

  (defun send-notification (title message)
    (shell-command (format "osascript -e 'display notification \"%s\" with title \"%s\"' sound name \"default\"" message title))))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets window-numbering which-key undo-tree recentf-ext powerline org-roam ob-go ob-browser nyan-mode neotree multiple-cursors material-theme helm exec-path-from-shell company benchmark-init all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
