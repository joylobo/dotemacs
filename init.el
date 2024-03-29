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
(setq gc-cons-threshold 100000000)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Log the startup time.
(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight corresponding parentheses when cursor is on one.
(show-paren-mode t)

;; Insert Parenthesis by Pair: electric-pair-mode
; (electric-pair-mode 1)

;; Highlight tabulations.
(setq-default highlight-tabs t)

;; Show trailing white spaces.
(setq-default show-trailing-whitespace t)

;; Remember cursor position.
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; Recently opened files.
(recentf-mode 1)
(use-package recentf-ext :ensure t)
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
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

;; (use-package evil :ensure :init (evil-mode t))

;; helm
(use-package helm
  :ensure t
  :config  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("C-x c o" . helm-occur)))

(use-package which-key
  :ensure t
  :defer t
  :init (which-key-mode))

(use-package undo-tree
  :defer t
  :ensure t
  :bind (("C-x u" . 'undo-tree-visualize)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C->" . 'mc/mark-next-like-this)
	 ("C-<" . 'mc/mark-previous-like-this)
	 ("C-?" . 'mc/mark-all-like-this)))

(use-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode))

(use-package yasnippet-snippets :ensure t :defer t)

(use-package dash :ensure t)


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
  :defer t
  :init
  (window-numbering-mode 1))

;; Disable splash screen and startup message.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; powerline theme
(use-package powerline
  :ensure
  :config (powerline-default-theme))

;(use-package doom-modeline
;  :defer t
;  :ensure t
;  :hook (after-init . doom-modeline-mode))

;; dracula theme.
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

;; nyan-mode
(use-package nyan-mode
  :ensure t
  :config (nyan-mode t))

;; neotree.
(use-package neotree
  :ensure t
  :defer t
  :init
  (progn
    (global-set-key [f8] 'neotree-toggle)
    (use-package all-the-icons
      :ensure t
      :config
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))))

;; company
(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode t))

;; org-mode
(use-package ob-go :defer t :ensure t)
(use-package ob-browser :defer t :ensure t)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (org-babel-do-load-languages 'org-babel-load-languages '((browser . t) (C . t) (calc . t) (emacs-lisp . t) (plantuml . t) (go . t) (js . t) (shell . t)))
  (add-hook 'org-mode-hook #'visual-line-mode))

(setq org-plantuml-jar-path
  (concat (file-name-directory load-file-name) "plantuml.jar"))

(use-package org-roam
  :defer t
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes/"))
  :bind (("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n t" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :defer t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(setq org-roam-capture-templates
  '(("d" "default" plain "%?"
    :target (file+head "${slug}.org" "#+title: ${title}\n#+date: %<%Y-%m-%d %H:%M:%S>\n")
    :unnarrowed t)))

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

(defun goto-documents-path()
  (interactive)
  (find-file "~/Documents")
  (neotree)
  (switch-to-buffer "Documents"))
(global-set-key (kbd "s-D") 'goto-documents-path)

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
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(sml-mode zenburn-theme yasnippet-snippets writeroom-mode window-numbering which-key use-package undo-tree sml-modeline recentf-ext powerline org-roam ob-go ob-browser nyan-mode neotree multiple-cursors material-theme helm exec-path-from-shell evil dracula-theme doom-modeline company benchmark-init all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
