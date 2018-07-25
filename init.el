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
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; Install the use-package if needed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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

(use-package magit
  :ensure t
  :config (global-set-key (kbd "C-x g") 'magit-status))


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

;; Disable splash screen and startup message.
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

(use-package flycheck
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode) ;; Turn on flychecking globally.
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers	'(javascript-jshint))) ;; Disable jshint since we prefer eslint checking.
    (flycheck-add-mode 'javascript-eslint 'web-mode) ;; Use eslint with web-mode for jsx files.
    (setq-default flycheck-temp-prefix ".flycheck") ;; Customize flycheck temp file prefix.
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers	'(json-jsonlist))))) ;; Disable json-jsonlist checking for json files.

;; gtd
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/gtd/inbox.org"
			 "~/gtd/gtd.org"
			 "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
			   ("~/gtd/someday.org" :level . 1)
			   ("~/gtd/tickler.org" :maxlevel . 2)))
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode t))

(use-package smart-compile :ensure t)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
	(let* ((w (split-window-vertically)))
	  (select-window w)
	  (set-window-buffer w "*scratch*")
	  (switch-to-buffer "*compilation*")
	  (shrink-window 10))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)
(global-set-key [f9] 'smart-compile)


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
    (use-package company-tern
      :ensure t
      :config
      (add-to-list 'company-backends 'company-tern)
      )

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
(use-package go-mode :ensure t)
(use-package company-go :ensure t)
(add-hook 'go-mode-hook (lambda ()
			  (add-hook 'go-mode-hook
				    (set (make-local-variable 'compile-command)
					 (format "go run %s" (file-name-nondirectory buffer-file-name)))
				    (set (make-local-variable 'company-backends) '(company-go))
				    (company-mode))))


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
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "s-K") 'kill-other-buffers)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (htmlize apib-mode emmet-mode recentf-ext window-numbering web-mode use-package tern-auto-complete nyan-mode neotree js2-mode irony-eldoc helm go-mode go-autocomplete flycheck exec-path-from-shell dracula-theme company-irony benchmark-init all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
