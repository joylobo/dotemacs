(use-package yasnippet :init (yas-global-mode))
(use-package yasnippet-snippets :defer t)

(use-package dash :defer t)

(use-package company
  :init
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-minimum-prefix-length 1)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (global-company-mode t))

(use-package lsp-mode
  :hook (
	 (prog-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(use-package copilot
  :hook
  (prog-mode . copilot-mode)
  (copilot-mode . (lambda () (setq-local copilot--indent-warning-printed-p t)))
  :bind
  (:map copilot-completion-map
	("<tab>" . 'copilot-accept-completion)
	("TAB" . 'copilot-accept-completion)
	("C-TAB" . 'copilot-accept-completion-by-word)
	("C-<tab>" . 'copilot-accept-completion-by-word)))
(use-package copilot-chat)

(use-package rainbow-mode :config (rainbow-mode))

(provide 'init-devtools)
