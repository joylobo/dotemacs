(use-package yasnippet
  :config
  (setq yas-use-menu nil)
  (yas-global-mode))
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
(use-package company-box :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :hook (
	 (js-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-node-setup)
  :hook (
	 (dap-mode . (lambda () (set-fringe-style (quote (12 . 8))))))
  :bind
  ("<f5>" . 'dap-debug-last)
  ("<f6>" . 'dap-continue)
  ("<f7>" . 'dap-next)
  ("<f8>" . 'dap-step-in)
  ("<f9>" . 'dap-breakpoint-toggle)
  ("S-<f9>" . 'dap-breakpoint-delete-all))

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

(use-package ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  (setopt ellama-language "Chinese")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama :chat-model "qwen2.5" :embedding-model "qwen2.5"))
  :config (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))

(provide 'init-devtools)
