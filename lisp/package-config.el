(use-package company
  :ensure t
  :init (company-mode 1))
(put 'set-goal-column 'disabled nil)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package which-key
  :ensure
  :config
  (which-key-mode))

;; emacs-vim commands *for a better performance*
(evil-mode 1)

(provide 'package-config)
