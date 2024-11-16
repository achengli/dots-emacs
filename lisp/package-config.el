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

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(use-package which-key
  :ensure
  :config
  (which-key-mode))

;; vim shortcuts
(evil-mode 1)

<<<<<<< HEAD
;; doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
;; doom modeline customization
(setq doom-modeline-support-imenu t
      doom-modeline-height 25
      doom-modeline-bar-width 4
      doom-modeline-window-width-limit nil)

;; yasnippet
(add-to-list 'load-path
             "~/path-to-yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag nil)
(add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode))
(setq imaxima-fnt-size "large")
(setq imaxima-pt-size 12)
=======
;; doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
;; doom modeline custom
(setq doom-modeline-support-imenu t
      doom-modeline-height 25
      doom-modeline-bar-width 4
      doom-modeline-window-limit nil)

>>>>>>> b196045 (Sat Nov 16 06:48:03 PM CET 2024)

(provide 'package-config)
