;; Emacs lisp initialization file
;; ---------------------------------------------
;; Author: Yassin Achengli - Copyright (cc-BY-NC) 2023-2025 
;; Description: Personal emacs config.
;; License: GPL version 3

;; Reduce UI elements (menubar, window functions...)
(progn
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1))

;; Set window opening dimensions.
(setq default-frame-alist
      '((height . 40)
	(width . 160)
	(left . 100)
	(top . 300)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(tool-bar-lines . 0)))

;; COLORSCHEME configuration and setup.
;; The colorscheme is changed using a function defined by a global
;; parameter called `*palette*'. This variable is set with symbol
;; `light' or `dark' for light and dark theme variant.
;;
;; `set-theme' function can be combined with `:liquid' keyword in
;; order to change the background color.

(defvar *palette* 'light "Color palette for emacs.")

(defun set-light-theme ()
  (interactive)
  (setq *palette* 'light)
  (set-theme))


(defun set-dark-theme ()
  (interactive)
  (setq *palette* 'dark)
  (set-theme))

(defun set-theme (&rest args)
  (if (equal *palette* 'light)
      (progn (load-theme 'leuven t)
	     (when (plist-get args :liquid) (set-background-color "#f0fff0")))
    (progn (load-theme 'modus-vivendi-tritanopia t)
	   (when (plist-get args :liquid) (set-background-color "#131313")))))

(set-theme)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; save minibuffer history
(savehist-mode 1)

;; setup a backup directory
(defcustom temoprary-file-directory
  (concat (getenv "HOME") "/.local/tmp") t)

(progn
  (unless (file-directory-p temporary-file-directory)
    (shell-command (concat "mkdir -p " temporary-file-directory)))
  (setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory))))


;; package repositories
(require 'package)
(let ((package-archives-list '(("gnu" . "https://elpa.gnu.org/packages/")
			       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			       ("melpa" . "https://melpa.org/packages/"))))
  (dolist (p-archive package-archives-list)
    (add-to-list 'package-archives p-archive) t)
  (package-initialize))

(defalias 'yes-or-no-p 'y-or-n-p)

;; little packages
(let ((HOME (getenv "HOME")))
  (add-to-list 'load-path (concat HOME "/.emacs.d/packages/"))
  (add-to-list 'load-path (concat HOME "/.emacs.d/lisp/")))

(require 'org)
(progn
  (load "package-config.el")
  (require 'package-config)
  (load "org-config.el")
  (require 'org-config))

;; ef-themes collection and modus-themes 
(unless (symbolp 'ef-bio)
  (package-install "ef-themes"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Font settings. Change the font style and font size with
;; `*font-style*' and `*font-size*'.
(let ((*font-style* 'hack)
      (*font-size* 11))
  (if (and (equal *font-style* 'hack) (find-font (font-spec :name "Hack Nerd Font")))
      (set-frame-font (format "Hack Nerd font %d" *font-size*) nil t)
    (if (and (equal *font-style* 'geist)
	     (find-font (font-spec :name "GeistMono Nerd Font")))
	(set-frame-font (format "GeistMono Nerd Font %d" *font-size*) nil t))
    (set-frame-font "Monospace 11" nil t)))

(setq inferior-lisp-program "sbcl")
