;; Emacs lisp initialization file
;; ---------------------------------------------
;; Author: Yassin Achengli - Copyright (cc-BY) 2024
;; Description: My public emacs configuration.
;; License: GNU GPLv3
;; ---------------------------------------------

;; minimal ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

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

;; package hosts
(require 'package)
(let ((package-archives-list '(("gnu" . "https://elpa.gnu.org/packages/")
			       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			       ("melpa" . "https://melpa.org/packages/"))))
  (dolist (p-archive package-archives-list)
    (add-to-list 'package-archives p-archive) t)
  (package-initialize))

;; ef-themes collection and modus-themes 
(unless (symbolp 'ef-bio)
  (package-install "ef-themes"))
(load-theme 'ef-bio t)

;; PlemolJP <https://github.com/yuru7/PlemolJP>
;; HackGen <https://github.com/yuru7/HackGen>
(let ((font-face-list
       '("PlemolJP Console NF"
	 "HackGen NF")))
  (set-face-attribute 'default nil :font (nth 0 font-face-list) :height 140))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("95e934b092694a2624adb653043d1dc016a6712fa27b788f9ff4dffb8ee08290" "84beb6ca7529b52c13124cafd04f9c3763b68a5a73bad89a1e6b596e6157af90" default))
 '(package-selected-packages '(ef-themes modus-themes evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; emacs-vim commands *for a better performance*
(evil-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; insert cons for each language you want to enable
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; org mode customizations
(load (concat (getenv "HOME") "/.emacs.d/org-config.el"))
