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
(load-theme 'modus-operandi t)

;; PlemolJP <https://github.com/yuru7/PlemolJP>
;; HackGen <https://github.com/yuru7/HackGen>
(let ((font-face-list
       '("PlemolJP Console NF"
	 "HackGen Console NF"
	 "VictorMono Nerd Font")))
  (set-face-attribute 'default nil :font (nth 0 font-face-list) :height 140))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("df1ed4aa97d838117dbda6b2d84b70af924b0380486c380afb961ded8a41c386" "5efa59da0b446dd939749e86fdf414ef2b666f80243999633d9e2e4fd22fd37c" "943f5fc368a959c5858ff466401357da25b5991c0eeb396e865fd8071e8fe88e" "e8480a7c0fcd73c430111858550dfb91326df654ccbe038485ba87158320595d" "515ebca406da3e759f073bf2e4c8a88f8e8979ad0fdaba65ebde2edafc3f928c" "1ea82e39d89b526e2266786886d1f0d3a3fa36c87480fad59d8fab3b03ef576e" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" "42b221449475e41bc04c5663164ffc5d1672c53163630d41d57ef27d5a01cca9" "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1" "8a0eba8f06120605030161981032469d87a9832402e4441daa91296aa463f124" "3bfdc037f4e076a4afe8060a7e663a75eb2f3a5bedf1c9902a5d5645fba08d56" "972e40d60ad5703cb92b7ec1da9e2cec068d8732063b8b42874e0391624e50a1" "95e934b092694a2624adb653043d1dc016a6712fa27b788f9ff4dffb8ee08290" "84beb6ca7529b52c13124cafd04f9c3763b68a5a73bad89a1e6b596e6157af90" default))
 '(package-selected-packages '(julia-mode lua-mode ef-themes modus-themes evil)))
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
 '((python . t)
   (lua . t)
   (julia . t)
   (octave . t)))

;; little packages
(defconst HOME (getenv "HOME"))
(add-to-list 'load-path (concat HOME "/.emacs.d/packages"))

;; runtime configuration files
(add-to-list 'load-path (concat HOME "/.emacs.d/after"))
(progn
  (require 'modus-rc))

;; org mode customizations
(load (concat (getenv "HOME") "/.emacs.d/org-config.el"))
