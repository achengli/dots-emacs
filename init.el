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

(set-background-color "#F0FFF0")

;; PlemolJP <https://github.com/yuru7/PlemolJP>
;; HackGen <https://github.com/yuru7/HackGen>
(let ((font-face-list
       '("PlemolJP Console NF"
	 "HackGen Console NF"
	 "VictorMono Nerd Font"
	 "BlexMono Nerd Font"
	 "Hack Nerd Font")))
  (set-face-attribute 'default nil :font (nth 1 font-face-list) :height 140))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0cb6dc28758f1173709003cc65b6ee8553297548c69e094385cc4c25f6357c5b" "622034e2b06b087d3e950652c93e465f3df6eab50bfdceddaa78077487e9bc24" "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983" "0664443859604a53d2257701f034459edf8eab3cc2be50c7d8ae36740fe35578" "159a29ab0ec5ba4e2811eddd9756aa779b23467723dcbdd223929fbf2dde8954" "7af2a6fcd1e743d165c58fd95d20b46c2d96d9873ab67fc9371bdc8fda463de7" "c6f78f6a0324b541082bfcc7e1672d1b760b147ec2a09a37dbd469235ced473b" "9f27d5afd6d78b40bf1290c10722818e0b90f141fc3758d3c2d284ccb565de15" "702d0136433ca65a7aaf7cc8366bd75e983fe02f6e572233230a528f25516f7e" "ca47f7b222eb380e3035fb594d52032acd89dae0a49eac3da722a5cd3f485e3b" "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" "0c5d7ffa7cdf8b889342d2cd38f6ddd2cc716561048feca89b17fda56677d6b8" "ed1b7b4db911724b2767d4b6ad240f5f238a6c07e98fff8823debcfb2f7d820a" "694dbeb8f98dddfb603a2fe0c04101f3fe457ee49bf90a6a581271e7f9c580c8" "f42ea77c304f98f13ce4ed3b7b84d4a96c69e651757185ba26aa7059d3219051" "0d9028987183602b113c27e52e549b2eeb23fb29ebfc19ca0b6612fa57472066" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "64045b3416d83e5eac0718e236b445b2b3af02ff5bcd228e9178088352344a92" "df1ed4aa97d838117dbda6b2d84b70af924b0380486c380afb961ded8a41c386" "5efa59da0b446dd939749e86fdf414ef2b666f80243999633d9e2e4fd22fd37c" "943f5fc368a959c5858ff466401357da25b5991c0eeb396e865fd8071e8fe88e" "e8480a7c0fcd73c430111858550dfb91326df654ccbe038485ba87158320595d" "515ebca406da3e759f073bf2e4c8a88f8e8979ad0fdaba65ebde2edafc3f928c" "1ea82e39d89b526e2266786886d1f0d3a3fa36c87480fad59d8fab3b03ef576e" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" "42b221449475e41bc04c5663164ffc5d1672c53163630d41d57ef27d5a01cca9" "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1" "8a0eba8f06120605030161981032469d87a9832402e4441daa91296aa463f124" "3bfdc037f4e076a4afe8060a7e663a75eb2f3a5bedf1c9902a5d5645fba08d56" "972e40d60ad5703cb92b7ec1da9e2cec068d8732063b8b42874e0391624e50a1" "95e934b092694a2624adb653043d1dc016a6712fa27b788f9ff4dffb8ee08290" "84beb6ca7529b52c13124cafd04f9c3763b68a5a73bad89a1e6b596e6157af90" default))
 '(org-agenda-files '("~/Documents/notas/planificacion-verano.org"))
 '(package-selected-packages
   '(ov latex-extra vterm which-key lsp-mode company timu-rouge-theme org-bullets julia-mode lua-mode ef-themes modus-themes evil)))
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
