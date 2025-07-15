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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1da45d87a83acb558e69b90015f0821679716be79ecb76d635aafdca8f6ebd4"
     "09d6eb1e30641126f684b056406c728a1d291c8c9295d9901bfe37a612789715"
     "0cb6dc28758f1173709003cc65b6ee8553297548c69e094385cc4c25f6357c5b"
     "622034e2b06b087d3e950652c93e465f3df6eab50bfdceddaa78077487e9bc24"
     "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983"
     "0664443859604a53d2257701f034459edf8eab3cc2be50c7d8ae36740fe35578"
     "159a29ab0ec5ba4e2811eddd9756aa779b23467723dcbdd223929fbf2dde8954"
     "7af2a6fcd1e743d165c58fd95d20b46c2d96d9873ab67fc9371bdc8fda463de7"
     "c6f78f6a0324b541082bfcc7e1672d1b760b147ec2a09a37dbd469235ced473b"
     "9f27d5afd6d78b40bf1290c10722818e0b90f141fc3758d3c2d284ccb565de15"
     "702d0136433ca65a7aaf7cc8366bd75e983fe02f6e572233230a528f25516f7e"
     "ca47f7b222eb380e3035fb594d52032acd89dae0a49eac3da722a5cd3f485e3b"
     "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9"
     "0c5d7ffa7cdf8b889342d2cd38f6ddd2cc716561048feca89b17fda56677d6b8"
     "ed1b7b4db911724b2767d4b6ad240f5f238a6c07e98fff8823debcfb2f7d820a"
     "694dbeb8f98dddfb603a2fe0c04101f3fe457ee49bf90a6a581271e7f9c580c8"
     "f42ea77c304f98f13ce4ed3b7b84d4a96c69e651757185ba26aa7059d3219051"
     "0d9028987183602b113c27e52e549b2eeb23fb29ebfc19ca0b6612fa57472066"
     "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2"
     "64045b3416d83e5eac0718e236b445b2b3af02ff5bcd228e9178088352344a92"
     "df1ed4aa97d838117dbda6b2d84b70af924b0380486c380afb961ded8a41c386"
     "5efa59da0b446dd939749e86fdf414ef2b666f80243999633d9e2e4fd22fd37c"
     "943f5fc368a959c5858ff466401357da25b5991c0eeb396e865fd8071e8fe88e"
     "e8480a7c0fcd73c430111858550dfb91326df654ccbe038485ba87158320595d"
     "515ebca406da3e759f073bf2e4c8a88f8e8979ad0fdaba65ebde2edafc3f928c"
     "1ea82e39d89b526e2266786886d1f0d3a3fa36c87480fad59d8fab3b03ef576e"
     "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     "42b221449475e41bc04c5663164ffc5d1672c53163630d41d57ef27d5a01cca9"
     "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1"
     "8a0eba8f06120605030161981032469d87a9832402e4441daa91296aa463f124"
     "3bfdc037f4e076a4afe8060a7e663a75eb2f3a5bedf1c9902a5d5645fba08d56"
     "972e40d60ad5703cb92b7ec1da9e2cec068d8732063b8b42874e0391624e50a1"
     "95e934b092694a2624adb653043d1dc016a6712fa27b788f9ff4dffb8ee08290"
     "84beb6ca7529b52c13124cafd04f9c3763b68a5a73bad89a1e6b596e6157af90"
     default))
 '(org-agenda-files '("~/Documents/notas/planificacion-verano.org"))
 '(package-selected-packages
   '(slime leuven-theme ac-slime slime-company smooth-scrolling gnuplot
	   company-maxima yasnippet maxima doom-modeline vala-mode ov
	   latex-extra vterm which-key lsp-mode company
	   timu-rouge-theme org-bullets julia-mode lua-mode ef-themes
	   modus-themes evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
