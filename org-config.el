;; org-mode init file

(setq org-latex-images-centered t)

(require 'org-colored-text)

;; Allow coloring text in org mode output when is exporting to latex
;; # e.g.
;; #+begin_src elisp
;; (setq org-latex-images-centered t)
;; #+end_src
;; [[color:pink][This is a pink link]]
(org-add-link-type
 "color"
 (lambda (path)
   "No follow action.")
 (lambda (color description backend)
   (cond
    ((eq backend 'latex)
     (format "{\\color{%s}%s}" color description))
    ((eq backend 'html)
     (let ((rgb (assoc color color-name-rgb-alist))
	   r g b)
       (if rgb
	   (progn
	     (setq r (* 255 (/ (nth 1 rgb) 65535.0))
		   g (* 255 (/ (nth 2 rgb) 65535.0))
		   b (* 255 (/ (nth 3 rgb) 65535.0)))
	     (format "<span style=\"color: rgb(%s,%s,%s)\">%s</span>"
		     (truncate r) (truncate g) (truncate b)
		     (or description color)))
	 (format "No Color RGB for %s" color)))))))

;; increases the font size of latex previews inside org files
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "·"))))))

(use-package org-bullets
  :ensure
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
