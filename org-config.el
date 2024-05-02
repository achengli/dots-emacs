(setq org-latex-images-centered t)

(require 'org-colored-text)

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
