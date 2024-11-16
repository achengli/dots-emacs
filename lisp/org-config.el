;; org-mode init file

(setq org-latex-images-centered t)

(defun org-insert-named-opt (name val)
  "Insert header named option in org mode buffer."
  (interactive "sOption name: \n
sValue: ")
    (with-current-buffer (current-buffer)
      (goto-char (point))
      (insert (concat "#+" name ": " val))))

(define-key org-mode-map (kbd "C-u n") #'org-insert-named-opt)

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "·"))))))

(use-package org-bullets
  :ensure
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; insert cons for each language you want to enable
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (lua . t)
   (julia . t)
   (octave . t)
   (maxima . t)))

(use-package org
  :hook (org-mode . (lambda () (org-indent-mode -1))))
(defun org-insert-named-opt (name val)
  "Put header pair name-value in current position.
#+NAME: value
"
  (interactive "sOption name: \nsValue: ")
	       (with-current-buffer (current-buffer)
		 (goto-char (point))
		 (insert (concat "#+" (upcase name) ": " val))))

(define-key org-mode-map (kbd "C-u n") #'org-insert-named-opt)

(provide 'org-config)
