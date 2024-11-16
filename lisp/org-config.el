;; org-mode init file

(setq org-latex-images-centered t)

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "·"))))))

(defvar org-bullets-active-semaphore t "Activate or deactivate org-bullets")

(when org-bullets-active-semaphore
(use-package org-bullets
  :ensure
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

;; insert cons for each language you want to enable
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (lua . t)
   (julia . t)
   (octave . t)
   (maxima . t)))

(defun org-insert-named-label (name content)
  "Define named label in org mode syntax."
  (interactive
   "sLabel name:\nsContent:")
  (with-current-buffer (current-buffer)
    (insert (string-make-multibyte (concat "#+" name ": "
		    (substring content 0 (- (length content) 0))))))) ;;; Put the content forward the cursor position.
(define-key org-mode-map (kbd "C-u n") 'org-insert-named-label)

(use-package org
  :hook (org-mode . (lambda () (org-indent-mode -1))))

(provide 'org-config)
