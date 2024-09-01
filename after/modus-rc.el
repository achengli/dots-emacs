;; modus operandi and modus vivendi post include configuration
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-custom-auto-reload t
      modus-themes-diable-other-themes t

      ;; modus themes with org-mode
      modus-themes-org-blocks 'tinted-background ; {nil, 'gray-background 'tinted-background}
      modus-themes-headings '((1 . (variable-pitch 1.5))
			      (2 . (1.3))
			      (agenda-date . (1.3))
			      (agenda-structure . (variable-pitch light 1.8))
			      (t . (1.1))))

(message "MODUS-RC file was loaded")
(provide 'modus-rc)
