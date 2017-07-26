(add-hook 'emacs-startup-hook
		  (lambda () (
					  (neotree)                 ;; Start with neotree open
					  (global-visual-line-mode) ;; Always use visual-line-mode
					  )
			)
		  )
