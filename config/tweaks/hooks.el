(add-hook 'emacs-startup-hook
		  (lambda () (
					  (global-visual-line-mode) ;; Always use visual-line-mode
					  )
			)
		  )


;; Delete whitespace at end of lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)
