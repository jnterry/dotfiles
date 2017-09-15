;; Enable spell check of comments in following language modes
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                yaml-mode
                shell-mode-hook
                php-mode-hook
                html-mode-hook
                haskel-mode-hook
                js-mode-hook
				js2-mode-hook
				c-mode-hook
				c++-mode-hook
			   )
		 )
  (add-hook hook 'flyspell-prog-mode)
)

;; Enable spell check of everything in following language modes
(dolist (hook '(org-mode-hook
				markdown-mode-hook
			   )
		 )
  (add-hook hook 'flyspell-mode)
  )

(setq ispell-personal-dictionary "~/.emacs.d/flyspell_personal_dictionary")
