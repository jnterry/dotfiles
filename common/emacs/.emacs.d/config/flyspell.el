;; Enables the flyspell mode in many languages

;; Enable spell check of comments and strings in following language modes
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                yaml-mode
                shell-mode-hook
                php-mode-hook
                html-mode-hook
								web-mode
								js-mode
								js2-mode
								javascript-mode
								css-mode
                haskel-mode-hook
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

;; Ensure that the personal dictionary is in emacs config dir so is
;; tracked by git
(setq ispell-personal-dictionary "~/.emacs.d/flyspell_personal_dictionary")

;; Prevent flyspell checking include file names
;; :TODO: don't think this is working...
(add-to-list 'ispell-skip-region-alist '("^#include" forward-line))

(eval-after-load "flyspell" '(diminish 'flyspell-mode))
