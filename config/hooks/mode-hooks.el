;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling Checks                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix indentation of enum class in c++                                   ;;
;; taken from: http://stackoverflow.com/questions/6497374/                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '*))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)
