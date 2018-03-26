;; Sets up the rainbow-delimiters package which highlights matching brackets
;; in the same color, varying the color used based on nesting depth

(use-package rainbow-delimiters
	:init
	;; use in most programming language major modes
	(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
)
