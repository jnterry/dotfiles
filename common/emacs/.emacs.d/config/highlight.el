;; This file setups up highlighting for various keywords
;;
;; Originally taken from casey's config:
;; https://github.com/HandmadeHero/cpp/blob/master/misc/.emacs#L120

;; Define new faces
(make-face 'font-lock-my-todo-face)
(make-face 'font-lock-my-opt-face )
(make-face 'font-lock-my-comp-face)

(modify-face 'font-lock-my-todo-face "red"    nil nil t t nil nil nil)
(modify-face 'font-lock-my-opt-face  "yellow" nil nil t t nil nil nil)
(modify-face 'font-lock-my-comp-face "cyan"   nil nil t t nil nil nil)
(modify-face 'font-lock-my-comp-face "green"  nil nil t t nil nil nil)

(mapc
 ;; Function which adds the keywords to 'mode;
 (lambda (mode)
	 (font-lock-add-keywords
		mode
		'((":\\(TODO\\):" 1 'font-lock-my-todo-face t)
			(":\\(OPT\\):"  1 'font-lock-my-opt-face  t)
			(":\\(COMP\\):" 1 'font-lock-my-comp-face t)
			(":\\(REF\\):"  1 'font-lock-my-comp-face t)
			)))

 ;; List of modes to apply the function to
 '(c++-mode
	 c-mode
	 glsl-mode
	 js-mode
	 html-mode
	 emacs-lisp-mode
	 python-mode
	 haskell-mode
	 perl-mode
	 org-mode
	 puppet-mode
	 web-mode
	 dockerfile-mode
	 yaml-mode
	 markdown-mode
	 json-mode
	 less-css-mode
	 ruby-mode
	 verilog-mode
	 ))
