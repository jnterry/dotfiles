;; Set up of mmm-mode (multiple major mode in single buffer)

(use-package mmm-mode
	;; mmm-mode relies on this non-standard library, load before mmm-mode by putting in init
	:init (require 'cl)
	:config

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; org-mode config

	;; Add class for blocks of org-mode within the tags <<@org @>>
	(mmm-add-classes
	 '((org-blocks
			:submode org-mode
			:front "<<@org"
			:back  "@>>"
			)))

	;; Enable org-blocks in the following major modes
	(add-to-list 'mmm-mode-ext-classes-alist '(c++-mode          nil org-blocks))
	(add-to-list 'mmm-mode-ext-classes-alist '(c-mode            nil org-blocks))
	(add-to-list 'mmm-mode-ext-classes-alist '(js-mode           nil org-blocks))
	(add-to-list 'mmm-mode-ext-classes-alist '(typescript-mode   nil org-blocks))
	(add-to-list 'mmm-mode-ext-classes-alist '(css-mode          nil org-blocks))

	;; :TODO: org mode tries to collapse links with [[ ]] in entire buffer,
	;; but this syntax is used in if statements in shell scripts (which may be
	;; included in puppet exec statements...)
	;; (add-to-list 'mmm-mode-ext-classes-alist '(sh-mode           nil org-blocks))
	;; (add-to-list 'mmm-mode-ext-classes-alist '(puppet-mode       nil org-blocks))


	(add-hook 'c-mode-common-hook (lambda()()))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;; Always enable mmm-minor-mode if we are in a buffer which might be able to use it
	(setq mmm-global-mode 'maybe)

	;; Highlight the embedded major mode with a dark green background
	(set-face-attribute 'mmm-default-submode-face nil :background "#112f11")
	)
