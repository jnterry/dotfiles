;;; lang-modes --- Setups various major modes for editing different languages
;;;
;;; Code:


;; A number of major-modes depend on emacs version 24.4 or newer
;; Work out if we are on this version and set a variable to use in rest of file
(defvar using-modern-emacs (or (> emacs-major-version 24)
															 (and (eq emacs-major-version 24) (>= emacs-minor-version 4))
															 ))

(print (concat "*** lang-modes using-modern-emacs: "
							 (if using-modern-emacs "true" "false")))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
)

(use-package puppet-mode
  :mode "\\.pp\\'"
  :config
  (setq puppet-indent-level     2)
  (setq puppet-indent-tabs-mode t)

)

(use-package haskell-mode
  :mode (("\\.hs\\'"  . haskell-mode)
		 ("\\.lhs\\'" . haskell-mode)
		)
)

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
		 ("\\.yml\\'"  . yaml-mode)
		)
)

(when using-modern-emacs
	(use-package markdown-mode
		:mode "\\.md\\'"
		))

(use-package js2-mode
  :mode "\\.js\\'"
  :init
   (setq js2-strict-trailing-comma-warning nil)
)

(use-package json-mode
  :mode "\\.json\\'"
)

(when using-modern-emacs
	(use-package web-mode
		:mode (("\\.html\\.erb\\'" . web-mode)
					 ("\\.mustache\\'"   . web-mode)
					 ("\\.jinja\\'"      . web-mode)
					 ("\\.php\\'"        . web-mode)
					 ("\\.vue\\'"        . web-mode)
					 ("\\.html\\'"       . web-mode)
					 ("\\.dot\\'"        . web-mode)
					 )
		:config
		(setq web-mode-enable-css-colorization t)
		(setq web-mode-markup-indent-offset    2)
		(setq web-mode-css-indent-offset       2)
		(setq web-mode-code-indent-offset      2)
		))

(when using-modern-emacs
	(use-package less-css-mode
		:mode "\\.less\\'"
		))

(use-package ruby-mode
	:mode (("\\.rb\\'"      . ruby-mode)
				 ("\\.vagrant\\'" . ruby-mode)
				)
)

(use-package markdown-mode
	:mode (("\\.md\\'"      . markdown-mode)
				)
)

(use-package verilog-mode
       :mode (("\\.v\\'" . verilog-mode))
       :config
       (clear-abbrev-table verilog-mode-abbrev-table)
)

(use-package glsl-mode
       :mode (("\\.glsl\\'" . glsl-mode)
							("\\.vert\\'" . glsl-mode)
							("\\.frag\\'" . glsl-mode)
				)
)

;; (load "specman-mode")
;; (add-to-list 'auto-mode-alist '("\\.e\\'" . specman-mode   ))
;; (add-to-list 'auto-mode-alist '("\\.ecom\\'" . specman-mode))

;;; lang-modes.el ends here
