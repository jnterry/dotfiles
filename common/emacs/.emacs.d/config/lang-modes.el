;; Setups various major modes for editing different languages

;; (add-hook 'c++-mode-hook (lambda()(c-set-offset 'innamespace 0)))
(add-hook 'c++-mode-hook (lambda()(c-set-offset 'label 'tab-width)))

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

(use-package go-mode
  :mode (("\\.go\\'"  . go-mode)
				)
)

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
				 ("\\.yml\\'"  . yaml-mode)
				)
)

;; markdown-mode relies on emacs version 24.4 or newer
(when (or (> emacs-major-version 24)
					(and (eq emacs-major-version 24) (>= emacs-minor-version 4))
					)
	(use-package markdown-mode
		:mode "\\.md\\'"
	)
)

(use-package json-mode
  :mode "\\.json\\'"
)

(use-package web-mode
	:mode (("\\.html\\.erb\\'" . web-mode)
				 ("\\.mustache\\'"   . web-mode)
				 ("\\.jinja\\'"      . web-mode)
				 ("\\.php\\'"        . web-mode)
				 ("\\.vue\\'"        . web-mode)
				 ("\\.html\\'"       . web-mode)
				 ("\\.dot\\'"        . web-mode)
				 ("\\.js\\'"         . web-mode)
				 ("\\.html\\'"       . web-mode)
				 )
	:config
	(setq web-mode-enable-css-colorization t)
	(setq web-mode-markup-indent-offset    2)
	(setq web-mode-css-indent-offset       2)
	(setq web-mode-code-indent-offset      2)
)

(use-package typescript-mode
 	:mode (("\\.ts\\'" . typescript-mode)
				 )
	:config
	(setq typescript-indent-level 2)
)

(use-package less-css-mode
	:mode "\\.less\\'"
)

(use-package ruby-mode
	:mode (("\\.rb\\'"      . ruby-mode)
				 ("\\.vagrant\\'" . ruby-mode)
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
