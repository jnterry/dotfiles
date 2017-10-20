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

(use-package markdown-mode
  :mode "\\.md\\'"
)

(use-package js2-mode
  :mode "\\.js\\'"
  :init
   (setq js2-strict-trailing-comma-warning nil)
)

(use-package json-mode
  :mode "\\.json\\'"
)

(use-package web-mode
  :mode (("\\.html\\'"       . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.jinja\\'"      . web-mode)
         ("\\.php\\'"        . web-mode)
				 ("\\.dot\\'"        . web-mode)
				 ("\\.vue\\'"        . web-mode)
		)
  :config
  (setq web-mode-enable-css-colorization t)
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
