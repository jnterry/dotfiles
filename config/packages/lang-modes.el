(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
)

(use-package puppet-mode
  :mode "\\.pp\\'"
  :config
  (setq puppet-indent-level     4)
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
  (setq web-mode-code-indent-offset      2)
  (setq web-mode-css-indent-offset       2)
  (setq web-mode-markup-indent-offset    2)
  (setq tab-width                        2)
  (setq-default tab-width                2)
  :init
  (add-hook 'after-init-hook
						(lambda ()
							; (web-mode-use-tabs)
							(set-variable tab-width 2)
						)
	)
)
