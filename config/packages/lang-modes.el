(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
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
		)
  :config
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-code-indent-offset      2)
  (setq web-mode-markup-indent-offset    4)
)
