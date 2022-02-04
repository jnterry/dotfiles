;; Setups various major modes for editing different languages

; disable lock files so react dev server doesn't crash...
(setq create-lockfiles nil)

;;(add-hook 'c++-mode-hook (lambda()(c-set-offset 'innamespace 0)))
(add-hook 'c++-mode-hook (lambda()(c-set-offset 'label 'tab-width)))

;; Change function style from default:
;;
;; function_call(
;;               test_param
;;              );
;;
;; to
;;
;; function_call(
;;   test_param
;; );
;;
;; Note that if params are put on same line as opening ( they will be kept in
;; correct alignment on subsequent lines
(add-hook 'c++-mode-hook (lambda()(c-set-offset 'arglist-intro 'c-basic-offset)))
(add-hook 'c++-mode-hook (lambda()(c-set-offset 'arglist-close 0)))
(add-hook 'glsl-mode-hook (lambda()(c-set-offset 'arglist-intro 'c-basic-offset)))
(add-hook 'glsl-mode-hook (lambda()(c-set-offset 'arglist-close 0)))


(setq cperl-indent-level 4
			cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

;; Override certain modes to use better versions
(fset 'perl-mode 'cperl-mode)

;; Set python indent level (8 by default...)
(add-hook 'python-mode-hook (lambda()(setq python-indent 4)))

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
				 ("\\.vue\\'"        . web-mode)
				 ("\\.html\\'"       . web-mode)
				 ("\\.dot\\'"        . web-mode)
				 ("\\.html\\'"       . web-mode)
				 )
	:config
	(setq web-mode-enable-css-colorization t)
	(setq web-mode-markup-indent-offset    2)
	(setq web-mode-css-indent-offset       2)
	(setq web-mode-code-indent-offset      2)
	(setq web-mode-script-padding          2)
)

; :TODO: these indentation settings make web-mode indent with 2 tabs
; for above lanages, as it tries to get offset of 4 with 2-width tabs
;(use-package web-mode
;  ; .tag files are used by the "Riot" UI framework in cockpit CMS
;  ; That repo uses 4 space indentation, and since we don't use .tag files anywhere else,
;	; lets setup the defaults correctly...
;	:mode (("\\.tag\\'"       . web-mode))
;	:config
;	(setq indent-tabs-mode nil)
;	(setq web-mode-enable-css-colorization t)
;	(setq web-mode-markup-indent-offset    4)
;	(setq web-mode-css-indent-offset       4)
;	(setq web-mode-code-indent-offset      4)
;  (setq web-mode-script-padding          4)
;)

(use-package php-mode
	:mode (("\\.php\\'"        . php-mode))
	:config
	(setq indent-tabs-mode nil)
	(setq tab-width          4)
	(setq c-basic-offset     4)
)


(use-package typescript-mode
 	:mode (("\\.ts\\'"         . typescript-mode))
	:config
	(setq typescript-indent-level      2)
	)

(use-package css-mode
  :mode (("\\.css\\'" . css-mode))
  :config
  (setq css-indent-offset   2)
)

;; (use-package polymode
;;   :after rjsx-mode
;;   :config
;;   (define-hostmode poly-rjsx-hostmode nil
;;     "RJSX hostmode."
;;     :mode 'rjsx-mode)
;;   (define-innermode poly-rjsx-css-innermode nil
;;     :mode 'css-mode
;;     :head-matcher "styled[^\n]+\`\n"
;;     :tail-matcher "\`"
;;     :head-mode 'host
;;     :tail-mode 'host)
;;   (define-polymode poly-rjsx-mode
;;     :hostmode   'poly-rjsx-hostmode
;;     :innermodes '(poly-rjsx-css-innermode))
;;   (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . poly-rjsx-mode))
;; )
;;
;; (use-package js3-mode
;;   :after css-mode
;;   :mode (("\\.js\\'" . rjsx-mode))
;; 	:config (add-hook 'rjsx-mode-hook
;; 										(lambda ()
;; 											(setq-local indent-line-function 'js-jsx-indent-line)
;; 											)
;; 										)
;; )

;; The built in js mode support tabs! unlike rjsx
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

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

(use-package terraform-mode
	:mode (("\\.tf\\'" . terraform-mode)
				)
	:config (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
)

;; (load "specman-mode")
;; (add-to-list 'auto-mode-alist '("\\.e\\'" . specman-mode   ))
;; (add-to-list 'auto-mode-alist '("\\.ecom\\'" . specman-mode))

;; Use org-mode, with beancount minor mode, for .bean files
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.bean\\'" . org-mode))
(add-hook 'find-file-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "bean")
              (beancount-mode +1))))
