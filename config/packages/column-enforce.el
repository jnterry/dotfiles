(load "ext/column-enforce-mode")

(use-package `

(use-package column-enforce
  :config
	(setq column-enforce-column 80)
	:init
	(global-column-enforce-mode t)
)
