(use-package column-enforce-mode
  :config (setq column-enforce-column 100)
	:init   (global-column-enforce-mode t)
	:diminish column-enforce-mode
)
