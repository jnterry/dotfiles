(use-package column-enforce-mode
  :config (setq column-enforce-column 80)
	:init   (global-column-enforce-mode t)
	:diminish column-enforce-mode
)
