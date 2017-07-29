;; Use tabs for indentation, and spaces for alignment
(use-package smart-tabs-mode)

(setq-default tab-width 4)
(setq         tab-width 4)

(setq-default indent-tabs-mode 1)
(setq         indent-tabs-mode 1)

;; Set languages to use smart tabs mode
(smart-tabs-insinuate 'c 'c++ 'java 'javascript)
