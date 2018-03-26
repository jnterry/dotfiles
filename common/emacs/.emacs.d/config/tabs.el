;; Use tabs for indentation, and spaces for alignment
(use-package smart-tabs-mode)

;; Use tab-width of 2
(setq-default tab-width 2)
(setq         tab-width 2)
(defvaralias 'c-basic-offset     'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level    'tab-width)

;; Use tabs everywhere
(setq-default indent-tabs-mode 1)
(setq         indent-tabs-mode 1)

;; Set languages to use smart tabs mode
(smart-tabs-insinuate 'c 'c++ 'java 'javascript)
