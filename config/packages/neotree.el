(load "ext/neotree")

(global-set-key [f8] 'neotree-toogle)

;; Start with neotree open by default
(neotree)

(setq neo-theme 'arrow)

;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
