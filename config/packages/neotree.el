(add-to-list 'load-path "~/.emacs.d/config/ext/all-the-icons/")
(require 'all-the-icons)

(load "ext/neotree")

(global-set-key [f8] 'neotree-toogle)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Start with neotree open by default
(neotree)
