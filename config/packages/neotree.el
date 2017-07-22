(add-to-list 'load-path "~/.emacs.d/config/ext/all-the-icons/")
(require 'all-the-icons)

(load "ext/neotree")

(global-set-key [f8] 'neotree-toggle)

;; Use graphical icons for file types/directories in graphics mode,
;; else us standard collapseable arrows
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Overwrite default hidden regex to show most hidden files, but not .git
(setq neo-hidden-regexp-list
      '(
	"^\\.git$" ;; Git dir
	"~$"      ;; emacs backup files
	"^#.*#$"  ;; emacs backup files
	"\\.elc$" ;; compiled el files
	)
      )

;; Start with neotree open by default
(neotree)
