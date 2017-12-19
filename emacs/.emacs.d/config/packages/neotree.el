(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :defer
  :config
  ;; Don't use file/directory icons in neotree pane
  (setq neo-theme 'arrow)
  ;; Overwrite default hidden regex to show most hidden files, but not .git
  (setq neo-hidden-regexp-list
      '("^\\.git$" ;; Git dir
	    "~$"       ;; emacs backup files
	    "^#.*#$"   ;; emacs backup files
	    "^\\.#"    ;; emacs backup files
	    "\\.elc$"  ;; compiled el files
		)
	  )
)
