;; Installs and configures neotree

(use-package neotree
  :bind (("<f8>" . neotree-toggle))
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
					)))

(add-hook 'neotree-mode-hook
					(lambda ()
					  (local-set-key (kbd "u"   ) 'neotree-refresh)        ;; u for update
						(local-set-key (kbd "e"   ) 'neotree-change-root)    ;; e for enter
						(local-set-key (kbd "m"   ) 'neotree-rename-node)    ;; m for mv
						(local-set-key (kbd "r"   ) 'neotree-delete-node)    ;; r for rm
						(local-set-key (kbd "t"   ) 'neotree-create-node)    ;; t for touch
						(local-set-key (kbd "c"   ) 'neotree-copy-node)      ;; c for cp
						(local-set-key (kbd "a"   ) 'neotree-stretch-toggle) ;; a - default is A
						))
