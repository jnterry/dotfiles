(use-package find-file-in-project
	:bind (("M-'" . find-file-in-project)
				 ("M-#" . find-file-with-similar-name)
				 )
	:config
	(add-to-list 'ffip-prune-patterns "*/docs/*")
	(add-to-list 'ffip-prune-patterns "*/build/*")
)
