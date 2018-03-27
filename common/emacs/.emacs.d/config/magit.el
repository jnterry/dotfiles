(use-package magit
	:bind (("C-x g" . magit-status))
	:config
	(setq magit-log-section-arguments '("--graph" "--color" "--decorate" "++order=topo" "-n256"))
	(setq magit-log-section-commit-count 30)
	)
