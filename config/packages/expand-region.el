;; Expand selection by semantic groups

(use-package expand-region
  :bind (
				 ("C-=" . er/expand-region)
				 ("C--" . er/contract-region)
				 )
)
