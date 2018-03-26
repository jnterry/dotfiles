;; Add bindings for expanding a selection by semantic groups
;;
;; https://github.com/magnars/expand-region.el

(use-package expand-region
  :bind (
				 ("C-=" . er/expand-region)
				 ("C--" . er/contract-region)
				 )
)
