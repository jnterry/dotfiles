;; Enables the use of multiple cursors for editing the buffer
;;
;; See https://github.com/magnars/multiple-cursors.el

(use-package multiple-cursors
	:bind (("C->" . mc/mark-next-like-this)
				 ("C-<" . mc/mark-previous-like-this)
				)
)
