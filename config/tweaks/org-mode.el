;; Tweaks to ORG mode

;; Drag and drop images into org-mode
(use-package org-download
  :ensure t
  :defer 2)

(setq org-latex-create-formula-image-program 'dvipng)

;; Only display 300px of wide images
(setq org-image-actual-width '(300))

;; Display code blocks with usual syntax highlighting
(setq org-src-fontify-natively t)

;; Lets tab work as normal in code blocks
(setq org-src-tab-acts-natively t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org mode script blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-src-lang-modes (quote (("elisp" . emacs-lisp)
								 ("C" . c)
								 ("cpp" . c++)
								 ("perl" . perl)
								 )
								)
	  )

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
