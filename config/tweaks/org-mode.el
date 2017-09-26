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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add characters to expand into #+BEGIN #+END blocks with <c then tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-variable 'org-structure-template-alist ()) ;; remove defaults

(add-to-list 'org-structure-template-alist
						 '(
							 "d"
							 "#+BEGIN_DEFINITION\n?\n#+END_DEFINITION\n" ""
							 )
						 )
(add-to-list 'org-structure-template-alist
						 '(
							 "e"
							 "#+BEGIN_EQUATION\n?\n#+END_EQUATION\n" ""
							 )
						 )
(add-to-list 'org-structure-template-alist
						 '(
							 "s"
							 "#+BEGIN_SRC ?\n\n#+END_SRC\n" ""
							 )
						 )
(add-to-list 'org-structure-template-alist
						 '(
							 "q"
							 "#+BEGIN_QUOTE\n?\n#+END_QUOTE\n" ""
							 )
						 )
(add-to-list 'org-structure-template-alist
						 '(
							 "n"
							 "#+BEGIN_SIDENOTE\n?\n#+END_SIDENOTE\n" ""
							 )
						 )
(add-to-list 'org-structure-template-alist
						 '(
							 "c"
							 "#+BEGIN_COLLAPSE ?\n\n#+END_COLLAPSE\n" ""
							 )
						 )
