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

;; Ensure indentation is preserved in exported src blocks
(setq org-src-preserve-indentation t)

;; Ensure SRC blocks are exported nicely when going to latex and pdf
(setq org-latex-listings t) ;; use listings package in code exports
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "listings")) ;; \usepackage listings
(add-to-list 'org-latex-packages-alist '("dvipsnames" "xcolor"))    ;; \usepackage color (for syntax highlighting)
(setq org-latex-listings-options
			'(("basicstyle" "\\footnotesize")
				("tabsize" "2")

				("keywordstyle" "\\color{NavyBlue}")
				("commentstyle" "\\color{OliveGreen}")
				("stringstyle"  "\\color{Mahogany}")

				;;("numberstyle" "\\tiny\\color{gray}")
				;;("numbers" "left")
				("numbers" "none")

				("frame" "single")
			 )
)

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

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add characters to expand into #+BEGIN #+END blocks with <c then tab
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-variable 'org-structure-template-alist ()) ;; remove defaults

(add-to-list 'org-structure-template-alist
						 '(
							 "d"
							 "#+BEGIN_DEFINITION ?\n\n#+END_DEFINITION\n" ""
							 )
						 )
(add-to-list 'org-structure-template-alist
						 '(
							 "b"
							 "#+BEGIN_BOX ?\n\n#+END_BOX\n" ""
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

(add-to-list 'org-structure-template-alist
						 '(
							 "a"
							 "#+BEGIN_ASIDE\n?\n#+END_ASIDE\n" ""
							 )
						 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-ca" 'org-agenda )
(global-set-key "\C-cc" 'org-capture)
