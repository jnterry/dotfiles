; Modifies various configuration options for org-mode

;; org mode is loaded by default by emacs, but we want access to some org mode
;; variables/function in this file without compiler warnings, also we want to
;; ensure that we have the latest version
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(require 'org)

;; Determine the major version of the org package, some of this config relies on
;; a relatively modern version, so we need to check
(setq org-major-version (string-to-number (nth 0 (split-string org-version "\\."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup images in org files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; :TODO: drag and drop images into org mode
;; Drag and drop images into org-mode
;; (use-package org-download)

;; Configure showing latex equation previews
;; use keybind C-c C-x C-l
(setq org-latex-create-formula-image-program 'dvipng)
(setq org-format-latex-options
			(quote (:foreground auto
							:background auto
							:scale 1.7
							:html-foreground "Black"
							:html-background "Transparent"
							:html-scale 1.0
							:matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

;; Only display 300px of wide images
(setq org-image-actual-width '(300))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup SRC block editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display code blocks with usual syntax highlighting
(setq org-src-fontify-natively t)

;; Lets tab work as normal in code blocks
(setq org-src-tab-acts-natively t)

;; Highlight start and end line for blocks in editor
(defface org-block-begin-line
  '((t ( :foreground "spring green"
				 :background "#112f11"
			 )))
  "Face used for the line delimiting the begin of source blocks.")
(defface org-block-end-line
  '((t ( :foreground "spring green"
				 :background "#112f11"
			 )))
  "Face used for the line delimiting the begin of source blocks.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure source blocks are exported nicely to latex and pdf, this includes:
;; - tabs that aren't size 0
;; - syntax highlighting
;; - outline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure indentation is preserved in exported src blocks
(setq org-src-preserve-indentation t)

;; Note, this relies on requiring ox-latex, this is built into org-mode
;; versions 8.0.0 and above, hence we check the major version first
(when (>= org-major-version 8)
	(require 'ox-latex)
	(setq org-latex-listings t) ;; use listings package in code exports
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
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup org mode src block editing
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
;; Setup org mode source block execution (babel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
	 (python . t)
	 (C      . t)
	 (js     . t)
	 (sh     . t)
	 ))

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

(add-to-list 'org-structure-template-alist
						 '(
							 "t"
							 "#+BEGIN_TODO\n?\n#+END_TODO\n" ""
							 )
						 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-ca" 'org-agenda )
(global-set-key "\C-cc" 'org-capture)
