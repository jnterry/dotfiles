;; Controls the theme used by emacs

;; Load the dark dracula-theme
(use-package dracula-theme
  :init (load-theme 'dracula t)
  :ensure t
)

;; Make emacs window slightly transparent
(if (or (eq system-type 'gnu) (eq system-type 'gnu/linux))
		(progn
			(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
			(add-to-list 'default-frame-alist '(alpha . (90 . 90)))))

;; Improve comment contrast
(set-face-foreground 'font-lock-comment-face "spring green")

(set-face-attribute 'default (selected-frame) :height 200)
