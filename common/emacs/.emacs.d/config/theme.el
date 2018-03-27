;; Controls the theme used by emacs

;; Load the dark dracula-theme
(use-package dracula-theme
  :init (load-theme 'dracula t)
  :ensure t
)

;; Make emacs window slightly transparent
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
