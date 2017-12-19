(use-package dracula-theme
  :init (load-theme 'dracula t)
  :ensure t
)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
