(unless (package-installed-p 'rainbow-delimiters)
  (package-refresh-contents)
  (package-install 'rainbow-delimiters))
(require 'rainbow-delimiters)

;; use in most programming language major modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
