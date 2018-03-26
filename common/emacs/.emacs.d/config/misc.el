;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Have buffers reload when files are changed on disk
(global-auto-revert-mode t)

;; Enable line numbers
(global-linum-mode t)

;; Dispaly time in modeline
(display-time-mode 1)

;; Ensure that after a Ctrl+c in Linux X11, you can paste in emacs
(setq select-enable-clipboard t)

;; Delete whitespace at end of lines on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
