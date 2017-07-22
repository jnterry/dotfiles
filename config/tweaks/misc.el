;; Set personal data
(setq user-full-name    "Jamie Terry"
      user-mail-address "jnterry@ntlworld.com")

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Have buffers reload when files are changed on disk
(global-auto-revert-mode t)

;; Enable line numbers
(global-linum-mode t)

;; Hide UI faff
(tool-bar-mode -1) ;; Top menu bar
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Dispaly time in modeline
(display-time-mode 1)
