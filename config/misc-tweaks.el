(setq user-full-name    "Jamie Terry"
      user-mail-address "jnterry@ntlworld.com")

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;;have buffers reload when files are changed on disk
(global-auto-revert-mode t)
