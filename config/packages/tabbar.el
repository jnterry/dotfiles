;; Tabbar - Adds tabs containing open buffers at top of window

(require 'tabbar)

;; Enable tabbar
(tabbar-mode)

;; Enable hotkeys for switching between tabs
(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)

;; Make all tabs be displayed in single group, hence can see all open buffers at once
;; Groups:
;;  - user  - all user created buffers
;;  - emacs - emacs created buffers (scratch, messages)
(defun my-tabbar-buffer-groups ()
   (list (cond ((string=      "*shell*" (buffer-name)          ) "user" ) ;; put emacs shells in user group
	       ((string-equal "*" (substring (buffer-name) 0 1)) "emacs") ;; other emacs buffers in emacs group
               ((eq major-mode 'dired-mode                     ) "emacs")
               (t                                                "user" ) ;; everything else in user group
	       )
	 )
   )

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
