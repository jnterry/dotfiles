;; Tabbar - Adds tabs containing open buffers at top of window

(unless (package-installed-p 'tabbar)
  (package-refresh-contents)
  (package-install 'tabbar))

(require 'tabbar)

;; Enable tabbar
(tabbar-mode)

;; Enable hotkeys for switching between tabs
(global-set-key (kbd "M-[") 'tabbar-backward-tab)
(global-set-key (kbd "M-]") 'tabbar-forward-tab )


(defun kill-other-buffers ()
      "Kills buffers but the current"
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (delq (neotree) (buffer-list))))
)

(global-set-key (kbd "<M-f3>") 'kill-other-buffers)

;; By default tabbar tries to put buffers from the same language into a "group"
;; It then only displays tabs in the same group as the currently opened buffer
;; This is a pain for a lot of tasks, eg, editing html, css and js simultaniouly
;;
;; This makes everything go into a single group named "user", apart from a few
;; special emacs buffers
;; Groups:
;;  - user  - all user created buffers
;;  - emacs - emacs created buffers (scratch, messages)
(defun my-tabbar-buffer-groups ()
  (list (cond
				 ;; Special cases -> some emacs buffers we want to be in the same group
				 ;; as files
				 ((string=      "*shell*"       (buffer-name)    ) "user" )
				 ((string=      "*terminal*"    (buffer-name)    ) "user" )
				 ((string=      "*compilation*" (buffer-name)    ) "user" )

				 ;; All other emacs special buffers go in their own group
				 ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
				 ((eq major-mode 'dired-mode                     ) "emacs")

				 ;; Default case -> everything else in user group
				 (t                                                "user" )
				)
		)
  )

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
