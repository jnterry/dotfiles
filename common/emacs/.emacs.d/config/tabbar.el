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
;;  - emacs - emacs created buffers (scratch, messages, etc)
;;  - cmd   - terminal replacement windows - these are the buffers which replace
;;            activities traditionally performed in terminal
;;  - code  - all other buffers
(defun my-tabbar-buffer-groups ()
  (list (cond
				 ;; Group together buffers which are replacements for terminal
				 ((string= "*shell*"         (buffer-name)                ) "terminal")
				 ((string= "*terminal*"      (buffer-name)                ) "terminal")
				 ((string= "*ansi-terminal*" (buffer-name)                ) "terminal")
				 ((string= "*compilation*"   (buffer-name)                ) "terminal")
				 ((string= "COMMIT_EDITMSG"  (buffer-name)                ) "terminal")
				 ((string= "magit"           (substring (buffer-name) 0 5)) "terminal")
				 ((string= "*magit"          (substring (buffer-name) 0 6)) "terminal")
				 ((string= "git-rebase-todo" (buffer-name)                ) "terminal")

				 ;; All other emacs special buffers go in their own group
				 ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
				 ((eq major-mode 'dired-mode                     ) "emacs")

				 ;; Default case -> everything else in user group
				 (t                                                "user" )
				)
		)
  )

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
