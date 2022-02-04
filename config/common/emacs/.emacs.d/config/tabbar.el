;; Tabbar - Adds tabs containing open buffers at top of window

(use-package awesome-tab
	:load-path "~/.emacs.d/ext/awesome-tab.el"
	:config
	(awesome-tab-mode t)
	:init
	(global-set-key (kbd "M-[") 'awesome-tab-backward-tab)
	(global-set-key (kbd "M-]") 'awesome-tab-forward-tab )
)
