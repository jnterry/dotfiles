;; Always use "external" copy tool (IE: scp) rather than that built into
;; emacs (which appears to break...)
(setq tramp-copy-size-limit nil)

;; Use custom terminal type so we can detect it in zsh config and use simpler
;; prompt style compatible with tramp
(setq tramp-terminal-type "tramp")
