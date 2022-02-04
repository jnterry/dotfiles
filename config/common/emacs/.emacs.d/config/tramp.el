;; Always use "external" copy tool (IE: scp) rather than that built into
;; emacs (which appears to break...)
(setq tramp-copy-size-limit nil)

;; Use custom terminal type so we can detect it in zsh config and use simpler
;; prompt style compatible with tramp
(setq tramp-terminal-type "tramp")

;; Use custom prompt pattern
;; The default matches the "last login XXX" line printed on many machines,
;; which then causes tramp to hang
;; See: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=147594
(setq tramp-shell-promp-pattern "^[^$>%]*[$>%] *")

;; Enable all debugging messages (will go in own buffer)
;; (setq tramp-verbose 6)
