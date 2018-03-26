;; Tweaks for the built in abbreviation expansion capabilities of emacs
;;
;;https://www.emacswiki.org/emacs/AbbrevMode

;; Always enable abbreviation mode
(setq-default abbrev-mode t)

;; Reads the abbreviations file on startup
(quietly-read-abbrev-file)
