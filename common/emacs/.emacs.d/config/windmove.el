;; Sets keybindings for windmove package. This allows moving between panes
;; in emacs using arrow keys
;;
;; Note that windmove is build into Emacs, but doesn't have keybindings set by
;; default - this file sets them up

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enables "Ctrl-c arrow key" to move between panes
;;
;; bind-key* overrides all other modes, needed since html mode uses C-c left to go back to previous tag
(bind-key* "C-c <left>"  #'windmove-left)
(bind-key* "C-c <right>" #'windmove-right)
(bind-key* "C-c <up>"    #'windmove-up)
(bind-key* "C-c <down>"  #'windmove-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enables Shift-arrow to move between panes
;;
;; Note: many terminal emulators do not support modifier keys with arrow keys,
;; hence these bindings are disabled since they should not be used in GUI, as
;; getting used to them in the gui would be detrimental for terminal usage
;;
;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))

;;; windmove.el ends here
