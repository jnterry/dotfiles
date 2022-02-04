;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets various config options for manipulating windows in emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enables "Ctrl-c arrow key" to move between panes
;;
;; windmove is built into emacs, but keybindings are not enabled by default
;;
;; bind-key* overrides all other modes, needed since in html mode C-c left means
;; go back to previous tag
(bind-key* "C-c <left>"  #'windmove-left)
(bind-key* "C-c <right>" #'windmove-right)
(bind-key* "C-c <up>"    #'windmove-up)
(bind-key* "C-c <down>"  #'windmove-down)

;; Could use default keybindings for windmove which is shift and arrow keys,
;; but many terminal emulators do not support modifier keys with arrow keys,
;; hence these bindings are disabled since they should not be used in GUI, as
;; getting used to them in the gui would be detrimental for terminal usage
;;
;; Code to enable default key bindings is below:

;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adds function to "pop" the currently selected window out into its own frame
;;
;; Taken from:
;; https://emacs.stackexchange.com/questions/7116/pop-a-window-into-a-frame
(defun my-turn-current-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;; Using C-x 4 as C-x 1, C-x 2 and C-x 3 are used by default for window
;; management
(bind-key* "C-x 4" #'my-turn-current-window-into-frame)
