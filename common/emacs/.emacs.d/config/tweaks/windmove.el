;; Allows moving between windows with shift-arrow keys

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure key's don't conflict in html-mode. This is originally
;; defined to "sgml-skip-tag-backward", which goes to the start
;; of the previous tag
(load "sgml-mode.el")
(defun clear-windmove-conflicts ()
	(define-key html-mode-map (kbd "C-c <left>" ) nil)
	)
(add-hook 'html-mode-hook 'clear-windmove-conflicts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set my keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-c <left>" ) 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>"   ) 'windmove-up)
(global-set-key (kbd "C-c <down>" ) 'windmove-down)
