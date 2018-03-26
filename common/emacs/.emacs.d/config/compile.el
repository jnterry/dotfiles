;; Sets up emacs compile mode that allows building a project through emacs

;; Taken from:
;; https://stackoverflow.com/questions/21125015/cycle-through-results-using-next-error-previous-error

(defvar compile-error-wrap-flash-color "#af00d7")

(use-package compile :after (find-file-in-project))

(defun my-indicate-error-nav-wrapped (direction)
  "Display a message in minibuffer indicating that we wrapped
also flash the mode-line"
  (let ((mode-line-color (face-background 'mode-line)))
    (message "Wrapped %s error" (symbol-name direction))
    (set-face-background 'mode-line compile-error-wrap-flash-color)
    (sit-for 0.5)
    (set-face-background 'mode-line mode-line-color)))

(defun my-next-error-wrapped (&optional arg reset)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move forwards (or
backwards, if negative). With just C-u as prefix moves to first error"
  (interactive "P")
  (condition-case nil
      (call-interactively 'next-error)
    ('user-error (progn (next-error 1 t)
                        (my-indicate-error-nav-wrapped 'next)))))

(defun my-jump-to-last-error (buffer)
  "Jump to last error in the BUFFER, this assumes that
the error is at last but third line"
  (save-selected-window
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))
    (forward-line -3)
    (call-interactively 'compile-goto-error)))

(defun my-previous-error-wrapped (&optional arg)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move backwards (or
forwards, if negative)."
  (interactive "P")
  (condition-case nil
      (if (compilation-buffer-p (current-buffer))
          (compilation-previous-error 1)
        (call-interactively 'previous-error))
    ('user-error (progn
                   (let ((error-buffer (next-error-find-buffer)))
                     ;; If the buffer has an associated error buffer use it to
                     ;; to move to last error
                     (if (and (not (eq (current-buffer) error-buffer))
                              (compilation-buffer-p error-buffer))
                         (my-jump-to-last-error error-buffer)
                       ;; Otherwise move to last point and invoke previous error
                       (goto-char (point-max))
                       (call-interactively 'previous-error))
                     (my-indicate-error-nav-wrapped 'previous))))))

(defun compile-a-project ()
	"Compiles a project by finding the project root as per ffip-get-project-root
then heuristically determining the project's type and building it"
	(interactive)

	;; Close any existing compilation windows
	(progn
		(if (get-buffer "*compilation*") ; If old compile window exists
  	(progn
  	  (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
  	  (kill-buffer "*compilation*") ; and kill the buffers
  	  )))

	;; Split window below so compile output ends up at bottom of screen
	(split-window-below)

	;; Make current window larger by half of the size of the other
	;; IE: shrink the height of the compiler output
	(enlarge-window (/ (window-height (next-window)) 2))

	;; Now compile the project
	(let ((project-root (ffip-get-project-root-directory)))
		(cond
		 ;; Build a project with makefile in root of vcs repo
		 ((file-exists-p (concat project-root "Makefile"))
			(let ((compile-command (concat "cd " project-root " && make")))
				(recompile)
				)
			)
		 ;; Build a project with makefile in /build folder of vcs repo
		 ((file-exists-p (concat project-root "build/Makefile"))
			(let ((compile-command (concat "cd " project-root "build && make")))
				(recompile)
				)
		 )
		 (t (print (concat "Failed to determine how to build project at "
											 project-root)))
		 )
		))

(bind-key* "M-;"  #'compile-a-project)
(bind-key* "<f5>" #'compile-a-project)
(bind-key* "M-,"  #'my-previous-error-wrapped)
(bind-key* "M-."  #'my-next-error-wrapped)
