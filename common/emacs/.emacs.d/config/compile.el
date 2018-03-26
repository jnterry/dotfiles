;; Various tweaks to emacs compile mode and setup of key bindings

(use-package compile :after (find-file-in-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs uses next-error and previous-error to jump the cursor between errors
;; found in compiler output.
;;
;; We want to cycle back to the beginning when the end is reached, these
;; functions achieve that goal
;;
;; Originally taken from:
;; https://stackoverflow.com/questions/21125015/cycle-through-results-using-next-error-previous-error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar compile-error-wrap-flash-color "#af00d7")

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

;; Ensure that the current error is always kept 0 lines from the top of the
;; output buffer
(setq compilation-context-lines 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To make it more obvious where errors are in a source file we want to
;; highlight them
;;
;; Code original taken from https://www.emacswiki.org/emacs/CompilationMode#toc7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'custom)

(defvar all-overlays ())

(defun delete-this-overlay(overlay is-after begin end &optional len)
	(delete-overlay overlay)
	)

(defun highlight-current-line()
	(interactive)
	(setq current-point (point))
	(beginning-of-line)
	(setq beg (point))
	(forward-line 1)
	(setq end (point))
	;; Create and place the overlay
	(setq error-line-overlay (make-overlay 1 1))

	;; Append to list of all overlays
	(setq all-overlays (cons error-line-overlay all-overlays))

	(overlay-put error-line-overlay
							 'face '(background-color . "#453939"))
	(overlay-put error-line-overlay
							 'modification-hooks (list 'delete-this-overlay))
	(move-overlay error-line-overlay beg end)
	(goto-char current-point)
	)

(defun delete-all-overlays()
	(while all-overlays
		(delete-overlay (car all-overlays))
		(setq all-overlays (cdr all-overlays))
		)
	)

(defun highlight-error-lines(compilation-buffer process-result)
	(interactive)
	(delete-all-overlays)
	(condition-case nil
			(while t
				(next-error)
				(highlight-current-line)
				(save-excursion
					(compilation-next-error-function 0)
					(highlight-current-line))
				)
		(error nil))
	)

(setq compilation-finish-function 'highlight-error-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default the compile command relies on the variable "compile-command" to
;; work out what to run.
;;
;; We already use the package "find-file-in-project" to make managing a project
;; in emacs easier, so we will use its API "ffip-get-project-root-directory" to
;; find the root of the project, and then heuristically work out what the
;; compile command is
;;
;; This code is self implemented - no source online
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
		)

	;; Enlarge this window by half the size of the compile window
	;; This has shrinks the compile output to half its original height
	;; Note: We must do this after compiling the project since the compile
	;; command will re-balance the windows
	(enlarge-window (/ (window-height (next-window)) 2))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key* "M-;"  #'compile-a-project)
(bind-key* "<f5>" #'compile-a-project)
(bind-key* "M-,"  #'my-previous-error-wrapped)
(bind-key* "M-."  #'my-next-error-wrapped)
