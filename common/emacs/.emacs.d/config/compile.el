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

;; Scroll compilation buffer as the process runs
(setq compilation-scroll-output t)

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

				;; Highlight the line in compilation buffer
				;; (highlight-current-line)

				;; Highlight the line in the source file
				(save-excursion
					(compilation-next-error-function 0)
					(highlight-current-line))
				)
		(error nil))
	)

(setq compilation-finish-functions 'highlight-error-lines)

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

(defun my-compile---do-compile-project ()
	;; Now compile the project
	(let ((project-root (ffip-get-project-root-directory))

				;; make output typically includes lines such as:
				;; file.cpp:10: recipe for target 'file.cpp.o' failed
				;;
				;; These are picked up by compilation mode as errors, hence using
				;; "next-error" hotkeys will jump to lines inside the make files
				;; In reality, it is the source code that is at fault - not the makefile
				;; and we don't want to edit the make file.
				;;
				;; Hence we use a command which filters those lines out:
				;; grep:
				;;  -v captures all but matches
				;;  -w does as whole word
				;;  see: https://askubuntu.com/questions/354993/how-to-remove-lines-from-the-text-file-containing-specific-words-through-termina
				;;
				;; The set pipefail ensures the error code from make is propagated
				;; out of the overall command (rather than the 0 returned by grep)
				;; see: https://stackoverflow.com/questions/1221833/pipe-output-and-capture-exit-status-in-bash
				(make-cmd "(set -o pipefail; make | grep -vwE 'recipe.*failed');")
				)

		(cond
		 ;; Build a project with makefile in root of vcs repo
		 ((file-exists-p (concat project-root "Makefile"))
			(let ((compile-command (concat "cd " project-root " && " make-cmd)))
				(recompile)
				)
			)
		 ;; Build a project with makefile in /build folder of vcs repo
		 ((file-exists-p (concat project-root "build/Makefile"))
			(let ((compile-command (concat "cd " project-root "build && " make-cmd)))
				(recompile)
				)
		 )
		 (t (message (concat "Failed to determine how to build project at '"
												 project-root
												 "'. Add a heuristic to ~/.emacs.d/config/compile.el")))
		 )
		))


(defun compile-a-project ()
	"Compiles a project by finding the project root as per ffip-get-project-root
then heuristically determining the project's type and building it"
	(interactive)

	(let ((existing-compilation-buffer (get-buffer "*compilation*"))
				(initial-buffer              (current-buffer))
			 )

		;; Open new compilation buffer if one doesn't already exist.
		;; If one does exist we assume the user is happy with its current layout
		;; (eg, as a spilt in current frame, open in a different frame, etc)
		(unless existing-compilation-buffer (split-window-below))

		;; Compile the project
		(my-compile---do-compile-project)

		;; Make sure we haven't moved the user away from the buffer they ran the
		;; compile command in
		(switch-to-buffer initial-buffer)

		;; If we created a buffer for the compilation output it will be taking up
		;; 50% of the screen. Half it down to 25% by enlarging the active window
		;;
		;; Note: We must do this after compiling the project since the compile
		;; command will re-balance the window sizes
		(unless existing-compilation-buffer
			(enlarge-window (/ (window-height (next-window)) 2))
			))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify compilation buffer appearance - by default it is perhaps best
;; described as a messy rainbow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq compilation-info-face
			'((t :foreground "cyan2"
					 :weight bold
					 )))
(setq compilation-warning-face
			'((t :foreground "orange2"
					 :weight bold
					 )))
(setq compilation-error-face
			'((t :foreground "red3"
					 :weight bold
					 )))
(setq compilation-message-face ())
(setq compilation-line-face ())
(setq compilation-message-face
			'((t :weight bold
					 )))
(setq compilation-line-face
			'((t :weight bold
					 )))
(setq compilation-column-face
			'((t :weight bold
					 )))

;; :TODO: ideally we want to preserve the colors output by the build system
;;
;; The code below should convert any ansi escape sequences defining colors into
;; appropriately formatted text, but for some reason the build system doesn't
;; seem to be including the ansi sequences. Apparently most programs checks a
;; some "TERM" env variable to check if colors are supported, emacs can override
;; this
;; see: https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
;;
;; BUT this doesn't appear to work:
;;   (setq compilation-environment "xterm-256color")
;; Try changing the compile command to echo $TERM
;; if the line above is commented out we get "dumb", as per that stack overflow
;; post, but if the above line is not commented out we get an empty line
;; Not sure how to fix

;; Ensure ansi escape codes for colors are interpreted correctly in the
;; compilation buffer
;; Taken from:
;; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(bind-key* "M-;"  #'compile-a-project)
(bind-key* "<f5>" #'compile-a-project)
(bind-key* "M-,"  #'my-previous-error-wrapped)
(bind-key* "M-."  #'my-next-error-wrapped)
