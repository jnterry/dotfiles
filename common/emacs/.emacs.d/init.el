;;; init.el --- Root emacs config file
;;;
;;; Commentary:
;;; Sets up basic settings and then loads all other config files
;;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide UI faff. This is done as very first operation to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup this instance as an emacs server if we are running in GUI mode (a
;; a terminal server should be started at login by shell config)
(load "server")

(if (display-graphic-p) (progn
													(setq server-name "gui")
													(unless (server-running-p) (server-start)
														)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup packge repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install use-package to make managing other installed packages easier, enable
;; lazy load, etc
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl-lib)
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t)
)
;; use diminish to remove always on minor-modes from status bar at bottom
;; Needs to be included before any other packages so we can use :diminish
;; keyword, hence load in init.el rather than as a file in config directory
(use-package diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't save emacs generated customisations in this file since this file is
;; tracked by git and we don't want to include them in the repository
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add load paths
(add-to-list 'load-path "~/.emacs.d/ext/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load config directories
(defun load-directory (dir)
	(mapc
	 (lambda (f) (load-file (concat (file-name-as-directory dir) f)))
	 (directory-files dir nil "\\.el$"))
	)
(load-directory "~/.emacs.d/config")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure all emacs files are byte compile so subsequent start-ups
;; are quicker. This is done after loading everything so everything
;; is defined at this point
(byte-recompile-directory (expand-file-name "~/.emacs.d/config") 0)

;;; init.el ends here
