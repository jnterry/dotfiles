;; Hide UI faff at startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Setup packge repository
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Install use-package to make managing installed packages easier
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl-lib)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (setq use-package-always-ensure t))

;; Don't save emacs generated customisations in this file since
;; this file is tracked by git
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add load paths
(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/config/ext/")

;; Load individual setup files
(load "ext/dracula-theme")

;; Load config directories
(load "ext/load-directory")
(load-directory "~/.emacs.d/config/packages")
(load-directory "~/.emacs.d/config/tweaks")

;; Ensure all emacs files are byte compile so subsequent start-ups
;; are quicker. This is done after loading everything so everything
;; is defined at this point
(byte-recompile-directory (expand-file-name "~/.emacs.d/config") 0)
