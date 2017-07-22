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
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (setq use-package-always-ensure t))


;; Add load paths
(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/config/ext/")

;; Load individual setup files
(load "theme")
(load "misc-tweaks")

;; Load config directories
(load "ext/load-directory")
(load-directory "~/.emacs.d/config/packages")
(load-directory "~/.emacs.d/config/hooks")
(load-directory "~/.emacs.d/config/tweaks")
