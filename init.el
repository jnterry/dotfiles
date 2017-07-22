;; Setup packge repository
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Add load paths
(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/config/ext/")

;; Load individual setup files
(load "theme")

;; Load config directories
(load "ext/load-directory")
(load-directory "~/.emacs.d/config/packages")
(load-directory "~/.emacs.d/config/hooks")
