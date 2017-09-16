#!/usr/bin/env bash

rm `find ~/.emacs.d | grep .elc`

rm    ~/.emacs.d/custom.el
touch ~/.emacs.d/custom.el
