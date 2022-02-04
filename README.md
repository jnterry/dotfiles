# What
Contains personal dotfiles for configuring various applications.

These files are managed by GNU stow; this creates system links in the correct location that point to the files in this repository. This allows all dotfiles to be tracked by a single repository, while being easily rolled out on a new machine


- `./config` - config files installed by stow
  - `./config/common` - files installed on all hosts
  - `./config/${name}` - files installed on hosts with particular names
- `./setup` - files for setting up a new debian machine

# Continuous Integration

[![Build Status](https://travis-ci.org/jnterry/dotfiles.svg?branch=master)](https://travis-ci.org/jnterry/dotfiles)

At first glance having CI for dotfiles may seem strange - however when we note that emacs is configured using a full Turing complete language it begins to make more sense. The idea here is to test that the config can be rolled out to a completely blank slate machine (like that provided by a continuous integration environment) in order to ensure the config does not rely on any custom human made changes to the system.

:TODO: can we test the other configs as well? Eg, check that programs can parse their .dotfiles

# To Install

1. ```git clone --recurse-submodules -j8 git@github.com:jnterry/dotfiles.git```
2. Run ```install.sh```
