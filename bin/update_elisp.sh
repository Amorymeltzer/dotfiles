#!/usr/bin/env bash

":"; exec emacs --script "$0" -- "$@" # -*-emacs-lisp-*-
(setq package-archives
       (quote
        (("gnu" . "http://elpa.gnu.org/packages/")
       ("melpa" . "http://melpa.org/packages/")))
       package-menu-async nil)
(list-packages)
(if package-menu--new-package-list (message "%s" package-menu--new-package-list) nil)
(package-menu-mark-upgrades)
(package-menu-execute t)
