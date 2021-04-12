#!/usr/bin/env bash

":"; exec emacs --script "$0" -- "$@" # -*-emacs-lisp-*-
(setq package-archives
      (quote
       (("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))
      package-menu-async nil)
(list-packages)
(if package-menu--new-package-list
    (message "New packages: %s" package-menu--new-package-list)
  (message "No new packages"))
(package-menu-mark-upgrades)
(when (package-menu--find-upgrades)
  (message "Upgrading packages: %s" (map-keys (package-menu--find-upgrades)))
  (package-menu-execute t))
