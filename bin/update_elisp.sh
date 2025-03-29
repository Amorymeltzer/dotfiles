#!/usr/bin/env bash

":"; exec emacs --script "$0" -- "$@" # -*-emacs-lisp-*-
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa" . 9)
	("gnu" . 6)
	("nongnu" . 3))
      package-menu-async nil)
(unless package--initialized
  (package-initialize))
(list-packages)
(if package-menu--new-package-list
    (message "New packages: %s" package-menu--new-package-list)
  (message "No new packages"))
(package-menu-mark-upgrades)
(when (package-menu--find-upgrades)
  (message "Upgrading packages: %s" (map-keys (package-menu--find-upgrades)))
  (package-menu-execute t))
