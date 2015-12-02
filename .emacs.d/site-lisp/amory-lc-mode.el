;;; amory-lc-mode.el --- show total line count information in status bar
;;
;; Copyright (C) 2011-2012 Toby Cubitt
;; Altered 2014 Amory Meltzer <amorymeltzer@gmail.com>
;;
;; Original author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: length, lines, mode line
;; URL:
;;
;;; Commentary:
;;
;; A simple minor-mode to display the length of the buffer in the status bar.
;; This file is NOT part of Emacs.
;;
;;
;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.
;;
;;; Change log:
;;
;; Version 0.1
;; * initial release
;;
;;; Code:

;; add length display to mode-line construct
(setq mode-line-position (assq-delete-all 'amory-lc-mode mode-line-position))

(setq mode-line-position
      (append
       mode-line-position
       '((amory-lc-mode
	  ;; (3 (:eval (format " %d" ;; Space only really important on lines >10k
	  (2 (:eval (format " L: %d"
			    ;; (point-max)
			    ;; (count-words-region (point-min) (point-max))
			    (line-number-at-pos (point-max)))))
	  nil))))


(define-minor-mode amory-lc-mode
  "Toggle line-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the total number of lines is
displayed in the mode-line.")

(provide 'amory-lc-mode)

;;; amory-lc-mode.el ends here
