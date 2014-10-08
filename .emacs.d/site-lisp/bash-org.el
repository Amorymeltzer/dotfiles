;;; bash-org.el --- post to bash.org from Emacs

;; Copyright (C) 2007  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: comm
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The Quote Database at bash.org contains many funny quotes from IRC
;; and other chat systems. This is a library to simplify posting to
;; bash.org from within Emacs. The user-serviceable entry point is
;; `bash-org-send-region', which see.


;;; History:
;; 2007-08-21: initial version.

;;; Code:

(require 'url)

(defvar bash-org-debug t)

(defvar bash-org-add-form "http://bash.org/?add"
  "URL to bash.org's add form.")

(defun bash-org-request-data (message)
  "Format MESSAGE suitable for POSTing to `bash-org-add-form'."
  (mapconcat (lambda (cons)
               (format "%s=%s"
                       (url-hexify-string (car cons))
                       (url-hexify-string (cdr cons))))
             `(("newquote" . ,message)
               ("strip" . "checked")
               ,(if bash-org-debug
                    '("submit2" . "Preview Quote")
                  '("submit1" . "Submit Quote")))
             "&"))

(defun bash-org-send-region (start end)
  "Send the region from START to END to bash.org as a new quote."
  (interactive "r")
  (let ((url-package-name "bash-org.el")
        (url-request-method "POST")
        (url-request-data
         (bash-org-request-data (buffer-substring start end)))
        buf)
    (message "%s" url-request-data)
    (setq buf (url-retrieve-synchronously bash-org-add-form))
    (if bash-org-debug
        (switch-to-buffer buf)
      (kill-buffer buf))))

(provide 'bash-org)
;;; bash-org.el ends here
