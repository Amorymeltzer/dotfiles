;; quotes.el --- A mode to keep a list of easily searchable quotes
;;
;; Author: Jay Belanger <belanger@truman.edu>
;; $Date: 2006/05/08 18:58:44 $
;; $Revision: 1.2 $

;; This file is *NOT* part of GNU Emacs.

;; Copyright (C) 2003 Michael Herstine <sp1ff@pobox.com>
;; Copyright (C) 2000 Jay Belanger <belanger@truman.edu>
;; Copyright (C) 1989 Paul Davis

;; This program  is free software; you  can redistribute it  and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2  of the License, or (at your option)
;; any later version.

;; This  program is  distributed in  the  hope that  it will  be useful,  but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR  A PARTICULAR PURPOSE.  See the  GNU General Public License
;; for more details.

;; You should  have received a copy  of the GNU General  Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; Quick Start:

;; Unzip  the file  Quotes.gz into  ~/.quotes/ (both  of these  are customiz-
;; able).  Quotes.gz is currently available at:

;;   ftp://vh213601.truman.edu/pub/Quotes/Quotes.gz

;; Then load this file, and start quotes with M-x quotes.

;; More generally, place this file somewhere in your `load-path', and add the
;; following to your `.emacs' (or otherwise arrange to have it evaluated):

;;  (autoload 'quotes-mode "quotes" "A mode for keeping quotes." t)

;;  (autoload 'quotes "quotes"  "A command to start quotes." t)

;; The `quotes-mode' description has more information.

;; This is based on Paul Davis's rolo.el

;;; Code

(defvar quotes-entry-delimiter "%"
  "Regexp used to delimit entries in a Quotes buffer")

(defvar quotes-entry-delimiter-regexp
  (concat "^" quotes-entry-delimiter)
  "Regexp used to locate start and end of entries in a Quotes buffer")

(defvar quotes-entry-total nil
  "Total number of quotes entries in current quotes-mode buffer")

(defvar quotes-default-directory "~/.quotes/"
  "The default directory to keep the quotes in.")

(defvar quotes-default-file "Quotes.gz"
  "The default file where the quotes are kept")

(defvar quotes-buffer nil
  "The buffer with the quotes.")

(make-variable-buffer-local 'quotes-buffer)

(defvar quotes-search-result-buffer nil
  "The buffer with the quotes search-result.")

(make-variable-buffer-local 'quotes-search-result-buffer)

(defvar quotes-search-result-p nil
  "Flag to tell whether or not there is a search-result.")

(make-variable-buffer-local 'quotes-search-result-p)

(defvar quotes-mode-map nil)

(defvar quotes-quote-prompt " ")

(defvar quotes-search-result-mode-map nil)

(defface quotes-search-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defvar quotes-search-result-line)

(defface quotes-search-result-line-face
  '((t (:background "lightgray")))
;        :foreground "gray")))
  "Face used to highlight the current line in the search result buffer.")

(defun quotes ()
  "Visit the quotes-file and display as a series of sequential entries.
Functions are available for locating and searching the entries.

The quotes-file format consists of each entry terminated by the
character quotes-entry-delimiter (% by default). "
  (interactive)
  (find-file (expand-file-name
               (read-file-name "Quotes file: "
                      quotes-default-directory
                      nil
                      nil
                      quotes-default-file)))
  (quotes-mode))


;; Quotes mode itself

(defun quotes-mode ()
  "A mode for keeping quotes.
Note that by default, searching in this mode is case-insensitive. If
you wish to alter this, add a quotes-mode-hook to your .emacs file.

To move to the next entry, use \"n\" or C-c C-n
To move to the previous entry, use \"p\" or C-c C-p
To scroll forward within an entry, use SPC,
To scroll backward, use \"b\".
To search through the entries, use \"s\" or C-c C-s
To do a reverse search through the entries, use \"r\" or C-c C-r

The command \"S\" or C-c TAB will create a
search-result buffer,  containing all the lines of the entries which
contain a given regular expression.
In the search-result buffer, the command RET will display the corresponding
entry in the quotes buffer,  the command \"n\" will move to the next
line and display the corresponding entry in the quotes buffer, and
the command \"p\" will move to the previous line and display the
corresponding entry in the quotes buffer. A SPC will scroll-forward in
search-result buffer, a \"b\" will scroll backward. The command \"q\" will
kill the search-result buffer.

To quit quotes, the command \"q\" can be used.

The quotes are meant to be read, not edited, but the following commands
are available for editing:
\\[quotes-delete-entry] will delete the current entry
\\[quotes-create-entry] will create a new entry after the current
entry (with an argument, it will create a new entry before the current
entry).
\\[quotes-edit-entry] will make the buffer editable."
  (interactive)
  (kill-all-local-variables)
  (widen)
  (setq major-mode 'quotes-mode)
  (setq mode-name "Quotes")
  (use-local-map quotes-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq mode-line-format '("" mode-line-modified
			   mode-line-buffer-identification
			   "   " global-mode-string "   %[("
			   mode-name "%n" mode-line-process
			   ")%]" "-%-"))
  (setq case-fold-search t)
  (setq quotes-entry-total (1- (quotes-count-entries-to (point-max))))
  (quotes-show-entry 1)
  (setq buffer-read-only t)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook
             (lambda ()
                (save-excursion
                   (if quotes-search-result-p
                       (progn
                          (pop-to-buffer quotes-search-result-buffer)
                          (quotes-search-result-kill-search-result))))) t t)
  (run-hooks 'quotes-mode-hook))


(defun quotes-search-result-mode ()
  "A mode for the search-result of a quotes.
This buffer will contain one line per entry in the quotes, and the
search-result line will contain the line of the quotes entry containing
the regular expression being searched for.
The command \\[quotes-search-result-current-entry] will display the
corresponding entry in the quotes buffer,  the command
\\[quotes-search-result-next-entry] will move to the next line and
display the corresponding entry in the quotes buffer, and the command
\\[quotes-search-result-previous-entry] will move to the previous line
and display the corresponding entry in the quotes buffer.
A SPC will scroll forward in the search result buffer, a \"b\" will
scroll backward.
Finally, the command \\[quotes-search-result-kill-search-result]  will kill
the search-result buffer."
  (setq major-mode 'quotes-search-result-mode)
  (setq mode-name "Quotes Search Result")
  (use-local-map quotes-search-result-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq truncate-lines t)
  (add-hook 'post-command-hook #'quotes-search-result-highlight-line nil t)
  (run-hooks 'quotes-search-result-mode-hook))


;; Keymaps

(defvar quotes-mode-map nil
  "Keymap for quotes-mode")

(if quotes-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-i" 'quotes-create-search-result)
    (define-key map "S" 'quotes-create-search-result)
    (define-key map "\C-c\C-n" 'quotes-next-entry)
    (define-key map "n" 'quotes-next-entry)
    (define-key map "\C-c\C-p" 'quotes-previous-entry)
    (define-key map "p" 'quotes-previous-entry)
    (define-key map "\C-c\C-s" 'quotes-search)
    (define-key map "s" 'quotes-search)
    (define-key map "\C-c\C-r" 'quotes-backward-search)
    (define-key map "r" 'quotes-backward-search)
    (define-key map "\C-c\C-d" 'quotes-delete-entry)
    (define-key map "\C-c\C-c" 'quotes-create-entry)
    (define-key map "\C-c\C-e" 'quotes-edit-entry)
    (define-key map "q" 'quotes-quit)
    (define-key map " " 'scroll-up)
    (define-key map "b" 'scroll-down)
    (define-key map "\C-x\C-s" 'quotes-save-buffer)
    (setq quotes-mode-map map)))

(defvar quotes-search-result-mode-map nil
  "Keymap for quotes-search-result-mode")

(if quotes-search-result-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'quotes-search-result-next-entry)
    (define-key map "p" 'quotes-search-result-previous-entry)
    (define-key map "q" 'quotes-search-result-kill-search-result)
    (define-key map "\C-m" 'quotes-search-result-current-entry)
    (define-key map " " 'scroll-up)
    (define-key map "b" 'scroll-down)
    (setq quotes-search-result-mode-map map)))



;; Displaying the entries

(defun quotes-show-entry (&optional n)
  "Display the current entry, (that which point is currently within),
or the Nth entry if N is non-null."
  (interactive "pn")
  (quotes-save-changes)
  (if (null n)
      (quotes-intern-display-this-entry)
    (if (or (> n quotes-entry-total) (< n 1))
	(message "no such entry")
      (let ((here (point)))
	(widen)
       (goto-char (point-min))
	(if (null (re-search-forward quotes-entry-delimiter-regexp
				     (point-max) t n))
;				     (point-max) t (1+ n)))
	    (progn
	      (goto-char here)
	      (quotes-show-entry)
	      (message "no such entry")))
	(previous-line 1)
	(quotes-intern-display-this-entry)))))

(defun quotes-intern-display-this-entry ()
  (narrow-to-region (quotes-entry-beg)
		    (max 1 (1- (quotes-entry-end))))
  (goto-char (point-min))
  (quotes-update-mode-line))

(defun quotes-count-entries-to (position)
  "Count the number of entries up to (and including) the current entry."
  (let ((n 1))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (and (< (point) position)
                  (re-search-forward quotes-entry-delimiter-regexp
                                     position t))
	(setq n (1+ n))
	(forward-line 1)))
    (max 1 n)))

(defun quotes-current-entry ()
  "The current entry number."
  (save-excursion
    (quotes-count-entries-to (point))))


;; motion control

(defun quotes-next-entry (&optional n)
  "Show the next entry, or the Nth-next entry if N is non-null."
  (interactive "pn")
  (if n
      (quotes-show-entry (+ (quotes-current-entry) n))
    (quotes-show-entry (1+ (quotes-current-entry)))))

(defun quotes-previous-entry (&optional n)
  "Show the previous entry, or the Nth-previous entry if N is non-null."
  (interactive "pn")
  (if n
      (quotes-show-entry (- (quotes-current-entry) n))
    (quotes-show-entry (1- (quotes-current-entry)))))


;; Searching

(defvar quotes-search-regexp nil
  "Last regexp used by quotes-search")

(make-variable-buffer-local 'quotes-search-regexp)

(defun quotes-search (regexp)
  "Search for a regular expression throughout the entries."
  (interactive
   (list (read-string (concat "Search ahead for"
				  (if quotes-search-regexp
				   (concat " ("
					   quotes-search-regexp
					   ")"))
				  ": "))))
  (if (equal "" regexp)
      (setq regexp quotes-search-regexp))
  (let ((start (point-min))
	(end (point-max))
	(here (point)))
    (widen)
    (if  (equal "" regexp)
        (forward-char 1))
    (if (null (re-search-forward regexp (point-max) t))
	(progn
	  (narrow-to-region start end)
	  (goto-char here)
	  (message (concat "\"" regexp "\" not found.")))
      (save-excursion
	(quotes-show-entry)))
    (setq quotes-search-regexp regexp)))

(defun quotes-backward-search (regexp)
  "Reverse search for a regular expression throughout the entries."
  (interactive
   (list (read-string (concat "Quotes Backward Search for"
				  (if quotes-search-regexp
				   (concat " ("
					   quotes-search-regexp
					   ")"))
				  ": "))))
  (if (equal "" regexp)
      (setq regexp quotes-search-regexp))
  (let ((start (point-min))
	(end (point-max))
	(here (point)))
    (widen)
    (if (equal "" regexp)
	(forward-char -1))
    (if (null (re-search-backward regexp (point-min) t))
	(progn
	  (narrow-to-region start end)
	  (goto-char here)
	  (message (concat "\"" regexp "\" not found.")))
      (save-excursion
	(quotes-show-entry)))
    (setq quotes-search-regexp regexp)))

(defun quotes-entry-beg ()
  "Return the beginning of the current entry as point."
  (save-excursion
    (if (null (re-search-backward
	       quotes-entry-delimiter-regexp (point-min) t))
	(point-min)
      (+ 1 (eol)))))

(defun quotes-entry-end ()
  "Return the end of the current entry as point."
  (save-excursion
    (if (null (re-search-forward
	       quotes-entry-delimiter-regexp (point-max) t))
	(point-max)
      (point))))



;; miscelleania

(defun quotes-get-random ()
  "Return a random quote."

  (with-temp-buffer
     (insert-file (expand-file-name quotes-default-file
                                    quotes-default-directory))
     (let* ((number-of-quotes (quotes-count-entries-to (point-max)))
            (chosen-quote (random number-of-quotes))
            p0)
       (message "Choosing quote number %d ..." chosen-quote)
       (goto-char (point-min))
       (unless (= 0 chosen-quote)
         (re-search-forward quotes-entry-delimiter-regexp (point-max) nil
                            chosen-quote))
       (if (eolp)
           (forward-char))
       ;; Point is now at the beginning of our quote
       (setq p0 (point))
       (if (re-search-forward quotes-entry-delimiter-regexp (point-max) t)
           (beginning-of-line))
       (buffer-substring p0 (point)))))

(defun quotes-insert-random ()
  "Insert a random quote at point."
  (interactive)
  (insert (quotes-get-random)))

(defun quotes-highlight-result (regexp)
  "Highlight the string searched for."
  (while (re-search-forward regexp nil t)
    (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                 'face 'quotes-search-face))
  (goto-char (point-min)))


(defun quotes-create-search-result (regexp)
  "Create the search-result buffer, and set the environment."
  (interactive
   (list (read-string (concat "Quotes Search for"
				  (if quotes-search-regexp
				   (concat " ("
					   quotes-search-regexp
					   ")"))
				  ": "))))
  (let ((linept 0) (prevline 0))
    (if (string= regexp "")
        (setq regexp quotes-search-regexp))
    (setq quotes-search-result-buffer
          (get-buffer-create (generate-new-buffer-name
                              (concat "Quotes search result for "
                                      regexp))))
    (let ((orig-buffer (current-buffer))
          (current-point (point))
          (start (point-min))
          (end (point-max)))
      (widen)
      (goto-char (point-min))
      (while (search-forward-regexp regexp (point-max) t)
        (setq linept (bol))
        (unless (= linept prevline)
          (quotes-invisible-insert-in-buffer (concat
                                    (int-to-string (quotes-current-entry))
                                    " ")
                                   quotes-search-result-buffer)
          (quotes-insert-in-buffer quotes-quote-prompt quotes-search-result-buffer)
          (append-to-buffer quotes-search-result-buffer (bol) (eol))
          (quotes-insert-in-buffer "\n" quotes-search-result-buffer))
        (setq prevline linept))
      (save-excursion
        (set-buffer quotes-search-result-buffer)
        (setq quotes-buffer orig-buffer)
        (if (= (point-max) (point-min))
            (insert "No matches")
          (goto-char (1- (point-max)))
          (delete-char 1))
        (setq buffer-read-only t)
        (goto-char (point-min))
        (setq quotes-search-result-line (make-overlay (bol) (eol)))
        (overlay-put quotes-search-result-line 'face 'quotes-search-result-line-face)
        (quotes-highlight-result regexp)
        (quotes-search-result-mode))
      (narrow-to-region start end)
      (goto-char current-point)
      (if quotes-search-result-p
          (pop-to-buffer quotes-search-result-buffer)
        (switch-to-buffer quotes-search-result-buffer)
        (switch-to-buffer-other-window orig-buffer)
        (setq quotes-search-result-p t)
        (pop-to-buffer quotes-search-result-buffer)))))
;;      (switch-to-buffer-other-window quotes-search-result-buffer))))


(defun quotes-search-result-kill-search-result ()
  "Kill the search-result buffer and window."
  (interactive)
  (save-excursion
    (set-buffer quotes-buffer)
    (setq quotes-search-result-p nil))
  (kill-this-buffer)
  (delete-window))

(defun quotes-search-result-kill-search-result-buffer ()
  "Kill the search-result buffer."
  (interactive)
  (save-excursion
    (set-buffer quotes-buffer)
    (setq quotes-search-result-p nil))
  (kill-this-buffer))

(defun quotes-search-result-current-entry ()
  "Show the quotes entry that is being referred to in the search-result."
  (interactive)
  (let ((pt1 (point)))
    (goto-char (bol))
    (let ((pt (point)))
      (search-forward " ")
      (forward-char -1)
      (let ((n (string-to-int (buffer-substring pt (point)))))
        (save-excursion
          (pop-to-buffer quotes-buffer)
          (quotes-show-entry n)
          (pop-to-buffer quotes-search-result-buffer))))
    (goto-char pt1)))

(defun quotes-search-result-next-entry ()
  "Go to the next line in the search-result buffer,
and show the corresponding quotes entry."
  (interactive)
  (forward-line 1)
  (quotes-search-result-current-entry))

(defun quotes-search-result-previous-entry ()
  "Go to the previous line in the search-result buffer,
and show the corresponding quotes entry."
  (interactive)
  (forward-line -1)
  (quotes-search-result-current-entry))

(defun quotes-insert-in-buffer (str buf)
  "Insert string str in buffer buf"
  (save-excursion
    (set-buffer buf)
    (insert str)))

(defun quotes-invisible-insert-in-buffer (str buf)
  "Insert string str in buffer buf"
  (save-excursion
    (set-buffer buf)
    (let ((pt (point)))
      (insert str)
      (overlay-put (make-overlay pt (point)) 'invisible 'quotes))))

(defun quotes-search-result-highlight-line ()
  (move-overlay quotes-search-result-line (bol) (eol)))

;; utility functions (who hasn't written these ?)

(defun quotes-create-entry (&optional arg)
  "Create a new quotes entry."
  (interactive "P")
  (if arg
      (quotes-backward-create-entry)
    (quotes-forward-create-entry)))

(defun quotes-forward-create-entry ()
  "Create a new quotes entry after the current entry."
  (interactive)
  (widen)
  (re-search-forward quotes-entry-delimiter (point-max) t)
  (quotes-edit-entry)
  (if (= (point) 1)
      (insert "\n"  quotes-entry-delimiter)
    (insert "\n" "\n" quotes-entry-delimiter))
  (backward-char 2)
  (narrow-to-region (point) (point))
  (setq quotes-entry-total (1+ quotes-entry-total))
  (quotes-update-mode-line))

(defun quotes-backward-create-entry ()
  "Create a new quotes entry before the current entry."
  (interactive)
  (widen)
  (re-search-backward quotes-entry-delimiter (point-min) 1)
  (if (> (point) 1)
      (forward-line 1))
  (quotes-edit-entry)
  (insert "\n" quotes-entry-delimiter "\n")
  (backward-char 3)
  (setq quotes-entry-total (1+ quotes-entry-total))
  (narrow-to-region (point) (point))
  (quotes-update-mode-line))


(defun quotes-delete-entry ()
  "Delete the current quote."
  (interactive)
  (if (yes-or-no-p "Do you really want to delete this entry? ")
      (progn
	(let ((current-entry (quotes-current-entry)))
	  (setq buffer-read-only nil)
	  (widen)
	  (kill-region (quotes-entry-beg) (1- (quotes-entry-end)))
	  (delete-char 1)
	  (if (looking-at "\n")
	      (delete-char 1))
	  (setq quotes-entry-total (1- quotes-entry-total))
	  (if (> current-entry quotes-entry-total)
	      (progn
		(forward-char -1)
		(kill-line)
		(beginning-of-line)))
	  (save-buffer)
	  (setq buffer-read-only t)
	  (quotes-show-entry)))))

(defun quotes-edit-entry ()
  "Change to buffer to editable"
  (interactive)
  (local-set-key "q" 'self-insert-command)
  (local-set-key " " 'self-insert-command)
  (local-set-key "b" 'self-insert-command)
  (local-set-key "n" 'self-insert-command)
  (local-set-key "p" 'self-insert-command)
  (local-set-key "s" 'self-insert-command)
  (local-set-key "r" 'self-insert-command)
  (local-set-key "S" 'self-insert-command)
  (setq buffer-read-only nil)
  (quotes-update-mode-line))


(defun quotes-save-changes ()
  "Save the current-buffer"
  (if (buffer-modified-p)
      (if (yes-or-no-p "Do you want to save the changes? ")
	    (save-buffer)
	(revert-buffer)))
  (local-set-key "q" 'quotes-quit)
  (local-set-key "n" 'quotes-next-entry)
  (local-set-key "p" 'quotes-previous-entry)
  (local-set-key "s" 'quotes-search)
  (local-set-key "r" 'quotes-backward-search)
  (local-set-key "S" 'quotes-create-search-result)
  (local-set-key " " 'scroll-up)
  (local-set-key "b" 'scroll-down)
  (setq buffer-read-only t)
  (quotes-update-mode-line))

(defun quotes-save-buffer ()
  "Save the current buffer"
  (interactive)
  (if (buffer-modified-p)
      (if (yes-or-no-p "Do you really want to save the changes? ")
	  (save-buffer)
	(revert-buffer)))
  (local-set-key "q" 'quotes-quit)
  (local-set-key "n" 'quotes-next-entry)
  (local-set-key "p" 'quotes-previous-entry)
  (local-set-key "s" 'quotes-search)
  (local-set-key "r" 'quotes-backward-search)
  (local-set-key "S" 'quotes-create-search-result)
  (local-set-key " " 'scroll-up)
  (local-set-key "b" 'scroll-down)
  (setq buffer-read-only t)
  (quotes-update-mode-line))

(defun quotes-update-mode-line ()
  "Make sure mode-line in the current buffer reflects all changes."
  (setq mode-line-process
        (format " %d/%d" (quotes-current-entry) quotes-entry-total))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))


(defun quotes-quit ()
  "Quit quotes"
  (interactive)
  (quotes-save-changes)
  (kill-buffer nil))

(defun eol ()
  "Return the value point at the end of the current line."
  (save-excursion
    (end-of-line)
    (point)))

(defun bol ()
  "Return the value point at the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (point)))



(provide 'quotes)

;; Local Variables:
;; fill-column: 77
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; End:

;;; quotes.el ends here.
