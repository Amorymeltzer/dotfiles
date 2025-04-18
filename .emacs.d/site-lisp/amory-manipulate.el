;;; amory-manipulate.el --- Manipulate lines and comments, move around -*- lexical-binding: t; -*-

;; Copy to kill ring, exists mainly for the below
;; Can use whole-line-or-region-kill-ring-save for interactive
(defun mark-line-and-copy ()
  "Copy the current line into the kill ring."
  (save-excursion
    (beginning-of-line)
    (push-mark)
    (forward-line 1)
    (kill-ring-save (region-beginning) (region-end)))
  (message "line copied"))

;; Easily duplicate line.  Somehow Emacs only added this in 29.1?!  Not an exact
;; duplicate (hah) of the built-in, but fine.  See also `duplicate-dwim'
(unless (fboundp 'duplicate-line)
  (defun duplicate-line ()
    "Copy this line under it; put point on copy in current column."
    (interactive)
    (let ((start-column (current-column)))
      (save-excursion
	(mark-line-and-copy) ; save-excursion restores mark
	(forward-line 1)
	(yank))
      (forward-line 1)
      (move-to-column start-column))
    (message "line dup'ed")))

;; Duplicate and comment-out
(defun duplicate-and-comment-line ()
  "Copy this line under it; put point on copy in current column."
  (interactive)
  (let ((start-column (current-column)))
    (save-excursion
      (mark-line-and-copy) ; save-excursion restores mark
      (forward-line 1)
      (yank))
    (comment-line 1)
    (move-to-column start-column))
  (message "line commented and dup'ed"))

(global-set-key (kbd "C-c C-y") 'duplicate-line)
(global-set-key (kbd "C-c y") 'duplicate-and-comment-line)
(global-set-key (kbd "C-c j") 'duplicate-dwim)


(defun yank-and-down ()
  "Yank the text and go down a line."
  (interactive)
  (yank)
  (exchange-point-and-mark)
  (forward-line 1))
(global-set-key (kbd "C-x C-y") 'yank-and-down)


;; Make three follow-mode windows, evenly spaced
(defun all-over-the-screen ()
  "Split the window and turn on follow-mode."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (follow-mode t))

;; Better C-x 3
(defun split-window-right-and-move ()
  (interactive)
  (split-window-right)
  (windmove-right))
(global-set-key (kbd "C-x 3") 'split-window-right-and-move)

;; Better C-x 2
(defun split-window-below-and-move ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defalias 'split-window-down-and-move 'split-window-below-and-move)
(global-set-key (kbd "C-x 2") 'split-window-down-and-move)


;; FIXME TODO
;; http://whattheemacsd.com/buffer-defuns.el-03.html
(defun window-swap-split ()
  "Vertical split shows more of each line, horizontal split shows more
lines. This code toggles between them. It only works for frames with exactly
two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c |") 'window-swap-split)

;; Toggle between split windows and a single window.  Could just use
;; winner-mode but this is probably easier
(defun toggle-windows-split()
  "Switch between one window and whatever split of windows we might have in
the frame.  The idea is to maximize the current buffer, while being able
to go back to the previous split of windows in the frame simply by
calling this command again."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
	(if (< 1 (count-windows))
	    (progn
	      (window-configuration-to-register ?u)
	      (delete-other-windows))
	  (jump-to-register ?u)))))
(global-set-key (kbd "C-c \\") 'toggle-windows-split)


;; Display line numbers temporarily when calling `goto-line'.  Should do
;; something to adjust `goto-line-relative' as well? FIXME TODO This looks
;; messed up if linum-relative is called FIXME TODO
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
	(display-line-numbers-mode 1)
	(let ((linenum (read-number "Goto line: ")))
	  (goto-char (point-min))
	  (forward-line (1- linenum))))
    (display-line-numbers-mode -1)))

;; Show relative line number
;; https://github.com/coldnew/linum-relative
(autoload 'linum-relative-toggle "linum-relative" "Toggle between
linum-relative and linum" t)
;; Use `display-line-numbers-mode' as the backend, rather than the (as of 29.1)
;; deprecated `linum-mode'
(setq linum-relative-backend 'display-line-numbers-mode)

;; Jump to a specific percentage of a buffer
;; https://unix.stackexchange.com/a/29398/43935
(defun goto-percent (percent)
  "Goto PERCENT of buffer.  Whole numbers only please."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))


;; Fix odd highlighting after? ;;;;; ###### FIXME TODO
;; Maybe just use something like edit-indirect
(defun edit-region (&optional edit-mode)
  "Edit the current region in a separate buffer.
With a prefix arg, change `major-mode' to EDIT-MODE."
  (interactive)
  (clone-indirect-buffer nil t)
  (narrow-to-region (region-beginning) (region-end))
  (shrink-window-if-larger-than-buffer)
  (when edit-mode (funcall edit-mode)))

(defun create-buffer (name)
  "Create a new buffer with NAME and switch to it.

The buffer starts in `fundamental-mode'."
  (interactive "BBuffer name:")
  (prog1 (switch-to-buffer (generate-new-buffer name))
    (fundamental-mode)))


(defun uniq-lines (beg end)
  "Remove consecutive duplicate lines in region BEG to END.

When there's no active region, act on the buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (goto-char beg)
    (let ((previousline nil))
      (while (and (< (point) end)
		  (not (eobp)))
	(let* ((beg (line-beginning-position))
	       (currentline (buffer-substring-no-properties beg (line-end-position))))
	  (if (string-equal previousline currentline)
	      (delete-region beg (progn (forward-line 1) (point)))
	    (setq previousline currentline)
	    (forward-line 1)))))))


;; Renumber list in region, from https://www.emacswiki.org/emacs/RenumberList#h5o-1
;; Using regex is neat, but I can never remember \,() for executing lisp, so put
;; `\,(1+ \#)' in the docs.
(defun renumber-list (start end &optional num)
  "Renumber the list items in the current START..END region.
    If optional prefix arg NUM is given, start numbering from that number
    instead of 1.  Can also do a regex, e.g. `\\,(1+ \\#)'."
  (interactive "*r\np")
  (save-excursion
    (goto-char start)
    (setq num (or num 1))
    (save-match-data
      (while (re-search-forward "^[0-9]+" end t)
	(replace-match (number-to-string num))
	(setq num (1+ num))))))

;; Control other windows easier
(global-set-key (kbd "C-M-t") 'scroll-other-window-down)
(defalias 'scroll-other-window-up 'scroll-other-window)
;; Mess around with defaults
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)
(global-set-key (kbd "M-<") 'beginning-of-buffer-other-window)
(global-set-key (kbd "M->") 'end-of-buffer-other-window)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)


(defun last-command-other-window()
  "Execute last command in other window."
  (interactive)
  (unless (memq last-command '(last-command-other-window eval-defun))
    (other-window 1)
    (funcall last-command)
    (other-window 1)))


(defun smart-beginning-of-line (&optional lineoffset)
  "Move the point to the first non-white character of the current
line. If the point is already there, move to the beginning of the
line instead. With argument LINEOFFSET not nil or 1, behave like
`beginning-of-line' instead."
  (interactive "^p")
  (setq lineoffset (or lineoffset 1))
  (if (= lineoffset 1)
      (let ((oldpos (point)))
	(back-to-indentation)
	(when (= oldpos (point))
	  (move-beginning-of-line 1)))
    (move-beginning-of-line lineoffset)))
(global-set-key (kbd "C-a") 'smart-beginning-of-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some commands from http://www.emacswiki.org/emacs/basic-edit-toolkit.el
;; Not things I'll use that often but hey why not
(defun open-newline-above (arg)
  "Move to the previous line (like vi) and then opens a line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))

(defun open-newline-below (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (call-interactively 'next-line arg)
  (if (not (member major-mode '(haskell-mode org-mode literate-haskell-mode)))
      (indent-according-to-mode)
    (beginning-of-line)))


;; Move/affect comments
(defun comment-part-move-up (n)
  "Move comment part up."
  (interactive "p")
  (comment-part-move (or (- n) -1)))

(defun comment-part-move-down (n)
  "Move comment part down."
  (interactive "p")
  (comment-part-move (or n 1)))

(defun comment-part-move (&optional n)
  "Move comment part."
  (or n (setq n 1))
  (let (cmt-current cmt-another cs-current cs-another)
    ;; If current line have comment, paste it.
    (setq cmt-current (comment-paste))
    (when cmt-current
      (setq cs-current (current-column)))
    ;; If another line have comment, paste it.
    (forward-line n)
    (setq cmt-another (comment-paste))
    (when cmt-another
      (setq cs-another (current-column)))
    ;; Paste another comment in current line.
    (forward-line (- n))
    (when cmt-another
      (if cs-current
	  (move-to-column cs-current t)
	(end-of-line))
      (insert cmt-another))
    ;; Paste current comment in another line.
    (forward-line n)
    (when cmt-current
      (if cs-another
	  (move-to-column cs-another t)
	(end-of-line))
      (insert cmt-current))
    ;; Indent comment, from up to down.
    (if (> n 0)
	(progn                          ;comment move down
	  (forward-line (- n))
	  (if cmt-another (comment-indent))
	  (forward-line n)
	  (if cmt-current (comment-indent)))
      (if cmt-current (comment-indent)) ;comment move up
      (save-excursion
	(forward-line (- n))
	(if cmt-another (comment-indent))))))

(defun comment-copy (arg)
  "Copy the first comment on this line, if any.
With prefix ARG, copy comments on that many lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_ (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
	(when cs
	  (goto-char cs)
	  (skip-syntax-backward " ")
	  (setq cs (point))
	  (comment-forward)
	  (kill-ring-save cs (if (bolp) (1- (point)) (point)))
	  (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun comment-paste ()
  "Paste comment part of current line.
If have return comment, otherwise return nil."
  (let (cs ce cmt)
    (setq cs (comment-on-line-p))
    (if cs                              ;If have comment start position
	(progn
	  (goto-char cs)
	  (skip-syntax-backward " ")
	  (setq cs (point))
	  (comment-forward)
	  (setq ce (if (bolp) (1- (point)) (point))) ;get comment
					;end position
	  (setq cmt (buffer-substring cs ce))        ;get comment
	  (kill-region cs ce)                        ;kill region between
					;comment start and end
	  (goto-char cs)                             ;revert position
	  cmt)
      nil)))

(defun comment-on-line-p ()
  "Whether have comment part on current line.
If have comment return COMMENT-START, otherwise return nil."
  (save-excursion
    (beginning-of-line)
    (comment-search-forward (line-end-position) t)))

(defun comment-dwim-next-line (&optional reversed)
  "Move to next line and comment dwim.
Optional argument REVERSED default is move next line, if reversed
is non-nil move previous line."
  (interactive)
  (if reversed
      (call-interactively 'previous-line)
    (call-interactively 'next-line))
  (call-interactively 'comment-dwim))

(defun comment-dwim-prev-line ()
  "Move to previous line and comment dwim."
  (interactive)
  (comment-dwim-next-line 't))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'amory-manipulate)

;;; amory-manipulate.el ends here
