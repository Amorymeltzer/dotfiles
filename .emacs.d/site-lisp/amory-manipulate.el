;;; amory-manipulate.el --- Manipulate lines and comments, move around

;; Split nicer
(setq split-window-keep-point nil)

;; ;;;;;; ##### FIXME TODO
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; Swap/flip/flop/transpose buffers easily
(require 'transpose-frame)
;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-paragraphs)

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

;; Easily duplicate line
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
  (message "line dup'ed"))

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


(defun yank-and-down ()
  "Yank the text and go down a line."
  (interactive)
  (yank)
  (exchange-point-and-mark)
  (forward-line 1))
(global-set-key (kbd "C-x C-y") 'yank-and-down)


;; Make three follow-mode windows, evenly spaced
(defun all-over-the-screen ()
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
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
	(if (< 1 (count-windows))
	    (progn
	      (window-configuration-to-register ?u)
	      (delete-other-windows))
	  (jump-to-register ?u)))))
(global-set-key (kbd "C-c \\") 'toggle-windows-split)


;; Display line numbers temporarily when calling goto-line
;; Looks messed up if linum-relative is called FIXME TODO
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

;; M-g prefix more useful?
;; (global-set-key "\M-g" 'goto-line)
;; Which line, probably not hugely useful, C-x l more useful
;; (global-set-key (kbd "C-x w") 'what-line)
(global-set-key (kbd "M-g c") 'move-to-column)

;; Move buffers around https://github.com/lukhas/buffer-move
(require 'buffer-move)
;; Other M-g mapping stuff
(global-set-key (kbd "M-g j") 'buf-move-left)
(global-set-key (kbd "M-g k") 'buf-move-down)
(global-set-key (kbd "M-g i") 'buf-move-up)
(global-set-key (kbd "M-g l") 'buf-move-right)


;; Jump to a specific percentage of a buffer
;; https://unix.stackexchange.com/a/29398/43935
(defun goto-percent (percent)
  "Goto PERCENT of buffer.  Whole numbers only please."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

;; Allow jumping between matching parenthesis.
(defun match-paren (arg)
  "Go to the matching parenthesis, brace, or backet if on one; otherwise insert *."
  (interactive "p")
  (cond ((looking-at "\\s\(\\|\\s\{\\|\\s\[") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)\\|\\s\}\\|\\s\]") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "*" 'match-paren)


;; Fix odd highlighting after? ;;;;; ###### FIXME TODO
(defun edit-region (&optional edit-mode)
  "Edit the current region in a separate buffer.
With a prefix arg, change `major-mode' to EDIT-MODE."
  (interactive (list (when current-prefix-arg (ted-read-major-mode))))
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
	(let* ((bol (point-at-bol))
	       (currentline (buffer-substring-no-properties bol (point-at-eol))))
	  (if (string-equal previousline currentline)
	      (delete-region bol (progn (forward-line 1) (point)))
	    (setq previousline currentline)
	    (forward-line 1)))))))


;; M-# to move to windows
;; https://github.com/nschum/window-numbering.el
(require 'window-numbering)
(window-numbering-mode t)

;; Easily switch to specific window using numbers (if >2)
;; https://github.com/abo-abo/ace-window
;; (require 'ace-window)
(autoload 'ace-window "ace-window" "Quickly switch windows" t)
(global-set-key (kbd "C-x o") 'ace-window)
(eval-after-load 'ace-window
  '(progn
     (set-face-attribute 'aw-leading-char-face nil
		    :foreground "deep sky blue"
		    :weight 'bold
		    :height 3.0)))

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


;; Goto last change in buffer
;; https://github.com/camdez/goto-last-change.el
;; FIXME TODO
(autoload 'goto-last-change "goto-last-change" "Set point to the position of
the last change." t)
(global-set-key "\C-x\C-\\" 'goto-last-change)

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
