;;;;;;;;; BEGIN
;; (setq emacs-load-start-time (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations                                                                  ;;
;;                                                                            ;;
;; "Show me your ~/.emacs and I will tell you who you are." -Bogdan Maryniuk  ;;
;;                                                                            ;;
;; "Emacs is like a laser guided missile. It only has to be slightly          ;;
;; mis-configured to ruin your whole day." -Sean McGrath                      ;;
;;                                                                            ;;
;; "While any text editor can save your files, only Emacs can save your soul.";;
;; -Per Abrahamsen                                                            ;;
;;                                                                            ;;
;; "Anyone with a 4-line .emacs file is suspicious."                          ;;
;; -Dino Chiesa                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Notes

;; Calendar mode
;; http://www.emacswiki.org/emacs/CalendarMode
;; M-x calendar, then p C-h for commands beginning with p, try also g
;; C-spc, move, M-= for time between dates
;;C-x ], Move ahead a year.
;;9 M-}, Find out what day is 9 months from the day at point.
;;. 177 C-b, Find out what day was 177 days ago.
;;8 C-n, Move ahead 8 weeks
;;g w, Go to a given week
;;3 d, show diary for 3 days

;; M-x customize-group, then option, gives customizable things for .emacs
;; F1 or C-h for help, then b for all bindings
;; Anything, then C-h for all that begin with it (C-x, C-c)
;; C-h c command, explain what that key command does (C-h c C-x C-c)
;; C-h f function, explain what that function does (C-h f calendar)
;; Or M-x where-is function
;; C-h a regex, search for that in commands ie anything with rot13
;; C-h v or M-x describe-variable for setq variables
;; C-x z, repeat last command
;; M-x man, man in emacs
;; C-j, newline
;; C-m, newline-and-indent
;; C-h l, last inputs
;; C-u M-!, insert shell command into buffer
;; M-q, justify a paragraph
;; C-x 8 C-h, list of special characters
;; sort-lines

;; http://www.emacswiki.org/emacs/EmacsNewbieKeyReference
;; http://www.rgrjr.com/emacs/emacs_cheat.html
;; http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enter debugger on error
(setq debug-on-error t)

;; When I was a child, I spake as a child,
;; I understood as a child, I thought as a child:
;; but when I became a man, I put away childish things.
;;   -- 1 Corinthians, 13:11
;; No menu bar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Load everybody proper-like
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; Prefer newer files even if not .elc
(setq load-prefer-newer t)

;; It'sa me
(setq user-mail-address "Amorymeltzer@gmail.com"
      user-full-name "Amory Meltzer"
      hostname (replace-regexp-in-string
		"\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" ""
		(with-output-to-string (call-process "hostname"
						     nil standard-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete
;; http://cx4a.org/software/auto-complete/manual.html#Configuration
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/ac-dict")

;; Characters entered before started, up=efficient, down=slower
;; (setq ac-auto-start 4)
;; Turn auto menu off
;; (setq ac-auto-start nil)

;; Change colors
;; (set-face-attribute 'ac-candidate-face nil
;;		    :background "lightgray" :underline "darkgray")
;; (set-face-attribute 'ac-selection-face nil
;;		    :background "steelblue")
;;
;;
;; ac-html mode, from https://github.com/cheunghy/ac-html
(require 'ac-html)
(add-hook 'html-mode-hook 'ac-html-enable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default M-/ is dabbrev-expand, but this is broken? ;;;;; ###### FIXME TODO
;; (global-set-key "\M-/" 'auto-complete)
;; Hippie expand expands lines, kind of like above but indiscriminate
(global-set-key "\M-?" 'hippie-expand)

;; Unexpand.  This only works if you use hippie, I want a
;; dabbrev-unexpand, sort of like unexpand-abbrev
(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))
(global-set-key "\M-\"" 'hippie-unexpand)

;; Sometimes hippie is a little TOO hip.  Reorder the list so that expand-line
;; and expand-list come much, much later, definitely after expand-dabbrev
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol
					 try-expand-line
					 try-expand-list))

;; Use ido for hippie-expand via C-c / -- not ideal.  Seems to swallow any
;; opening paren? ;;;;;; ##### FIXME TODO
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my-hippie-expand-completions)
	(last-command last-command)
	(buffer-modified (buffer-modified-p))
	(hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
	       (funcall hippie-expand-function nil)
	       (setq last-command 'my-hippie-expand-completions)
	       (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer,
    ;; however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they
    ;; are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defmacro my-ido-hippie-expand-with (hippie-expand-function)
  "Generate an interactively-callable function that offers ido-based completion
    using the specified hippie-expand function."
  `(call-interactively
    (lambda (&optional selection)
      (interactive
       (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
	 (if options
	     (list (ido-completing-read "Completions: " options)))))
      (if selection
	  (he-substitute-string selection t)
	(message "No expansion found")))))

(defun my-ido-hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (my-ido-hippie-expand-with 'hippie-expand))
(global-set-key (kbd "C-c /") 'my-ido-hippie-expand)


;; Expand region highlights intelligently
;; https://github.com/magnars/expand-region.el
(require 'expand-region)
(global-set-key (kbd "M-m") 'er/expand-region)
(global-set-key (kbd "M-M") 'er/contract-region)
;; Possible keybindings?
;; (global-set-key (kbd "C-+") 'er/expand-region)


;; Mouse stuff doesn't work in OSX terminal
;; (setq focus-follows-mouse 1
;;       mouse-autoselect-window 1)


;; Protect special buffers
(require 'keep-buffers)
(keep-buffers-mode 1)

;; Subword mode (consider CamelCase chunks as words)
(global-subword-mode 1)


;; Whinewhinewhine
;; (load-library "whine")
;; (whinify)


;; UTF-8 always, please
(setq locale-coding-system 'utf-8) ; please
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
(set-language-environment "UTF-8")
;; (setq-default buffer-file-coding-system 'utf-8-unix)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flymake stuff
;; Really should use flycheck if emacs >= 24 and revert back to flymake
;; otherwise Require flymake. ;;;;;; ##### FIXME TODO
(require 'flymake)

;; Deal with stupid jshint/javascript stuff
;; I really need to migrate to flycheck
(delete '("\\.js\\'" flymake-javascript-init) flymake-allowed-file-name-masks)

;; Static analysis can be slow, so only run flymake if I've not been typing for 5 seconds.
;; It will still run on save or hitting return.
(setq flymake-no-changes-timeout 5)

;; Disable in-place checking, and tell it to use ~/.emacs.d/tmp/ for the temp files.
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq flymake-run-in-place nil)

;; Only need these two if you plan to debug Flymake.
(setq flymake-log-file-name (concat temporary-file-directory "flymake.log"))
(setq flymake-log-level -1)

;; Tune how many checks can run in parallel, default of 4 should be fine.
;;(setq flymake-max-parallel-syntax-checks 1)

;; I want to see at most the first 4 errors for a line.
;;(setq flymake-number-of-errors-to-display 4)

;; I want to see all errors for the line.
(setq flymake-number-of-errors-to-display nil)

;; Flymake cursor to show in buffer If you're a TTY emacs user, flymake-cursor
;; is a must-have.
;; (require 'flymake-cursor)
;; https://github.com/illusori/emacs-flymake-cursor
(eval-after-load 'flymake '(require 'flymake-cursor))
(setq flymake-cursor-error-display-delay 0)

;;Turn on automatically
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; flymake-perlcritic stuff.  If flymake_perlcritic isn't in your $PATH you'll
;; need to give the full path here
;; https://github.com/illusori/emacs-flymake-perlcritic
(setq flymake-perlcritic-command "~/bin/flymake_perlcritic")
(require 'flymake-perlcritic)
(setq flymake-perlcritic-severity 2)

;; C-c C-v to go to next error
(global-set-key (kbd "C-c '") 'flymake-goto-next-error)
;; C-c C-C to go to prev error
(global-set-key (kbd "C-c ;") 'flymake-goto-prev-error)

;; Flymake faces, do the same for cperl's checking faces?
;; Redundant to cperl-mode??? ;;;;;; ##### FIXME TODO
(set-face-attribute 'flymake-errline nil
		    :foreground "black")
(set-face-attribute 'flymake-infoline nil
		    :foreground "black")
(set-face-attribute 'flymake-warnline nil
		    :foreground "black")
;; Should show all but doesnt? ;;;;;; ##### FIXME TODO
(setq flymake-number-of-errors-to-display nil)

;; Perltidy, also exists as stand-alone command line program
;; Uses ~/.perltidyrc
(require 'perltidy)

;; Whitespace
(require 'whitespace)
;; Turn on globally, probably if better just for programming modes
;; (global-whitespace-mode t)
(add-hook 'prog-mode-hook 'whitespace-mode)
;; Highlight tabs, spaces, lines, parts oflines >80 chars
;; http://www.emacswiki.org/cgi-bin/wiki/EightyColumnRule
(setq whitespace-style '(face empty lines-tail trailing space-before-tab))

;; Not needed in emacsen >= 23 because of lines-tail above
;; Auto-color lines over 80 in length .\{81\}
;; M-x highlight-lines-matching-regex or C-x w l
;; Unhighlight with unhighlight-regex or C-x w r
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-green-b)))
;; (add-hook 'perl-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-red-b)))


;;; God there are a lot of ways to do this, need to pick the best one
;; ;;;;;; ###### FIXME TODO
;; Highlight TODOs, FIXMEs, etc.
;; https://github.com/lewang/fic-mode
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;; Really should figure out and group font-lock stuff ;;;;; #### FIXME TODO
;; Regexp color for backslash, and... escapes?
(set-face-attribute 'font-lock-regexp-grouping-backslash nil
		    :foreground "#ff1493")
(set-face-attribute 'font-lock-regexp-grouping-construct nil
		    :foreground "#ff8c00")

;; highlight specified words
;; (defun my/add-watchwords ()
;;   (font-lock-add-keywords
;;    nil '(("\\_<\\(FIXME\\|TODO\\|XXX\\|@@@\\)\\_>"
;;           1 '((:foreground "pink") (:weight bold)) t))))
;; (add-hook 'prog-mode-hook 'my/add-watchwords)

;; https://github.com/dgutov/highlight-escape-sequences
;; (require 'highlight-escape-sequences)
;; (setq hes-simple-modes '(emacs-lisp-mode))
;; (hes-mode)


;; (require 'applescript-mode)


;; Make life easier, in all lisps
(define-key lisp-mode-shared-map (kbd "C-x e") 'eval-buffer)
(define-key lisp-mode-shared-map (kbd "C-x C-e") 'eval-buffer)


;; Use buffer name as frame title, only works in window-system
;; (setq frame-title-format "%b - emacs")
;; (setq frame-title-format '(buffer-file-name "%f" ("%b")))
;; (let ((name (assq 'name default-frame-alist)))
;;   (when (consp name)
;;     (setcdr name ())))

;; (modify-frame-parameters (selected-frame) '((name)))

;; create a backup file directory
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))


;; Save every on inputs and idle
(setq auto-save-interval 300) ; default 100
(setq auto-save-timeout 90) ; default 30

;; Versions
(setq delete-old-versions t
      backup-by-copying t
      kept-new-versions 20 ; 6
      kept-old-versions 3
      version-control t)

;; Allow diff/ediff with backups
;; Do something to alias diff, etc. ;;;;; ######## TODO FIXME
;; https://github.com/emacsmirror/pick-backup
(require 'pick-backup)

;; Default -c, similar to u
(setq diff-switches "-u -w")

;; Saner ediff?
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Options to pass to ls, default just -l
(setq list-directory-verbose-switches "-lh")

;; Updates buffer if file is changed elsewhere, DON'T save until the
;; other process has finished writing!!!  Set manually!
;; (auto-revert-tail-mode 1)

;; Recent files (~/.emacs.d/.recentf)
;; Pointless if saving buffers as below?
(require 'recentf)
(recentf-mode 1)
(setq recentf-save-file "~/.emacs.d/recentf")
;; Uses ~ instead of full path
(setq recentf-filename-handlers (quote abbreviate-file-name))
;; Same as above?
;; (setq recentf-menu-filter (quote recentf-relative-filter))
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 30)

;; Maybe just use ido-choose-from-recentf instead 'cause that's the point of ido
(global-set-key (kbd "C-x f") 'recentf-open-files)

;; Exclude boring files
(add-to-list 'recentf-exclude "\\.el.gz\\'")
(add-to-list 'recentf-exclude "\\.elc\\'")
(add-to-list 'recentf-exclude "\\/opt\\/local\\/share\\/emacs.*\\'")
(add-to-list 'recentf-exclude "\\.ido.last\\'")
(add-to-list 'recentf-exclude "\\.smex-items\\'")
(add-to-list 'recentf-exclude "\\.recentf\\'")

;; Cleanup timer
;; (setq recentf-auto-cleanup 300)
(setq recentf-auto-cleanup 'never)

;; From http://www.emacswiki.org/emacs/recentf-ext.el:
;;; [2009/03/01] (@* "`recentf' as most recently USED files")
;; (defun recentf-push-buffers-in-frame ()
;;   (walk-windows
;;    (lambda (win)
;;      (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
;;        (and bfn (recentf-add-file bfn))))))
;; (add-to-list 'window-configuration-change-hook 'recentf-push-buffers-in-frame)

;; Does above (?) and marks most recently visited file as "most recent"
;; (require 'recentf-ext)

;; Rules for submenus
;; ;;;;;;;; ####### FIXME TODO
;; (setq recentf-arrange-by-rules-min-items 0
;;       recentf-arrange-by-rules-others nil
;;       recentf-menu-filter 'recentf-arrange-by-rule
;;       recentf-menu-title "recentF"
;;       )
;; (add-to-list 'recentf-arrange-rules (quote (("Perl files (%d)" ".\\.pl\\'"))))
;; (add-to-list 'recentf-arrange-rules (quote (("CGI files (%d)" ".\\.cgi\\'"))))

;; Ensure cgi ends up in perl mode
(add-to-list 'auto-mode-alist '("\\.[cG][gG][iI]$" . perl-mode))


;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
	     '("\\.\\(markdown\\|mdml\\|mkdn\\|text\\|md\\)\\'" . markdown-mode))

;; Should add some more here
;; Also need to make interactives for bold, italics, headers, etc
(eval-after-load "markdown-mode"
  '(progn
     ;; key bindings
     (define-key markdown-mode-map (kbd "C-M-f") 'forward-symbol)
     (define-key markdown-mode-map (kbd "C-M-b") 'editutil-backward-symbol)
     (define-key markdown-mode-map (kbd "C-M-u") 'my/backward-up-list)

     (define-key markdown-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
     (define-key markdown-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
     (define-key markdown-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
     (define-key markdown-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
     (define-key markdown-mode-map (kbd "C-c C-u") 'outline-up-heading)))

;; Generate README.md markdown from header of elisp file for github
;; checkdoc might be useful beforehand
;; https://github.com/thomas11/md-readme
(require 'md-readme)

(defun markdown-linkify ()
  "Make region or current word into a link to itself."
  (interactive)
  (let* ((bounds
	  (if (and mark-active transient-mark-mode)
	      (cons (region-beginning) (region-end))
	    (bounds-of-thing-at-point 'url)))
	 (beg (car bounds))
	 (end (cdr bounds))
	 (url (buffer-substring beg end))
	 (newtext (format "[%s](%s)" url url)))
    (delete-region beg end)
    (insert newtext)))

;; Convert html to markdown
;; https://github.com/Bruce-Connor/html-to-markdown
(autoload 'html-to-markdown "html-to-markdown" "Convert current
buffer or region to mardown and display it in a separate window."
  t)

;; htmlize-buffer/file
;; https://github.com/emacsmirror/htmlize
(require 'htmlize)

(defun linkify-region-html (start end)
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert "<a href=\"\">" str "</a>")))

;; Tidy mode to judge your html
;; http://www.emacswiki.org/emacs/tidy.el
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy_parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

(defun tidy-then-indent ()
  "Tidy leaves a buffer looking flat, so indent after use"
  (interactive)
  (tidy-buffer)
  (indent-buffer))

(autoload 'tidy-then-indent "tidy" "Run Tidy HTML parser then indent the
current buffer" t)
(defalias 'tidy-indent 'tidy-then-indent)


;; If a region selected, typing replaces it
(delete-selection-mode t)

;; Wrap-region mode, wrap with quotes or braces
;; https://github.com/rejeep/wrap-region.el
(require 'wrap-region)
;; (wrap-region-mode t)
(wrap-region-global-mode t)
(wrap-region-add-wrappers
 '(
   ("/" "/" nil ruby-mode)
   ("/* " " */" "#" (java-mode javascript-mode css-mode))
   ("`" "`" nil (markdown-mode ruby-mode))))


;; Print a buffer.  Requires htmlize and coral.  See
;; http://www.emacswiki.org/emacs/MacPrintMode
;; (when (require 'mac-print-mode nil t)
;;   (mac-print-mode 1))


(defun visit-most-recent-file ()
  "Visits the most recently open file in `recentf-list' that is not already being visited."
  (interactive)
  (let ((buffer-file-name-list (mapcar 'buffer-file-name (buffer-list)))
	most-recent-filename)
    (dolist (filename recentf-list)
      (unless (memq filename buffer-file-name-list)
	(setq most-recent-filename filename)
	(return)))
    (find-file most-recent-filename)))
(global-set-key (kbd "C-x C-r") 'visit-most-recent-file)

;; Reveal file or folder in finder
;; https://github.com/kaz-yos/elisp/blob/master/reveal-in-finder.el
(require 'reveal-in-finder)
(global-set-key (kbd "C-c z") 'reveal-in-finder)

;; Save a list of open files in ~/.emacs.d/.emacs.desktop(.lock)
(desktop-save-mode t)
;; Automatically unless nonexistant
(setq desktop-save 'ask-if-new
      desktop-restore-eager t ; Load this many buffers, rest when lazy
      ;; desktop-restore-eager t ; Load this many buffers, rest when lazy
      desktop-load-locked-desktop 'ask ; Just as a reminder
      desktop-base-file-name "emacs.desktop"
      ;; Don't try to save if file is locked, I'll always be quick
      desktop-not-loaded-hook (quote (desktop-save-mode-off))
      desktop-path (quote ("~/.emacs.d/" "~"))) ; Just in case


;; Load the desktop *after* all init stuff is done.  Does what???
;; ;;;;;; ####### FIXME TODO
;; (eval-after-load "init"
;;   '(progn
;;      ;; (desktop-read)
;;      (desktop-save-mode 1)))

;; Save a bunch of variables to the desktop file.  For lists specify the len
;; of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 50)
		(file-name-history        . 100)
		(grep-history             . 30)
		(compile-history          . 30)
		(minibuffer-history       . 60)
		(query-replace-history    . 50)
		(read-expression-history  . 30)
		(regexp-history           . 60)
		(regexp-search-ring       . 30)
		(search-ring              . 30)
		(shell-command-history    . 50)
		tags-file-name
		register-alist)))

;; I'd like the desktop to be automatically saved whenever Emacs has been idle
;; for some time:
(if completion-ignore-case
    (defvar desktop-save-time
      (run-with-idle-timer 60 t (lambda ()
				  (garbage-collect)
				  (wm-save-desktop-with-message)))
      "idle-timer (see \\[run-with-idle-timer]) to save desktop \\[cancel-timer] to cancel it."))
(defun wm-save-desktop-with-message ()
  (interactive)
  (desktop-save desktop-dirname)
  (message "Desktop saved at %s, memory-limit %x."
	   (wm-format-time-string "%T %a %d %b %y")
	   (memory-limit)))


(defun wm-format-time-string (format &optional time)
  "Like format-time-string except that %a gives a 2-char abbreviation."
  ;; Under GNU 19.34 (format-time-string "%2a") returns "2a".
  (callf or time (current-time))
  (if (string-match "\\(\\`\\|[^%]\\|\\(%%\\)+\\)%a" format)
      (let ((i (match-end 0)))
	(concat (format-time-string (substring format 0 (- i 2)) time)
		(substring (format-time-string "%a" time) 0 2)
		(wm-format-time-string (substring format i) time)))
    (format-time-string format time)))

;; (defvar wm-desktop-save-timer
;;   (run-with-idle-timer
;;    60 ;take the action after idle for this many seconds
;;    t  ;repeat (i.e. don't just do it the first time Emacs has been idle)
;;    (lambda ()
;;      (garbage-collect)
;;        ;bundle with desktop timer for performance reasons
;;      (wm-save-desktop-with-message)))
;;   "Timer object causing desktop saving after idle time.
;; This can be an arg of some functions (apropos \"timer\").")

;; Open at last place visited in a file
;; Any overlap with desktop or persistency?
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")


;; Save clipboard strings into kill ring before replacing them.
(setq save-interprogram-paste-before-kill t)

;; Bookmarks are persistent and they have names; not markers. Bookmarked
;; positions can also be relocated (found) if they move slightly because of
;; text changes.

;; To navigate to a bookmark (linking to a file or directory), just press
;; `C-x r b'. You'll be prompted for the bookmark name, and it will open that
;; file or directory.
;; C-x r l for list
;; C-x r m to make a new one
;; C-x r d to delete
(global-set-key "\C-xrd" 'bookmark-delete)
;; where to save the bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks")
;; each command that sets a bookmark will also save your bookmarks
(setq bookmark-save-flag 1)


;; scrolling
(setq
 scroll-margin 0                        ;; do smooth scrolling
 scroll-conservatively 100000		;; ... the defaults ...
 scroll-up-aggressively 0.0             ;; ... are very ...
 scroll-down-aggressively 0.0           ;; ... annoying
 scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

;; Keep cursor away from edges when scrolling up/down
;; https://github.com/aspiers/smooth-scrolling/
(require 'smooth-scrolling)
(setq smooth-scroll-margin 3) ;; default 10

;; Modeline customizations
;; Time
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode t)

;; Battery percentage
(display-battery-mode t)
(setq battery-update-interval 180) ;; Default 60s

;; Show column-number, size in the mode line
(column-number-mode 1) ; performance hit?
(size-indication-mode t)

;; case INsensitive search.  Once turned off somehow?
(setq-default case-fold-search t)

;; Regex search... Always
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)

;; Alias query functions so all the replace functions start with
;; "replace" like replace-regexp.  Not really sure what the difference
;; here is as compared to rr and qrr.
(defalias 'replace-query 'query-replace)
(defalias 'replace-query-regexp 'query-replace-regexp)
(defalias 'replace-query-regexp-eval 'query-replace-regexp-eval)

;; Visual feedback for regex replace
;; Should I alias the above to this?  Maybe. ;;;;;; ####### FIXME TODO
;; Also setup isearch, isearch regexp for this style?  Probably
;; Replace is fast, query asks
;; Only works DOWN a bufer
;; Sourced by below -steroids
;; https://github.com/benma/visual-regexp.el
;; (require 'visual-regexp)
;; Python-style regexp instead of stupid-ass friggin' crazy escapes
;; https://github.com/benma/visual-regexp-steroids.el
(require 'visual-regexp-steroids)

(progn
  (defalias 'visual-regexp-replace 'vr/replace)
  (defalias 'replace-visual-regexp 'vr/replace)
  (defalias 'visual-regexp-query-replace 'vr/query-replace)
  (defalias 'replace-query-visual-regexp 'vr/query-replace)

  (defalias 'rr 'vr/replace)
  (defalias 'qrr 'vr/query-replace))

;; I will never use these, but might as well usurp them as well
(global-set-key (kbd "M-%") 'vr/replace)
(global-set-key (kbd "C-M-%") 'vr/query-replace)

;; Potential keybindings?
;; (global-set-key (kbd "C-c r") 'vr/replace)
;; (global-set-key (kbd "C-c q") 'vr/query-replace)

;; Display number of matches in modeline for isearch.  Need to use anzu instead
;; of visual regexp since it does much the same thing ;;;;;;; ##### FIXME TODO
;; (global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
;; (global-set-key (kbd "C-x M-%") 'anzu-query-replace-at-cursor)
;; (global-set-key (kbd "C-x %") 'anzu-replace-at-cursor-thing)
;; https://github.com/syohex/emacs-anzu
(require 'anzu)
(global-anzu-mode t)
(setq anzu-deactivate-region t
      anzu-search-threshold 1000
      anzu-replace-to-string-separator " => ")

;; Debold
(set-face-attribute 'anzu-mode-line nil :weight 'normal)
;; Black text is more readable
(set-face-attribute 'isearch nil :foreground "black")
(set-face-attribute 'lazy-highlight nil :foreground "black")

;; Jump to word (char, line, with C-u, C-u C-u)
;; https://github.com/winterTTr/ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Reorder so line with one prefix, char with two
(setq ace-jump-mode-submode-list
      '(ace-jump-word-mode        ; first one always maps to: C-c SPC
	ace-jump-line-mode        ; second one always maps to: C-u C-c SPC
	ace-jump-char-mode))      ; third one always maps to: C-u C-u C-c SPC

;; Jump back with C-x spc
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(add-to-list 'debug-ignored-errors "\\[AceJump\\].*")

;; Allow scrolling (not off-screen) during search
;; Kind of weird if going up?
(setq isearch-allow-scroll t
      ;; Add search commands to history, allow resuming
      isearch-resume-in-command-history t)

;; Grab the full word for searching
(defun isearch-yank-symbol ()
  (interactive)
  (isearch-yank-internal (lambda () (forward-symbol 1) (point))))

(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


;; Utilize system's trash can
(setq-default delete-by-moving-to-trash t
	      trash-directory "~/.trash/emacs")

;; ignore case when completing, including buffers and filenames
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Completion in mini-buffer
(icomplete-mode t)

;; minibuffer window expands vertically as necessary to hold the text that you
;; put in the minibuffer
(setq resize-mini-windows t)

;; Save minibuffer history
(savehist-mode 1)
(setq
 savehist-autosave-interval 60 ; Default 300
 savehist-ignored-variables (quote (ido-cur-list)) ; Not sure what do...
 savehist-additional-variables '(search-ring regexp-search-ring))

;; Long history, remove dupes
(setq history-length 1000
      history-delete-duplicates t)


;; Jiggle a bit when moving from buffer to buffer.  Opens up the compilation
;; log all the time, which sucks.  Hence, this is turned off
;; (require 'jiggle)
;; (jiggle-mode 1)
;; (setq jiggle-how-many-times 5)
;; Also jiggle when movings through searches
;; (jiggle-searches-too 1)


;; Make M-w a bit like C-w.  Still kind of weird?
;; Overlaps with something else ;;;;;; ##### FIXME TODO
(require 'whole-line-or-region)
(whole-line-or-region-mode 1)
(global-set-key (kbd "M-w") 'whole-line-or-region-kill-ring-save)

;;;;;;;;;;;;;;;;;;;
;; IDO, Interactively Do Things
;; interactively do things makes better buffer/find-file menus
;; C-s, C-r cycle
;; C-f, C-b switch to file or buffer mode
;; C-j new file with entered text

;; ido-hacks.el is better
;; https://github.com/scottjad/ido-hacks
(require 'ido-hacks)
(ido-mode t) ; is this the best place for this?
(ido-hacks-mode 1)
;; Complete space/hyphen in ido like M-x
;; https://github.com/doitian/ido-complete-space-or-hyphen
(require 'ido-complete-space-or-hyphen)

;; Fuzzy-ish matching
(setq ido-enable-flex-matching t)
;; Better fuzzy matching, does the above need to be turned on?
;; MUCH slower, plus highlighting error? ;;;;;;; ####### TODO FIXME
;; (require 'ido-better-flex)
;; (ido-better-flex/disable)
;; Ditto above
;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; (setq ido-use-faces nil)

;; Not exactly sure but it sounds nice, right?  Use ido-ubiquitous
(setq ido-everywhere t)
;; https://github.com/DarwinAwardWinner/ido-ubiquitous
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

;; Use ido for yes-or-no
;; https://github.com/DarwinAwardWinner/ido-yes-or-no
(require 'ido-yes-or-no)
(ido-yes-or-no-mode t)

;; Vertical mode, kind of awkward
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode 1)

;; ido-at-point, better completion-at-point
;; https://github.com/katspaugh/ido-at-point
(require 'ido-at-point)
(ido-at-point-mode)
(setq ido-at-point-fuzzy t)

;; Specify save file in ~/.emacs.d/ folder
(setq ido-save-directory-list-file "~/.emacs.d/ido.last"
      ;; Kind of keeps buffer names around via recentf in case things get closed
      ido-use-virtual-buffers t
      ;; Probably good/useful
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ;; Use / to enter directory if it's first, not just unique
      ido-enter-matching-directory 'first)
;; Not exactly sure but it sounds nice, right?
(ido-load-history)
;; No .DS files
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Sort ido filelist by modified time instead of alphabetically, buries .
(defun ido-sort-mtime ()
  (setq ido-temp-list
	(sort ido-temp-list
	      (lambda (a b)
		(time-less-p
		 (sixth (file-attributes (concat ido-current-directory b)))
		 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
	      (lambda (x) (and (char-equal (string-to-char x) ?.) x))
	      ido-temp-list))))

(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;; ~ in ido returns to home directory
(add-hook 'ido-setup-hook
	  (lambda ()
	    ;; Go straight home
	    (define-key ido-file-completion-map
	      (kbd "~")
	      (lambda ()
		(interactive)
		(if (looking-back "/")
		    (insert "~/")
		  (call-interactively 'self-insert-command))))))

;; C-k to kill buffer, C-b to bury it
;; http://endlessparentheses.com/Ido-Bury-Buffer.html
(add-hook
 'ido-setup-hook
 (defun endless/define-ido-bury-key ()
   (define-key ido-completion-map
     (kbd "C-b") 'endless/ido-bury-buffer-at-head)))

(defun endless/ido-bury-buffer-at-head ()
  "Bury the buffer at the head of `ido-matches'."
  (interactive)
  (let ((enable-recursive-minibuffers t)
	(buf (ido-name (car ido-matches)))
	(nextbuf (cadr ido-matches)))
    (when (get-buffer buf)
      ;; If next match names a buffer use the buffer object;
      ;; buffer name may be changed by packages such as
      ;; uniquify.
      (when (and nextbuf (get-buffer nextbuf))
	(setq nextbuf (get-buffer nextbuf)))
      (bury-buffer buf)
      (if (bufferp nextbuf)
	  (setq nextbuf (buffer-name nextbuf)))
      (setq ido-default-item nextbuf
	    ido-text-init ido-text
	    ido-exit 'refresh)
      (exit-minibuffer))))


(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
			  (mapcar (lambda (path)
				    (replace-regexp-in-string home "~" path))
				  recentf-list)
			  nil t))))
(global-set-key (kbd "C-c r") 'ido-choose-from-recentf)
(global-set-key (kbd "C-c C-r") 'ido-choose-from-recentf)

;; Alt for ido recentf, pretty sweet since shorter names...
;; Shouldn't really have both?
;; (defun recentf-open-files-compl ()
;;   (interactive)
;;   (let* ((all-files recentf-list)
;;	 (tocpl (mapcar (function
;;			 (lambda (x) (cons (file-name-nondirectory x) x))) all-files))
;;	 (prompt (append '("File name: ") tocpl))
;;	 (fname (completing-read (car prompt) (cdr prompt) nil nil)))
;;     (find-file (cdr (assoc-ignore-representation fname tocpl)))))

;; Open file in another window (mirrors C-x C-f)
;; Maybe use some C-x/C-c M-f stuff here?
(global-set-key (kbd "C-c C-f") 'ido-find-file-other-window)
;; Also check out ido-find-file-in-dir for getting around size of Dropbox/
;; dired may help with this quite a bit
;; Open file in another window, don't select it
(global-set-key (kbd "C-c f") 'ido-display-file)
;; Open buffer in another window, select it
(global-set-key (kbd "C-c C-b") 'ido-switch-buffer-other-window)
;; Open buffer in another window, don't select it
(global-set-key (kbd "C-c b") 'ido-display-buffer)
(global-set-key (kbd "C-M-f") 'ido-display-buffer)

;; Set some faces for ido
(set-face-attribute 'ido-only-match nil :foreground "green")
(set-face-attribute 'ido-first-match nil :foreground "blue" :weight 'normal)
;; (set-face-attribute 'ido-incomplete-regexp nil
;; (set-face-attribute 'ido-hacks-flex-match nil
;; (set-face-attribute 'ido-subdir nil
;; (set-face-attribute 'ido-indicator nil
;; (set-face-attribute 'ido-virtual nil


;; ;;;;;;;;; ##### FIXME TODO
;; Better/easier buffer menu???
;; (global-set-key (kbd "C-c C-b") 'bs-show)
;; What about ibuffer?!  Also iswitchb-mode for switching...
;; (global-set-key (kbd "C-c C-b") 'ibuffer
;; Consider this from http://emacs.stackexchange.com/q/2177/2051
;; (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; Also electric, but... eh
;; (electric-buffer-list)

;; ibuffer options
;; (setq ibuffer-always-show-last-buffer :nomini)
;; (setq ibuffer-default-shrink-to-minimum-size t)
;; (setq ibuffer-jump-offer-only-visible-buffers nil)
;; (setq ibuffer-show-empty-filter-groups nil)

;; Set specific filter groups via mode
;; (setq ibuffer-saved-filter-groups
;;       '(("default"
;;	 ("dired" (mode . dired-mode))
;;	 ("source" (or
;;		    (mode . c-mode)
;;		    (mode . c++-mode)
;;		    (mode . objc-mode)
;;		    (mode . cperl-mode)
;;		    (mode . perl-mode)
;;		    (mode . java-mode)
;;		    (filename . "\\.rb\\'")))
;;	 ("web" (or
;;		 (filename . "\\.js\\'")
;;		 (filename . "\\.erb\\'")
;;		 (mode . html-mode)
;;		 (mode . nxml-mode)
;;		 (mode . nxhtml-mode)
;;		 (mode . haml-mode)
;;		 (mode . css-mode)
;;		 (mode . sass-mode)
;;		 (mode . coffee-mode)))
;;	 ("doc" (or
;;		 (mode   . latex-mode)
;;		 (mode   . metapost-mode)
;;		 (mode   . doc-view-mode)
;;		 (mode   . markdown-mode)))
;;	 ("build" (or
;;		   (mode . cmake-mode)
;;		   (mode . makefile-mode)
;;		   (mode . makefile-gmake-mode)
;;		   (filename . "Gemfile\\'")
;;		   (filename . "Gemfile\\.lock\\'")
;;		   (filename . "[Rr]akefile\\'")))
;;	 ("pim" (or
;;		 (name    . "^\\*Calendar\\*$")
;;		 (name    . "^diary$")
;;		 (mode    . org-mode)))
;;	 ("system" (or
;;		    (mode       . help-mode)
;;		    (mode       . completion-list-mode)
;;		    (mode       . apropos-mode)
;;		    (name      . "^\\*.*\\*$")
;;		    (filename . "\.emacs\.d")
;;		    (mode     . custom-mode))))))

;; (setq
;;  ibuffer-fontification-alist
;;  '(;; read-only buffers
;;    (10 buffer-read-only eshell-ls-readonly-face)
;;    ;; emacs' "special" buffers
;;    (15 (string-match "^*" (buffer-name)) eshell-ls-special-face)
;;    ;; hidden buffers
;;    (20 (and (string-match "^ " (buffer-name)) (null buffer-file-name))
;;        eshell-ls-symlink-face)
;;    ;; help buffers
;;    (25 (memq major-mode ibuffer-help-buffer-modes)
;;        eshell-ls-archive-face)
;;    ;; IRC buffers
;;    (30 (eq major-mode 'erc-mode) erc-notice-face)
;;    ;; dired buffers
;;    (35 (eq major-mode 'dired-mode) eshell-ls-directory-face)))

;; Kill current buffer
(defalias 'kill-current-buffer 'kill-buffer-and-window)
(global-set-key (kbd "C-c 0") 'kill-current-buffer)
(global-set-key (kbd "C-c C-c") 'kill-current-buffer)
(global-set-key (kbd "C-x w") 'kill-current-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
;; (global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\M-w" 'copy-region-as-kill)

;; Interesting code, will prompt at login to answer before loading .emacs
;; (setq mylist (list "red" "blue" "yellow" "clear" "i-dont-know"))
;; (ido-completing-read "what is your favorite color? " mylist)


;;;;;;;;;;;;;;;;;;;
;; Smex stuff, better M-x using ido
;; Should put this closer to the end since (smex-initialize) so early will
;; miss functions loaded via autoload
;; While smex is active...
;; C-h f gets describe function on selected command
;; M-. goes to definition of selected command
;; C-h w shows key bindings
;; smex-show-unbound-commands for frequently used commands with not key binding
;; https://github.com/nonsequitur/smex/
(require 'smex)
;; Specify save file in ~/.emacs.d/ folder, MUST be before initializing
(setq smex-save-file "~/.emacs.d/smex-items")
(smex-initialize)

;; Only a slight speed enhancement, but let's be honest: I'm not loading tons
;; of code building new functions all the time here
(setq smex-auto-update nil)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
(global-set-key (kbd "C-c M-X") 'execute-extended-command)
;; Just in case you can't use meta for some reason, although admittedly this
;; is probably just a waste of a useful key combo
;; (global-set-key (kbd "C-c x") 'execute-extended-command)
(setq smex-prompt-string "Smx ")
(setq smex-history-length 256)

;;;;;;;;;;;;;;;;;;;
;; Calendar/Diary stuff
(setq diary-file "~/diary")
;; (setq mark-diary-entries-in-calendar t)
;; When M-x diary, default is 1
(setq number-of-diary-entries 5)
(setq mark-holidays-in-calendar t)
(global-set-key (kbd "C-c c") 'calendar)
;; Mark the current day
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; Davis, CA
(setq calendar-location-name "Davis, CA"
      calendar-latitude [38 52 north]
      calendar-longitude [121 65 west])

;; NYC
;; (setq calendar-location-name "New York, NY"
;;       calendar-latitude [40 80 north]
;;       calendar-longitude [73 97 west])

;;; Scrolling
;; Nothing works?
;; Fix foolish calendar-mode scrolling.
(add-hook 'calendar-load-hook
	  '(lambda ()
	     (define-key calendar-mode-map ">" 'scroll-calendar-left)
	     (define-key calendar-mode-map "<" 'scroll-calendar-right)
	     (define-key calendar-mode-map "\C-x>" 'scroll-calendar-left)
	     (define-key calendar-mode-map "\C-x<" 'scroll-calendar-right)))

;; ;(defun p-calendar-prefs () (interactive)
;; (add-hook 'calendar-load-hook
;;	  (lambda()
;;		 (define-key calendar-mode-map [M-right] 'calendar-forward-month)
;;		 (define-key calendar-mode-map [M-left] 'calendar-backward-month)
;;		 (define-key calendar-mode-map [C-M-left] 'calendar-backward-year)
;;		 (define-key calendar-mode-map [M-C-right] 'calendar-forward-year)))

;; (add-hook 'calendar-load-hooks 'p-calendar-prefs)
;; (add-hook 'calendar-after-frame-setup-hooks 'p-calendar-prefs)

;;;;;;;;;;;;;;;;;;;

;; Cleaner, more meaningful narrow-to-region
;; https://github.com/Bruce-Connor/fancy-narrow
(require 'fancy-narrow)

;; Allows hiding of comments
(autoload 'hide/show-comments-toggle "hide-comnt"
  "Toggle hiding/showing of comments in the active region or whole buffer." t)
(defalias 'toggle-comments 'hide/show-comments-toggle)
(global-set-key (kbd "C-x ;") 'toggle-comments)
;; (global-set-key (kbd "C-x ;") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-x #") 'comment-or-uncomment-region)

;; Autowrap comments but only comments (longlines-mode for all)
(setq-default fill-column 78 ; Default is 70
	      comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook 'auto-fill-mode)
;; https://github.com/alpaker/Fill-Column-Indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#000000"
      fci-rule-column '80)
(add-hook 'prog-mode-hook 'fci-mode)


;; I can't tell if this does anything different from M-q but the author
;; certain seems to think so... https://snarfed.org/fillcode
;; (require 'fillcode)
(autoload 'fillcode-mode "fillcode" "A minor mode to enhance fill functions
when in source code modes such as python-mode or perl-mode" t)

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

;; Comment colors
(set-face-attribute 'font-lock-comment-face nil :foreground "black")
;; Comment-starter color
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "red")

;; (require 'boxquote)
;; Better than boxquote
;; (require 'rebox2)
;; (setq rebox-style-loop '(24 16))
;; (global-set-key [(meta q)] 'rebox-dwim)
;; (global-set-key [(shift meta q)] 'rebox-cycle)

;; open my init files
(defun dot-emacs ()
  "Open `~/.emacs'."
  (interactive)
  (find-file (expand-file-name "~/.emacs")))

(defun dot-bashrc ()
  "Open `~/.bashrc'."
  (interactive)
  (find-file (expand-file-name "~/.bashrc")))

(defalias 'bash-mode 'sh-mode)

;; Default mode
(setq-default major-mode 'text-mode)

;; Make RET aka C-m tab as well, C-j just newline
;; (global-set-key "\C-m" 'newline-and-indent)

;; Cleaner: Reindent current line, insert newline, indent newline
(global-set-key "\C-m" 'reindent-then-newline-and-indent)
(global-set-key "\C-j" 'newline)

;; Probably don't need these, don't work currently anyway
;; lpstat -p -d to get list of printers or lpq
;; (setq printer-name "PDF")
;; (setq send-mail-function (quote mailclient-send-it))

;; enlarge or shrink windows
(global-set-key (kbd "C-x }") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <up>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x {") 'shrink-window-horizontally)
(global-set-key (kbd "C-x <down>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c {") 'enlarge-window)
(global-set-key (kbd "C-c }") 'shrink-window)
;; (global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)


;; Prevent the startup message and splash screen
(setq inhibit-startup-echo-area-message "Amory"
      inhibit-startup-message t)

;; See what you type in real-time
(setq echo-keystrokes 0.01)

;; Max lines in *Messages*, default 1000
(setq message-log-max 1500)
;; 2305843009213693951
;; (setq message-log-max most-positive-fixnum)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hi-lite current line
;; As an alternative to the builtin, use Drew Adams' hl-line+
;; col-highlight requires vline.el as well
(require 'hl-line+)
(require 'col-highlight)
;; (global-hl-line-mode 1)
;; (column-highlight-mode 1)
;; Show when idle
(toggle-hl-line-when-idle 1)
(toggle-highlight-column-when-idle 1)
;; Customize hl-line, etc. colors
(set-face-attribute 'hl-line nil :background "black" :foreground "white")
;; (set-face-attribute 'col-highlight
;;		    nil :background "black" :foreground "white")
;; Doesn't perfectly hidge the fringe lines, but sobeit
(set-face-attribute 'vertical-border
		    nil :background "black" :foreground "black")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Point out changes
;; I hate this
;; (highlight-changes-mode 1)

;; Turn off git
;; Same thing, first one safer?  But not just git?
;; (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
;; (delete 'Git vc-handled-backends) ;; delete git from list of backends
(setq vc-handled-backends nil) ;; delete all backends

;; Useful for git related work
;; (require 'find-file-in-project)
;; (global-set-key (kbd "C-x f") 'find-file-in-project)

;; Highlight ( and ) Highlight phrase if no matching paren.
(show-paren-mode t)
(setq show-paren-style 'expression
      show-paren-delay 0.5)

;; Highlight parens currently between
;; https://github.com/nschum/highlight-parentheses.el
(require 'highlight-parentheses)
(highlight-parentheses-mode t)
;; Bold 'em, Color mismatched differently
(progn
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (set-face-attribute 'show-paren-match-face nil :background "red")
  (set-face-attribute 'show-paren-mismatch-face nil :background "red")
  (setq hl-paren-colors (quote ("red" "white" "green" "cyan"
				"red" "white" "green" "cyan"))))

;; Allow highlighting of phrases.  idle-highlight is way too sticky.
;; Maybe hook for prog modes (perl, etc.)  ;;;;;;;; ####### FIXME TODO
;; https://github.com/nschum/highlight-symbol.el
(require 'highlight-symbol)
;; (require 'idle-highlight-mode)
(setq highlight-symbol-idle-delay 1) ; default 0.5
;; (setq highlight-symbol-list
;; ;;;;;; ##### FIXME TODO FIX COLORS BEFORE GOING FORWARD
;; (highlight-symbol-mode t)

;; (defun my-highlight-idling-hook ()
;;   (make-local-variable 'column-number-mode)
;;   (column-number-mode t)
;;   (if window-system (hl-line-mode t))
;;   (idle-highlight-mode))

;; (add-hook 'emacs-lisp-mode-hook 'my-highlight-idling-hook)
;; (add-hook 'perl-mode-hook 'my-highlight-idling-hook)

;; list-colors-display for basic colors, list-faces-display for color options
;; See the rest: https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html#Standard-Faces
(progn
  ;; modeline colors
  (set-face-attribute 'mode-line nil :background "white" :foreground "black")
  ;; minibuffer prompt
  (set-face-attribute 'minibuffer-prompt
		      nil :foreground "magenta" :weight 'normal)
  ;; buffer name
  (set-face-attribute 'mode-line-buffer-id nil :background "cyan")
  ;; not sure
  (set-face-attribute 'mode-line-emphasis nil :background "magenta")
  ;; highlight on modeline?
  (set-face-attribute 'mode-line-highlight nil :background "red")
  ;; other window, fringe lines
  (set-face-attribute 'mode-line-inactive nil :background "blue"))

;; Display depth indicator, kind of weird but may be useful
(setq minibuffer-depth-indicate-mode t)

;; Remap C-t to C-x prefix
;; (bind "C-t" (lookup-key global-map (kbd "C-x")))

;; Ctrl-q map.  I'll never use quoted-insert, so I might as well make this
;; thing more useful.  Should add to this. ;;;;; #### FIXME TODO
(defvar my/ctrl-q-map (make-sparse-keymap)
  "My original keymap bound to C-q.")
(defalias 'my/ctrl-q-prefix my/ctrl-q-map) ; Why?
(define-key global-map (kbd "C-q") 'my/ctrl-q-prefix)
(define-key my/ctrl-q-map (kbd "C-q") 'quoted-insert)
(define-key my/ctrl-q-map (kbd "C-c") 'column-highlight-mode)
(define-key my/ctrl-q-map (kbd "C-a") 'align-regexp)
(define-key my/ctrl-q-map (kbd "q") 'qrr)
(define-key my/ctrl-q-map (kbd "r") 'rr)
(define-key my/ctrl-q-map (kbd "a") 'align)
(define-key my/ctrl-q-map (kbd ".") 'highlight-symbol-at-point)
(define-key my/ctrl-q-map (kbd "?") 'highlight-symbol-remove-all)
(define-key my/ctrl-q-map (kbd "/") 'highlight-symbol-remove-all)


;; https://github.com/nflath/hungry-delete
;; Delete all white space chars at once
;; http://endlessparentheses.com/hungry-delete-mode.html
(require 'hungry-delete)
(global-hungry-delete-mode)

;; If at beginning of a line, don't make me C-k twice.
;; (setq kill-whole-line t)

;; Turn off stupid "yes" / "no" full word prompts
(fset 'yes-or-no-p 'y-or-n-p)
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; No mouse
(setq use-dialog-box nil
      use-file-dialog nil)

;; Was exchange-point-and-mark
(global-set-key (kbd "C-x C-x") 'delete-other-windows)

;; C-x u for tree, C-_ to undo, M-_ to redo, etc.
(require 'undo-tree)
(global-undo-tree-mode)
;; Larger size limits for undo
(setq undo-outer-limit 18000000 ;; 12000000
      undo-limit 100000) ;; 80000
;; Define a hook to automatically show diff/timestamps?
;; ;;;;;;;; ########## FIXME TODO

;; Something useful instead of uppercase region, esp. given typos.  Never use
;; this, should maybe put something better here
(global-set-key (kbd "C-x C-u") 'undo)

;; Highlight region undo, yanked, etc., is awesome
;; https://github.com/k-talo/volatile-highlights.el
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Easily indent given line according to mode
;; https://github.com/hbin/smart-shift
(require 'smart-shift)
(global-smart-shift-mode 1)


;; http://github.com/rejeep/emacs/blob/master/rejeep-defuns.el
(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))

(defun indent-buffer ()
  "Indents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))


(defun replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
      (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))

(defun camelize-buffer ()
  (interactive)
  (goto-char 0)
  (ignore-errors
    (replace-next-underscore-with-camel 0))
  (goto-char 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use ido to select which window based off window name
(defun rotate-list (list count)
  "Rotate the LIST by COUNT elements"
  (cond
   ((= count 0) list)
   ((not list) list)
   (t
    (rotate-list (nconc (cdr list) (list (car list)) '()) (1- count)))))

(defun dka-sort-by-other-list (to-sort-list other-list)
  (let* ((index 0)
	 (other-alist (mapcar (lambda (buffer)
				(setq index (+ index 1))
				(cons buffer index))
			      other-list))
	 (swartz (mapcar (lambda (item)
			   (cons (cdr (assoc item other-alist)) item))
			 to-sort-list))
	 (sorted-list (sort swartz (lambda (a b) (< (car a) (car b))))))
    (mapcar 'cdr sorted-list)))

(defun jump-to-window ()
  "Interactively jump to another visible window based on it's
`buffer-name' using `ido-completing-read'"
  (interactive)
  (let* ((visible-buffers (mapcar '(lambda (window) (window-buffer window)) (window-list)))
	 (sorted-visible-buffers (dka-sort-by-other-list visible-buffers (buffer-list)))
	 (rotated-buffer-list (rotate-list sorted-visible-buffers 1))
	 (visible-buffer-names (mapcar (lambda (buffer) (buffer-name buffer)) rotated-buffer-list))
	 (buffer-name (ido-completing-read "Enter buffer to jump to: "
					   visible-buffer-names
					   nil t))
	 (window-of-buffer
	  (delq nil
		(mapcar '(lambda (window)
			   (if (equal buffer-name (buffer-name (window-buffer window)))
			       window nil)) (window-list)))))
    (select-window (car window-of-buffer))))

(global-set-key (kbd "C-c C-o") 'jump-to-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; bs-cycling is way better
(global-set-key (kbd "C-x <right>") 'bs-cycle-next)
(global-set-key (kbd "C-x <left>") 'bs-cycle-previous)
;; Which buffers to show
;; all, files, files-and-scratch, all-intern-last(?)
(setq bs-default-configuration "files")

;; Create your own!
;; "Name", must-show-regex, must-show-func, dont-rgx, dont-func, sort-funct
;; (add-to-list 'bs-configurations
;;	     '("SQL" nil nil nil
;;	       (lambda (buf)
;;		 (with-current-buffer buf
;;		   (not (memq major-mode
;;			      '(sql-interactive-mode sql-mode))))) nil))


;; keyboard-escape-quit is a stronger keyboard-quit
(global-set-key [(control g)] 'keyboard-escape-quit)

;; ssh editing, just kinda nice
;; https://github.com/jhgorrell/ssh-config-mode-el
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)


;; Ensure M-x shell uses login.  Not that I ever use M-x shell...
(setq explicit-bash-args '("--login"))
;; C-c s do a one-liner shell command
(global-set-key [(control c) (s)] 'shell-command)
;; C-c a to go to a terminal shell (ansi-term)
(global-set-key [(control c) (a)] 'ansi-term)

;; Exit in (ansi-)term returns to the emacs buffer
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc))) ad-do-it
	   (kill-buffer buffer)) ad-do-it))
(ad-activate 'term-sentinel)

;; Always use /bin/bash, don't ask
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Make links in man pages, etc., work, in ansi-mode
(defun my-term-hook()
  (goto-address-mode))
(add-hook 'term-mode-hook 'my-term-hook)

;; Convert DOS `^M' end of lines to Unix end of lines.  See also
;; set-buffer-file-coding-system (C-x RET f) with unix.  Do for mac?
(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))
;; They all sound the same
(defalias 'dos2unix 'dos-to-unix)
(defalias 'dostounix 'dos-to-unix)

(defun replace-smart-quotes ()
  "Replace MS smart quotes with normal quotes in this buffer."
  (interactive)
  (save-excursion
    (let ((fixes '((342396 . "\"") (342392 . "'") (342387 . "--")
		   (342397 . "\"") (342393 . "'"))))
      (goto-char (point-min))
      (while (/= (point) (point-max))
	(let* ((this (char-after (point)))
	       (match (assq this fixes)))
	  (when match
	    (delete-char 1)
	    (insert (cdr match))))
	(forward-char 1)))))


;; Might remember this better
(defalias 'reload-buffer 'revert-buffer)

;; insert a time stamp string
;; ADD PREFIX FOR date, %R, OR ALL ;;;;;;;;;;; FIXME TODO ##########
(defun insert-time-stamp ()
  "Insert a time stamp comment."
  (interactive "*")
  (insert (format "%s %s %s" comment-start
		  (format-time-string "%Y-%m-%d %R")
		  comment-end)))

;; Prefix means C-u (4) or C-u C-u (16)
(defun insert-date (prefix)
  "Insert the current date in ISO format. With prefix-argument, add day of
week. With two prefix arguments, add day of week and time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
		      ((equal prefix '(4)) "%Y-%m-%d %a")
		      ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
    (insert (format-time-string format))))
(global-set-key (kbd "C-c .") 'insert-date)


;; On duplicate filenames, show paths not <2>
;; Default in 24.4?
(require 'uniquify)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(uniquify-after-kill-buffer-p t)
 ;; '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))


(defun checksum-region (s e)
  "Print a checksum (currently md5) of the region."
  (interactive "r")
  (message (md5 (buffer-substring s e))))
(defalias 'md5-region 'checksum-region)


;; BTC in modeline, requires request.el
(autoload 'btc-ticker-mode "btc-ticker" "asdasd" t)
(setq btc-ticker-api-poll-interval 60)

(defun loan-payment-calculator (amount rate years)
  "Calculate what the payments for a loan of AMOUNT dollars when annual
percentage rate is RATE and the term of the loan is YEARS years.  The RATE
should expressed in terms of the percentage \(i.e. \'8.9\' instead of
\'.089\'\) and must contain a decimal point.  The total amount of interest
charged over the life of the loan is also given."
  (interactive "nLoan Amount: \nnAPR (per): \nnTerm (years): ")
  (let ((payment (/ (* amount (/ rate 1200)) (- 1 (expt (+ 1 (/ rate 1200)) (* years -12.0))))))
    (message "%s payments of $%.2f. Total interest $%.2f"
	     (* years 12) payment (- (* payment years 12) amount))))

(defun interest-basic-savings-calculator (principal rate years compounds)
  "Calculate the future value of a savings account given PRINCIPAL at RATE for
YEARS.  The RATE should be expressed as a percentage \(i.e. \'8.9\' instead of
\'.089\'\) and must contain a decimal point."
  (interactive "nPrincipal: \nnRate (per): \nnLength (years): \nnCompound freq.: ")
  (let ((final (* principal (expt (+ 1 (/ (/ rate 100) compounds)) (* years compounds)))))
    (message "$%.2f after %s years at %.2f"
	     final years rate)))


;; Add compounding option?  Default to 12 ;;;;;; ##### FIXME TODO
(defun interest-contributions-savings-calculator (principal rate years contrib)
  "Calculate the future value of a savings account given PRINCIPAL at RATE for
YEARS with CONTRIB contributions before every period.  The RATE should be
expressed as a percentage \(i.e. \'8.9\' instead of \'.089\'\) and must
contain a decimal point."
  (interactive "nPrincipal: \nnRate (per): \nnLength (years): \nnContribution: ")
  (let  ((final (+ (* principal (expt (+ 1 (/ rate 100)) years)) (* contrib (/ (- (expt (+ 1 (/ rate 100)) (+ 1 years)) (+ 1 (/ rate 100))) (/ rate 100))))))
    (message "$%.2f after %s years at %.2f"
	     final years rate)))

(defun interest-present-value-calculator (future rate years)
  "Calculate the present value needed to produce a future value FUTURE in
YEARS given RATE.  The RATE should be expressed as a percentage \(i.e. \'8.9\'
instead of \'.089\'\) and must contain a decimal point."
  (interactive "nFuture Value: \nnRate (per): \nnLength (years): ")
  (let ((final (/ future (expt (+ 1 (/ rate 100)) years))))
    (message "$%.2f given %s years at %.2f"
	     final years rate)))

(defun interest-rate-calculator (present future years)
  "Calculate the interest rate RATE needed to produce a future value FUTURE in
YEARS given a starting PRESENT value.  One of the years should contain a
decimal point."
  (interactive "nPrincipal: \nnFuture Value: \nnLength (years): ")
  (let ((final (- (expt (/ future present) (/ 1.0 years)) 1))) ; Decimal allows decimals
    (message "%.2f%% yields %.2f from %.2f after %s years"
	     (* 100 final) future present years)))


(defun round-to-decimal (value place)
  "Round the given VALUE to a given decimal PLACE.  Entering 0 for PLACE will
return an integer (with a 0 in the tenths place), while negative numbers will
round to ones, tens, etc."
  (interactive "nValue: \nnPlace: ")
  (let ((multiplier (* 1.0 (expt 10 place))))
    (message "%s asd" (/ (round (* multiplier value)) multiplier))))


;; Guess keybindings game
;; Maybe tweak to get a list for studying?
(require 'keywiz)
;; Poker
(require 'poker)
;; Mandelbrot set
(require 'u-mandelbrot)


;; From https://github.com/purcell/emacs-xkcd
;; (require 'emacs-xkcd)
;; (require 'xkcd); or this?

;; Requires howdoi to be installed (python)
(require 'howdoi)


;; Locate takes forever...
(setq locate-command "mdfind")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idling stuff

;; Random emacs usage quote in minibuffer when idle.  Probably better if these
;; don't clog up *messages*, maybe via popwin ;;;;;; ##### FIXME TODO
;; (require 'random-idle-quote)
;; (random-idle-quote)


;; Zone after 3 idle minutes
(autoload 'zone "zone" "Zone out, completely" t)
(autoload 'zone-when-idle "zone" "Zone out when Emacs has been
idle for SECS seconds." t)
;; (setq zone-idle (* 60 3))
;; (zone-when-idle zone-idle)

;; Zone-choose to select a specific zoning method
(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive
   (list
    (completing-read
     "Program: "
     (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
    (zone)))

;; Define md5 zoning
(defun zone-pgm-md5 ()
  "MD5 the buffer, then recursively checksum each hash."
  (let ((prev-md5 (buffer-substring-no-properties ;; Initialize.
		   (point-min) (point-max))))
    ;; Whitespace-fill the window.
    (zone-fill-out-screen (window-width) (window-height))
    (random t)
    (goto-char (point-min))
    (while (not (input-pending-p))
      (when (eobp)
	(goto-char (point-min)))
      (while (not (eobp))
	(delete-region (point) (line-end-position))
	(let ((next-md5 (md5 prev-md5)))
	  (insert next-md5)
	  (setq prev-md5 next-md5))
	(forward-line 1)
	(zone-park/sit-for (point-min) 0.1)))))

;; Add md5 to zone list
(eval-after-load "zone"
  '(unless (memq 'zone-pgm-md5 (append zone-programs nil))
     (setq zone-programs
	   (vconcat zone-programs [zone-pgm-md5]))))


(defun wm-format-time-string (format &optional time)
  "Like format-time-string except that %a gives a 2-char abbreviation."
  ;; Under GNU 19.34 (format-time-string "%2a") returns "2a".
  (callf or time (current-time))
  (if (string-match "\\(\\`\\|[^%]\\|\\(%%\\)+\\)%a" format)
      (let ((i (match-end 0)))
	(concat (format-time-string (substring format 0 (- i 2)) time)
		(substring (format-time-string "%a" time) 0 2)
		(wm-format-time-string (substring format i) time)))
    (format-time-string format time)))

;; Moved to bottom (duh)
;; (message "Emacs loaded at %s." (wm-format-time-string "%T %a %d %b %y"))


;; Should figure out an autoload or eval-after-load for this
;; ;;;;;;; ###### FIXME TODO
;; (require '1000-words)


;;; Maybe set up a modemap for definitions?  ;;;;;; ##### FIXME TODO
;; Thesaurus
;; (require 'thesaurus)
(autoload 'thesaurus-choose-synonym-and-replace "thesaurus"
  "Search for synonyms in the http://words.bighugelabs.com database." t)
(setq thesaurus-bhl-api-key "20766aa5f2e2d098765f7f8ebac19579")
(define-key global-map (kbd "C-c C-t") 'thesaurus-choose-synonym-and-replace)

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(global-set-key (kbd "C-c C-n") 'dictionary-search)
(global-set-key (kbd "C-c C-m") 'dictionary-match-words)


;; Format to display in a buffer? ;;;;;;;; ###### FIXME TODO
(defvar open-dictionary-hist)
(defun open-dictionary (the-word)
  "Open Dictionary.app for the-word"
  (interactive (list
		(let* ((wap (word-at-point))
		       (w (read-from-minibuffer
			   (format "Dictionary Lookup (%s): " wap)
			   nil nil nil 'open-dictionary-hist)))
		  (if (zerop (length w)) wap w))))
  (start-process "dict" nil "open" (concat "dict:///" the-word)))



;; JUST USE WEBJUMP!!!!
(global-set-key (kbd "C-x g") 'webjump)
;; Add some missing items to the webjump catalog ;;;;; #### FIXME TODO
(eval-after-load "webjump" '(progn
			      (add-to-list 'webjump-sites
					   '("Urban Dict" .  [simple-query
							      "www.urbandictionary.com"
							      "http://www.urbandictionary.com/define.php?term="
							      ""]))
			      ;; Fix emacswiki search in webjump
			      (add-to-list 'webjump-sites
					   '("Emacs Wiki" .  [simple-query
							      "www.emacswiki.org"
							      "https://duckduckgo.com/?q="
							      "+site%3Aemacswiki.org"]))
			      ;; DOI
			      (add-to-list 'webjump-sites
					   '("DOI" .  [simple-query
						       "www.dx.doi.org/"
						       "www.dx.doi.org/"
						       ""]))
			      ;; Youtube
			      (add-to-list 'webjump-sites
					   '("Youtube" .  [simple-query
							   "www.youtube.com"
							   "http://www.youtube.com/results?search_query="
							   ""]))
			      ;; IMDB
			      (add-to-list 'webjump-sites
					   '("IMDB" .  [simple-query
							"www.imdb.com"
							"www.imdb.com/find?q="
							"&s=all"]))))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Google: "))))))
(global-set-key (kbd "C-c g") 'google)

(defun youtube ()
  "Search YouTube with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.youtube.com/results?search_query="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Search YouTube: "))))))

(defun wikipedia ()
  "Search enWiki with a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://en.wikipedia.org/wiki/Special:Search/"
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Search enWikipedia: "))))))
(global-set-key (kbd "C-c w") 'wikipedia)

(defun doi ()
  "Resolve a DOI link."
  (interactive)
  (browse-url
   (concat
    "http://www.dx.doi.org/"
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "DOI link: "))))))
(global-set-key (kbd "C-c d") 'doi)

(defun emacswiki ()
  "Search the EmacsWiki for a region or specific query."
  (interactive)
  (browse-url
   (concat
    "https://duckduckgo.com/?q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Emacs: ")))
    "+site%3Aemacswiki.org")))
(global-set-key (kbd "C-c e") 'emacswiki)


;; When editing web stuff (CGI scripts, html) I may want to quickly check it.
;; browse-url-of-buffer will render the url assigned to a buffer.  This tells
;; Emacs how to map a given filename to a url
;; Check out skewer https://github.com/skeeto/skewer-mode
(setq browse-url-filename-alist
      '(("/Users/Amory/Documents/perl/website/" . "http://amorymeltzer.org/cgi-bin/")
	("^/\(ftp@\|anonymous@\)?\([^:]+\):/*" . "ftp://\2/")
	("^/\([^:@]+@\)?\([^:]+\):/*" . "ftp://\1\2")
	("^/+" . "file:/")))

(global-set-key (kbd "C-c C-v") 'browse-url-of-buffer)

(global-set-key (kbd "C-c v") 'browse-url)

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
	 (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
	  ((search-forward "<html" nil t) (html-mode)))))

;; Google translate interface
;; https://github.com/atykhonov/google-translate
(require 'google-translate)
(require 'google-translate-default-ui)
;; Override default through C-u prefix
;; (setq google-translate-default-target-language "en")
;; Always detect
(setq google-translate-default-source-language "auto"
      google-translate-enable-ido-completion 1
      google-translate-show-phonetic 1)
;; (global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-ct" 'google-translate-query-translate)


;; Post to bash.org via Emacs!  Here mainly as an example of cool capabilities
;; (require 'bash-org)

;; In case I ever want to screencast Emacs
;; https://github.com/lewang/command-log-mode
(autoload 'clm/open-command-log-buffer "command-log-mode" "Log
keyboard commands to a buffer.  Useful for screencasting Emacs."
  t)
;; (add-hook 'prog-mode-hook 'command-log-mode)


;; DNA mode, mucks with keybindings
(autoload 'dna-mode "dna-mode" "Major mode for dna" t)
;; I think this only loads dna-primer after dna-mode is called
(eval-after-load 'dna-mode '(require 'dna-primer))
;; If a buffer begins with dna-like things
(add-to-list 'magic-mode-alist '("^>\\|ID\\|LOCUS\\|DNA" . dna-mode))
(add-to-list 'auto-mode-alist
	     '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))
(add-hook 'dna-mode-hook 'turn-on-font-lock)


;; CSV mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; will I even use these?
;; (require 'thingatpt+)
;; Expanded things, namely kill thing, mark thing, and upward mark thing
;; (require 'thing-opt)
;; (define-thing-commands)


;; Describe whatever I'm on/in
;; http://www.sugarshark.com/elisp/init/lisp.el.html
(defun describe-foo-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:

-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first
    ;; half.
    (cond ((setq sym (ignore-errors
		       (with-syntax-table emacs-lisp-mode-syntax-table
			 (save-excursion
			   (or (not (zerop (skip-syntax-backward "_w")))
			       (eq (char-syntax (char-after (point))) ?w)
			       (eq (char-syntax (char-after (point))) ?_)
			       (forward-sexp -1))
			   (skip-chars-forward "`'")
			   (let ((obj (read (current-buffer))))
			     (and (symbolp obj) (fboundp obj) obj))))))
	   (describe-function sym))
	  ((setq sym (variable-at-point)) (describe-variable sym))
	  ;; now let it operate fully -- i.e. also check the surrounding sexp
	  ;; for a function call.
	  ((setq sym (function-at-point)) (describe-function sym)))))
(global-set-key (kbd "C-h h") 'describe-foo-at-point)


;; Make the scratch buffer blank
;; (setq initial-scratch-message "")
;; Use haikus instead
(require 'amory-emacs-haiku)
;; Initialize *scratch* buffer with a random Emacs haiku
(setq initial-scratch-message (amory-random-emacs-haiku))


;; All the cperl options, bad?  Need to fix ;;;;; #### FIXME TODO
;; Affects:
;; cperl-font-lock, cperl-electric-lbrace-space, cperl-electric-parens
;; cperl-electric-linefeed, cperl-electric-keywords, cperl-lazy-help-time
;; cperl-info-on-command-no-prompt, cperl-clobber-lisp-bindings
(setq cperl-hairy t)
;; Help in cperl, default is 5s
;; Works in perl-mode?! too?
(setq cperl-lazy-help-time 1)
;; Don't mess with C-h; would be useful but for the above
(setq cperl-clobber-lisp-bindings 1)
;; flymake in cperl
(add-hook 'cperl-mode-hook 'flymake-mode)
;; Treat _ as word character, probably counter-intuitive
;; (setq cperl-under-as-char t)
;; Good?
(setq cperl-font-lock t
      cperl-highlight-variables-indiscriminately t)

;; Do some color fixin' in cperl-mode
(eval-after-load "cperl-mode"
  '(progn
     (setq fill-column 78)		; Need a better place for this...
					; ;;;;;; ##### FIXME TODO
     (set-face-attribute 'cperl-array-face nil :background "nil" :foreground
			 "blue" :underline t) ; arrays
     (set-face-attribute 'cperl-hash-face nil :background "nil" :foreground
			 "red" :underline t) ; hashes
     (set-face-attribute 'cperl-nonoverridable-face nil :background "nil"
			 :foreground "nil") ; `print`, anything else?
     (define-key cperl-mode-map (kbd "C-c C-y") nil)
     (define-key cperl-mode-map (kbd "C-c C-f") nil)
     (define-key cperl-mode-map (kbd "C-c C-b") nil)))

;; Sets face just for cperl-mode, to return magenta to use strict/warnings
;; From https://stackoverflow.com/a/17630877/2521092
;; First create new face which is a copy of hl-line-face
(copy-face 'font-lock-function-name-face 'cperl-strictwarnings-face)

;; Change what you want in this new face
(set-face-attribute 'cperl-strictwarnings-face nil
		    :foreground "magenta" :bold nil)

;; The function to use the new face
(defun my-cperl-strictwarnings ()
  (set (make-local-variable 'font-lock-function-name-face)
       'cperl-strictwarnings-face))

;; Finally, the hook
(add-hook 'cperl-mode-hook 'my-cperl-strictwarnings)

;; Perldoc in emacs
;; (defun perldoc (module) "Equivalent to `perldoc module`"
;; (interactive "MModule: ")
;; (shell-command (concat "perldoc " module)))

;; More complete, but the default in cperl-perldoc is nicer
;; (require 'perl-find-library)
(defalias 'perldoc 'cperl-perldoc)

;; Perl mode alias to make cperl default for .pl
(defalias 'perl-mode 'cperl-mode)


;;; Should probably figure out a way to diminish these fuckers
;; Colors numbers
;; https://github.com/Fanael/number-font-lock-mode
(require 'number-font-lock-mode)
;; Color identifiers based on their name
;; https://github.com/Fanael/rainbow-identifiers
(require 'rainbow-identifiers)
;; Color identifies uniquely
;; https://github.com/ankurdave/color-identifiers-mode
(require 'color-identifiers-mode)

;; Get perldoc after C-h via P
(define-key 'help-command "P" 'perldoc)

;; Search more than just commands but eh, I never use...
(define-key 'help-command "a" 'apropos)


;; Pop-up with completions for prefixes, uses 'popwin
;; Would be nice to get working for M-g ;;;;;; ##### FIXME TODO
;; https://github.com/kbkbkbkb1/guide-key
;; https://github.com/m2ym/popwin-el
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-q")
      guide-key/idle-delay 0.5 ; quicker
      guide-key/recursive-key-sequence-flag t
      guide-key/popup-window-position 'bottom
      guide-key/recursive-key-sequence-flag t
      guide-key/popup-window-position 'bottom)
(guide-key-mode 1)


;; Display what function block if I'm in in certain modes
;; (set-face-attribute 'which-func nil
;;                     :foreground "LightPink3" :weight 'bold)
;; (add-hook 'sh-mode-hook 'which-function-mode)
;; (add-hook 'emacs-lisp-mode-hook 'which-function-mode)


;; Jump to a definition in the current file (holy shit this is awesome)
;; Does this automatically use ido?  Others think it doesn't but I do...
(global-set-key (kbd "C-c i") 'imenu)
;; Unnecessary
;; (set-default 'imenu-auto-rescan t)

;; Face stuff

;; Print face at point
;; M-x what-cursor-position, full with prefix
;; C-u C-x =
;; M-x describe-face to get list, describe certain others
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defalias 'elisp-mode 'emacs-lisp-mode)
;; Give info at point in elisp mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;; 0.5 is too quick, 1 is too slow
(setq eldoc-idle-delay 0.75)


;; Interesting! But no
;; ;; Make scripts executable after they have been saved.
;; (add-hook 'after-save-hook
;;    '(lambda ()
;;       (let ( (temp (substring buffer-file-name -3)) )
;;         (if (or (equal temp ".pl")
;;                 (equal temp ".sh"))
;;             (executable-make-buffer-file-executable-if-script-p)))))


;; Enable wildcard open files
(setq find-file-wildcards t)


;;;;;;;;;;;;;;;;;;;; FLYSPELL ;;;;;;;;;;;;;;;;;;;;
;; ;;; Settings for Flyspell  (Spell checker.)


;; (require 'flyspell)

;; (defun turn-on-flyspell-mode () (flyspell-mode 1))
;; (defun turn-off-flyspell-mode () (flyspell-mode 0))
;; (setq flyspell-highlight-properties t
;;       flyspell-multi-language-p nil
;;       flyspell-issue-welcome-flag nil)

;; (defun turn-on-flyspell-prog-mode () (interactive)
;;   (if (fboundp 'flyspell-prog-mode)
;;       (flyspell-prog-mode)
;;     (setq p-flyspell-restricted-checking t)
;;     (flyspell-mode 1)))

;; (when (and (>= emacs-major-version 20) window-system)
;;   (add-hook 'mail-send-hook 'turn-off-flyspell-mode)
;;   (add-hook 'text-mode-hook 'turn-on-flyspell-mode)
;;   (add-hook 'tex-mode-hook 'turn-on-flyspell-mode t)
;;   (add-hook 'mail-setup-hook 'turn-on-flyspell-mode)
;;   (add-hook 'fundamenal-mode-hook 'turn-on-flyspell-mode t)
;;   (add-hook 'log-mode-hook 'turn-on-flyspell-mode t)
;;   (add-hook 'math-edit-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'verilog-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'perl-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'tcl-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'c-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'postscript-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'c++-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'sh-mode-hook 'turn-on-flyspell-prog-mode t)
;;   (add-hook 'emacs-lisp-mode-hook 'turn-on-flyspell-prog-mode t))


;; (defvar p-flyspell-restricted-checking nil
;;   "If not nil, flyspell only checks spelling in comments and strings.")
;; (make-variable-buffer-local 'p-flyspell-restricted-checking)

;; (defadvice flyspell-get-word
;;   (after checkable first activate)
;;   "Return nil instead of word if checking restricted and not in comment or string.

;; Spell check is restricted if p-flyspell-restricted-checking non-nil.
;; Word considered to be in comment or string if face property is set
;; to font-lock-comment-face or font-lock-string-face."
;;   (setq ad-return-value
;;         (if (and p-flyspell-restricted-checking
;;                  ad-return-value
;;                  (not (member (get-text-property (car (cdr ad-return-value))
;;                                                  `face)
;;                               (list 'font-lock-comment-face
;;                                     'font-lock-string-face))))
;;             nil
;;           ad-return-value)))

;; (if (fboundp 'flyspell-prog-mode)
;;   (ad-deactivate 'flyspell-get-word))


;; FUCKS SHIT UP ;;;;;;; #########
;; Use pretty symbols in buffer
;; (require 'pretty-mode)

;; real lisp hackers use the lambda character
;; courtesy of stefan monnier on c.l.l
;; http://stackoverflow.com/a/158057/2521092
;; (defun sm-lambda-mode-hook ()
;;   (font-lock-add-keywords
;;    nil `(("\\<lambda\\>"
;;    (0 (progn (compose-region (match-beginning 0) (match-end 0)
;;         ,(make-char 'greek-iso8859-7 107))
;;       nil))))))
;; (add-hook 'emacs-lisp-mode-hook 'sm-lambda-mode-hook)
;; (add-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
;; (add-hook 'scheme-mode-hook 'sm-lambda-mode-hook)
;; FUCKS SHIT UP ;;;;;;; #########


;; Rename file and buffer
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: "
				      (file-name-directory filename)
				      nil nil
				      (file-name-nondirectory filename))))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))

;; What does this do? ;;;; ##### FIXME TODO
;; Don't use ido for this.
(put 'rename-current-buffer-file 'ido 'ignore)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
	(ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
	(delete-file filename)
	(kill-buffer buffer)
	(message "File '%s' successfully removed" filename)))))

(defun insert-file-name ()
  (interactive)
  (insert (file-name-nondirectory
	   (buffer-file-name
	    (if (minibufferp)
		(window-buffer (minibuffer-selected-window))
	      (current-buffer))))))

;; ;;;;;; ##### FIXME TODO
(defun copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to
project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new (if eproject-mode
		  (s-chop-prefix (eproject-root) filename)
		filename))))
;; Copy file path to kill ring, probably not the best keys for this
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

;; http://curiousprogrammer.wordpress.com/2009/05/14/inserting-buffer-filename/
;; Version to do whole/part of path? ;;;;;; ##### FIXME TODO
(defun get-files-and-buffers ()
  (let ((res '()))
    (dolist (buffer (buffer-list) res)
      (let ((buffername (buffer-name buffer))
	    (filename (buffer-file-name buffer)))
	(unless (string-match "^ *\\*.*\\*$" buffername)
	  (push buffername res))
	(when filename (push filename res))))))

(defun insert-file-or-buffer-name (&optional initial)
  (interactive)
  (let ((name (ido-completing-read "File/Buffer Name: "
				   (get-files-and-buffers)
				   nil nil initial)))
    (when (and (stringp name) (> (length name) 0))
      (insert name))))

(defun join-following-line (n)
  (interactive "p")
  (if (>= n 0)
      (while (> n 0)
	(join-line t)
	(setq n (1- n)))
    (while (< n 0)
      (join-line)
      (setq n (1+ n))))
  (indent-according-to-mode))

;; How is this different than join-line????
(defun join-previous-line (n)
  (interactive "p")
  (join-following-line (- n)))


;; Control itunes, pointless
;; https://github.com/tavisrudd/emacs.d/blob/master/osx-itunes.el
;; https://github.com/bodhi/emacs.d/blob/master/site-lisp/osx-osascript.el
;; (require 'osx-itunes)
;; (setq itunes-key [f6])


;; See how annoying it truly is
;; (setq garbage-collection-messages t)

;; TEST FOR EMACS VERSION USE FOR IDO AND SMEX
;; ;;;;;;;;;;;;;; ############ FIXME TODO
;; (if (and (>= emacs-major-version 25) (>= emacs-minor-version 2))
;;     (message "Emacs loaded at %s." (wm-format-time-string "%T %a %d %b %y"))
;;   (message "Emacs dumb at %s." (wm-format-time-string "%T %a %d %b %y")))

;; (when (> emacs-major-version 23)
;;   (message "Emacs is >23"))

;; Notification panel, defaults to growl
;; Use to give note about startup?  ########## ;;;;;;;;; FIXME TODO
(autoload 'notify "notify" "Notify TITLE, BODY.")
;; (autoload 'notify "Notify TITLE, BODY.")
;; (notify "one" "two")
;; Simple, direct
;; (require 'growl)


;; Emacs should just have code that automatically sets this threshold
;; according to some function involving a constant, the current date, and
;; Moore's Law.
;; (setq large-file-warning-threshold 50000000)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold (* 8 1024 1024))

;; Just in case
(setq warning-suppress-types nil)

;; Display browse kill ring, set key to auto-complete with ido
;; Kinda weird
;; https://github.com/browse-kill-ring/browse-kill-ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)
(require 'kill-ring-ido)
(global-set-key (kbd "M-y") 'kill-ring-ido)


;;; Byte-compile everthing in .emacs.d folder
;; Required for byte-compilation not to fail???
(setq byte-compilation-scroll-output t)
;; In shell: emacs -batch -f batch-byte-compile ~/.emacs.d/**/*.el
;; Aliased(ish) to recompile_emacs
(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))
;; An .elc file is probably out of date after a save, so remove it
(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))
	    nil t))
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jj" 'whitespace-cleanup)
(key-chord-define-global "hh" 'indent-buffer)
(key-chord-define-global "ii" 'byte-compile-init-dir)
(key-chord-define-global "uu" 'undo-tree-undo)
(key-chord-define-global "yy" 'browse-kill-ring)
(key-chord-define-global "xx" 'er/expand-region)

;; Avoid reaching for shift for common symbols
;; http://endlessparentheses.com/banishing-the-shift-key-with-key-chord-in-emacs.html
;; (key-chord-define-global "0o" ")")
;; (key-chord-define-global "1q" "!")
;; (key-chord-define-global "2w" "@")
;; (key-chord-define-global "3e" "#")
;; (key-chord-define-global "4r" "$")
;; (key-chord-define-global "5r" "%")
;; (key-chord-define-global "5t" "%")
;; (key-chord-define-global "6y" "^")
;; (key-chord-define-global "6t" "^")
;; (key-chord-define-global "7y" "&")
;; (key-chord-define-global "8u" "*")
;; (key-chord-define-global "9i" "(")
;; (key-chord-define-global "-p" "_") ;; Gets annoying since works both ways

(key-chord-define-global "<<" 'smart-shift-left)
(key-chord-define-global ">>" 'smart-shift-right)

;; ;;;;;;;;;;;;;; ############ FIXME TODO ;;;;;;;;;;;;;; ############
;; ;;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/
(defun word-histogram-region (posBegin posEnd)
  "Display word histogram showing frequency of word occurrence."
  (interactive "r")
  (message "Counting...")
  (let* ((ht (make-hash-table :test 'equal))
	 (totals '()))
    (save-excursion
      (goto-char posBegin)
      (while (and (< (point) posEnd)
		  (re-search-forward "\\w+\\W*" posEnd t))
	(puthash (match-string 0) (1+ (gethash (match-string 0) ht 0)) ht)))
    (maphash (lambda (key value)
	       (setq totals (cons (list key value) totals)))
	     ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Word histogram"
      (princ (format "%d different words\n\n"
		     (length totals)))
      (dolist (item totals)
	(let
	    ((key (car item))
	     (count (cadr item))
	     (maxcount (cadr (car totals))))
	  (princ (format "%2d %20s %s\n" count key
			 (make-string (/ (* count (min 36
						       maxcount)) maxcount) ?+))))))))

(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((ht (make-hash-table :test 'equal))
	 (number-of-buffers (loop for buffer being the buffers
				  for mode-name = (symbol-name (buffer-local-value 'major-mode buffer))
				  do (incf (gethash mode-name ht 0))
				  count 1))
	 (totals (sort (loop for key being the hash-keys of ht
			     using (hash-values value)
			     collect (list key value))
		       (lambda (x y) (if (eql (second x) (second y))
					 (string-lessp (first x) (first y))
				       (> (second x) (second y)))))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
		     number-of-buffers (length totals)))
      (loop for (key count) in totals
	    do (princ (format "%2d %20s %s\n"
			      count
			      (if (equal (substring key -5) "-mode")
				  (substring key 0 -5) key)
			      (make-string count ?+)))))))


;; wc-mode to display chars, words, lines in mode-line
(autoload 'wc-mode "wc-mode" "Toggle word-count mode." t)
;; Define something else that shows total lines count in the modeline,
;; probably via count-lines-page ;;;;;;;;;; ####### FIXME TODO
(autoload 'amory-lc-mode "amory-lc-mode" "Toggle line-count mode." t)


(defun kf-reverse-lines-region (b e)
  "Reverse the order of lines containing B (inclusive) to E (exclusive)."
  (interactive "r")
  ;; There are two ways to do this: the Emacs way, and the easy way.
  ;; We're going to do it the easy way.
  (save-excursion
    (let ((lines ())
	  (b (progn (goto-char b) (beginning-of-line) (point)))
	  (e (progn (goto-char e) (beginning-of-line) (point))))
      (goto-char b)
      (while (< (point) e)
	(setq lines
	      (cons
	       (buffer-substring (point) (progn (forward-line 1) (point)))
	       lines)))
      (delete-region b e)
      (mapcar 'insert lines))))

(defun kf-randomize-region (b e)
  (interactive "*r")
  (save-excursion
    (apply
     'insert
     (sort (split-string (delete-and-extract-region b e) "\\b")
	   (function (lambda (a b) (> (random 2) 0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulate lines and comments, move around
;; ;;;;;; ##### FIXME TODO
(require 'amory-manipulate)
;;;;;;;;;;; FIXME TODO ###########
;; A bunch of reference functions, mainly from:
;; http://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Includes genetic code stuff that needs to be fixed like whoa
(require 'amory-reference-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fixme-insert ()
  "Signal something that needs to be dealt with."
  (interactive)
  (insert "\;\;\;\;\;\; \#\#\#\#\# F\IXME T\ODO"))
(defalias 'insert-fixme 'fixme-insert)

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
	  "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
	  "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
	  "aliquip ex ea commodo consequat. Duis aute irure dolor in "
	  "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
	  "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
	  "culpa qui officia deserunt mollit anim id est laborum."))
(defalias 'ipsum 'lorem)

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

;; (defun my-tab-fix ()
;; (global-set-key "\t" 'indent-or-expand))
;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; (add-hook 'perl-mode-hook 'my-tab-fix)
;; (add-hook 'sh-mode-hook 'my-tab-fix)

;; Same thing
;; (global-set-key "\t" 'indent-or-expand)
;; (global-set-key (kbd "TAB") 'indent-or-expand)
;; (global-set-key (kbd "C-SOMETHING") 'indent-or-expand)


;; Neat trick, should use somehow...
;; https://stackoverflow.com/a/18407606/2521092
(defvar script-name "~/bin/marketupdate.sh")
(defun call-my-script-with-word ()
  (interactive)
  (shell-command
   (concat script-name " " (thing-at-point 'word))))


;; Diminish
;; Hide extraneous minor modeline crap I don't like
;; At the end so I nothing throws an error
(require 'diminish)
(diminish 'auto-complete-mode "ac")
(diminish 'flymake-mode "Fly")
(diminish 'isearch-mode)
;; (diminish 'jiggle-mode)
(diminish 'abbrev-mode "Abv")
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)
(diminish 'auto-fill-function)
;; (diminish 'font-lock-mode "Fn")
(diminish 'visual-line-mode "vl")
(diminish 'fic-mode)
(diminish 'whole-line-or-region-mode)
(diminish 'highlight-parentheses-mode)
(diminish 'undo-tree-mode)
(diminish 'highlight-symbol-mode "hls")
(diminish 'volatile-highlights-mode)
(diminish 'anzu-mode)
(diminish 'ace-jump-mode "Ace")
(diminish 'guide-key-mode)
(diminish 'fancy-narrow-mode)
(diminish 'subword-mode)
;; Diminish doesn't work here??  Dumb
(setq eldoc-minor-mode-string nil)

;; Function to shorten major modes in modeline
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
	(setq mode-name ,new-name))))

(rename-modeline "lisp-mode" emacs-lisp-mode "Elisp")
(rename-modeline "sh-script" sh-mode "Shell")

;; Follow-mode on the mode line
;; https://stackoverflow.com/q/11326350/2521092
(add-to-list 'minor-mode-alist '(follow-mode " follow"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(smex-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ted-read-major-mode ()
  "Read a major mode from the user, and return it.
Based on Kevin Rogers' `edit-region' interactive spec."
  (intern (completing-read
	   (format "Major mode (default `%s'): " major-mode)
	   obarray 'kr-major-mode-p t nil nil
	   (symbol-name major-mode))))

(defun kr-major-mode-p (symbol)
  "Return non-nil if SYMBOL is a major mode.
Used in `interactive' forms to read major mode names from the user."
  (and (fboundp symbol)
       (let ((function-name (symbol-name symbol)))
	 (and (string-match "-mode\\'" function-name)
	      (not (string-match "\\`turn-\\(on\\|off\\)-"
				 function-name))))
       (not (assq symbol minor-mode-alist))))


(defun ted-kill-mode-buffers (&optional mode)
  "Kill all buffers of this major mode.
With optional argument MODE, all buffers in major mode MODE are killed
instead."
  (interactive (list (when current-prefix-arg (ted-read-major-mode))))
  (setq mode (or mode major-mode))
  (when (or current-prefix-arg
	    (y-or-n-p (format "Really kill all %s buffers? " mode)))
    (mapc (lambda (buffer)
	    (when (with-current-buffer buffer
		    (eq major-mode mode))
	      (kill-buffer buffer)))
	  (buffer-list))))


(defun ted-find-mode (extension &optional interactive)
  "Returns the mode in which a file with EXTENSION would be opened."
  (interactive "sExtension: \np")
  (let ((mode (assoc-default (concat "." extension) auto-mode-alist
			     'string-match default-major-mode)))
    (when interactive
      (message "A file with extension .%s would be opened with mode %s"
	       extension mode))
    mode))

(defun ted-next-warning ()
  "Advance to the next buffer location in `font-lock-warning-face'."
  (interactive)
  (let ((here (point)))
    (condition-case nil
	(progn
	  (goto-char (next-property-change (point)))
	  (while (not (memq (get-text-property (point) 'face)
			    '(font-lock-warning-face
			      js2-error-face js2-warning-face)))
	    (goto-char (next-property-change (point)))))
      (error (goto-char here)
	     (error "There are no more warnings in the buffer!")))))



;; ;; use setq-default to set it for /all/ modes
;; (setq mode-line-format
;;	      (list
;;	       ;; the buffer name; the file name as a tool tip
;;	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
;;				   'help-echo (buffer-file-name)))

;;	       ;; line and column
;;	       "(" ;; '%02' to set to 2 chars at least; prevents flickering
;;	       (propertize "%02l" 'face 'font-lock-type-face) ","
;;	       (propertize "%02c" 'face 'font-lock-type-face)
;;	       ") "

;;	       ;; relative position, size of file
;;	       "["
;;	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above
;;	       ;; top
;;	       "/"
;;	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
;;	       "/"
;;	       (propertize (number-to-string (line-number-at-pos (point-max))) 'face 'font-lock-constant-face)
;;	       "] "

;;	       ;; the current major mode for the buffer.
;;	       "["

;;	       '(:eval (propertize "%m" 'face 'font-lock-string-face
;;				   'help-echo buffer-file-coding-system))
;;	       "] "


;;	       ;; "[" ;; insert vs overwrite mode, input-method in a
;;	       ;; ;; tooltip
;;	       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;;				   'face 'font-lock-preprocessor-face
;;				   'help-echo (concat "Buffer is in "
;;						      (if overwrite-mode "overwrite" "insert") " mode")))

;;	       ;; was this buffer modified since the last save?
;;	       '(:eval (when (buffer-modified-p)
;;			 (concat ","  (propertize "Mod"
;;						  'face 'font-lock-warning-face
;;						  'help-echo "Buffer has been modified"))))

;;	       ;; is this buffer read-only?
;;	       '(:eval (when buffer-read-only
;;			 (concat ","  (propertize "RO"
;;						  'face 'font-lock-type-face
;;						  'help-echo "Buffer is read-only"))))
;;	       "] "

;;	       ;; add the time, with the date and the emacs uptime
;;	       ;; in the tooltip
;;	       '(:eval (propertize (format-time-string "%H:%M")
;;				   'help-echo
;;				   (concat (format-time-string "%c; ")
;;					   (emacs-uptime "Uptime:%"))))
;;	       " --"
;;	       ;; i don't want to see minor-modes; but if you want,
;;	       ;; uncomment this:
;;	       ;; minor-mode-alist  ;; list of minor modes
;;	       "%-" ;; fill with '-'
;;	       ))



;; ;; use setq-default to set it for /all/ modes
;; (setq mode-line-format
;;       (list
;;        ;; the buffer name; the file name as a tool tip
;;        '(:eval (propertize "%b " 'face 'font-lock-keyword-face
;;			   'help-echo (buffer-file-name)))

;;        ;; line and column
;;        "(" ;; '%02' to set to 2 chars at least; prevents flickering
;;        (propertize "%02l" 'face 'font-lock-type-face) ","
;;        (propertize "%02c" 'face 'font-lock-type-face)
;;        ") "

;;        ;; relative position, size of file
;;        "["
;;        (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
;;        "/"
;;        (propertize "%I" 'face 'font-lock-constant-face) ;; size
;;        "] "

;;        ;; the current major mode for the buffer.
;;        "["

;;        '(:eval (propertize "%m" 'face 'font-lock-string-face
;;			   'help-echo buffer-file-coding-system))
;;        "] "


;;        "[" ;; insert vs overwrite mode, input-method in a tooltip
;;        '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;;			   'face 'font-lock-preprocessor-face
;;			   'help-echo (concat "Buffer is in "
;;					      (if overwrite-mode "overwrite" "insert") " mode")))

;;        ;; was this buffer modified since the last save?
;;        '(:eval (when (buffer-modified-p)
;;		 (concat ","  (propertize "Mod"
;;					  'face 'font-lock-warning-face
;;					  'help-echo "Buffer has been modified"))))

;;        ;; is this buffer read-only?
;;        '(:eval (when buffer-read-only
;;		 (concat ","  (propertize "RO"
;;					  'face 'font-lock-type-face
;;					  'help-echo "Buffer is read-only"))))
;;        "] "

;;        ;; add the time, with the date and the emacs uptime in the tooltip
;;        '(:eval (propertize (format-time-string "%H:%M")
;;			   'help-echo
;;			   (concat (format-time-string "%c; ")
;;				   (emacs-uptime "Uptime:%"))))
;;        " --"
;;        ;; i don't want to see minor-modes; but if you want, uncomment
;;        ;; this:
;;        ;; minor-mode-alist  ;; list of minor modes
;;        "%-" ;; fill with '-'
;;        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Another modeline thingy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun init-el-setup-mode-line ()
;;   (setq-default
;;    mode-line-format
;;    (list
;;     " "
;;     (propertize "%b" 'face 'font-lock-keyword-face)
;;     " ("
;;     (propertize "%02l" 'face 'font-lock-type-face)
;;     ","
;;     (propertize "%02c" 'face 'font-lock-type-face)
;;     ") ["
;;     (propertize "%p" 'face 'font-lock-constant-face)
;;     "/"
;;     (propertize "%I" 'face 'font-lock-constant-face)
;;     "] ["
;;     (propertize "%m" 'face 'font-lock-string-face)
;;     "] ["
;;     `(:eval (,(lambda ()
;;		(propertize (symbol-name buffer-file-coding-system)
;;			    'face 'font-lock-builtin-face))))
;;     "] ["
;;     `(:eval (,(lambda ()
;;		(mode-line-status-list
;;		 ((buffer-modified-p) "Mod" font-lock-warning-face)
;;		 (buffer-read-only "RO" font-lock-type-face)
;;		 ((buffer-narrowed-p) "Narrow" font-lock-type-face)
;;		 (defining-kbd-macro "Macro" font-lock-type-face)))))
;;     "]")))

;;;;;;; END
;; (when (require 'time-date nil t)
;;   (message "Emacs startup time: %d seconds." (time-to-seconds (time-since emacs-load-start-time))))
(message "Emacs loaded at %s." (wm-format-time-string "%T %a %d %b %y"))
