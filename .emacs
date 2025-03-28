;;; .emacs -*- lexical-binding: t; -*-

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
;; "Anyone with a 4-line .emacs file is suspicious." -Dino Chiesa             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HISTORY IDEAS FIXME TODO
;; history-length
;; list-command-history-max
;; savehist-minibuffer-history-variables
;; dmm/discover-my-mode-history
;; Man-topic-history
;; flycheck-read-checker-history
;; imenu--history-list
;; regexp-history
;; read-expression-history
;; face-name-history
;; buffer-name-history
;; read-envvar-name-history
;; shell-command-history
;; read-number-history
;; file-name-history
;; query-replace-history
;; minibuffer-history
;; extended-command-history
;; ido-file-history
;; ido-buffer-history
;; search-ring
;; regexp-search-ring

;; Notes

;; C-h b for all bindings
;; Or M-x where-is function
;; C-h a regex, search for that in commands ie anything with rot13
;; C-x z, repeat last command
;; M-x man, man in emacs
;; C-j, newline
;; C-m, newline-and-indent
;; C-h l, last inputs
;; C-u M-!, insert shell command into buffer
;; C-x 8 C-h, list of special characters; RET to enter by name

;; http://www.emacswiki.org/emacs/EmacsNewbieKeyReference
;; http://www.rgrjr.com/emacs/emacs_cheat.html
;; http://www.emacswiki.org/emacs/Reference_Sheet_by_Aaron_Hawley
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexp-Backslash.html

;; Should really look into and make use of f, s, dash
;; Am I just going to keep improving this until it matches spacemacs?

;; Of note, toolforge is on 28.2 aka `libgnutls-version' 30709
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; BEGIN
;; Enter debugger on error
(setq debug-on-error t
      init-file-debug t)

;; Don't be stingy on the memory, we have lots now. It's the distant future.
;; 1GB, see also https://akrl.sdf.org and most-positive-fixnum.  Has big
;; speedups for startup, fwiw.  Should just do GCMH.el FIXME TODO
(setq gc-cons-threshold (* 1 1024 1024 1024)
      gc-cons-percentage 0.2
      ;; Increase?  Changed to 65536 in 30.1
      read-process-output-max (* 1 1024 1024))


;; UTF-8 always, always, always
(set-default-coding-systems 'utf-8)
(setq-default locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
;; (setq-default buffer-file-coding-system 'utf-8-unix)

;; Local lisp, will take precedence over elpa directory
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
;; Prefer newer files even if not .elc
(setq load-prefer-newer t)

;; Customizations from within emacs interface; set to `null-device' to turn off?
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Prepare for themes
(setq
 ;; Custom theme directory, in case
 custom-theme-directory (expand-file-name "themes" user-emacs-directory)
 ;; Treat all themes as safe
 custom-safe-themes t)

;; Define a hook to run after we load a theme, pretty useful!  But for real, I
;; need to get over my hesitancy to use custom.el and just put faces there
;; sometimes and not over-use this.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.

See also `enable-theme-functions' and `disable-theme-functions'")
(advice-add 'load-theme :after
	    (lambda (&rest _)
	      "Run `after-load-theme-hook'."
	      (run-hooks 'after-load-theme-hook)))


;;;;;; Modeline customizations ;;;;;;
;; Probably kind of pointless given I'm manually defining the format below?

;; Time
;; Lots more to customize FIXME TODO
(require 'time)
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode t)

;; Battery percentage
(require 'battery)
(when (not (equal "Battery status not available" (battery)))
  (display-battery-mode)
  (setq battery-update-interval 240 ;; Default 60s
	battery-mode-line-limit 85  ;; Default 100
	battery-load-low 25	    ;; Default 25
	battery-load-critical 10))  ;; Default 10

;; Show column-number, size in the mode line
(column-number-mode 1) ; performance hit?
(size-indication-mode t)


;; Tighten things up when there's not enough space.  At least, in theory; in
;; practice, it doesn't like the ending "%-" aka "fill with -"
;; (setq mode-line-compact 'long)

;; Need to define this ahead of time, so I can apply it to the mode-line section
;; here, then make use of it later in the flycheck section.
;; Weird, can I do just the text, not background?  Or, need to customize I
;; guess, that's a flycheck-color-mode-line thing I think FIXME TODO
(defface flycheck-mode-line-color-face
  '((t))
  "Face with which to color the Flycheck mode-line status text.
 Makes use of flycheck-color-mode-line-mode and setting
 `flycheck-color-mode-line-face-to-color'."
  :group 'flycheck-faces)

;;; Actual Modeline custom format
;; CONSIDER: Colors faded when inactive, see emacs se but see also
;; `mode-line-active' and `mode-line-inactive'
;; Consider using timu-macos colors for more variety? TODO
;; Git:main?  or eh?  See also vc faces in customize
;;; https://stackoverflow.com/q/28468975/2521092
;;; https://emacs.stackexchange.com/q/17439/2051
;;; https://kitchingroup.cheme.cmu.edu/blog/2014/09/19/A-git-status-Emacs-modeline/
;; Can also do (for the above) global-mode-string
;; Also: follow-mode.  Check out minor-mode-alist for other ideas
;; which-function-mode yeah for sure TODO
;; FOR REFERENCE: https://www.emacswiki.org/emacs/ModeLineConfiguration
;; Also: https://www.gnu.org/software/emacs/manual/html_node/elisp/_0025_002dConstructs.html
;; Also: https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html
;; Check out moody? TODO
(setq-default mode-line-format
      (list
       ;; Error if memory full?
       "%e"

       ;; window-numbering-mode will insert itself here, which is fine.  Uses
       ;; window-numbering-face, which is also fine, but I've set it to
       ;; mode-line-buffer-id, in order to be bold, which is also fine.  The
       ;; main point is that by using the various built-in mode-line faces, it
       ;; dims when inactive, which is ideal.

       ;; Toss in @ if we're editing via emacsclient server, otherwise -, the
       ;; former borrowed from `mode-line-remote'.  Serves as sort of a spacer
       ;; that recapitulates `mode-line-client' in the place of
       ;; `mode-line-front-space'.  See also `(daemonp)'
       '(:eval (propertize
		(if (frame-parameter nil 'client)
		    ;; This should be subtle since it's technically
		    ;; unnecessary
		    "@" "-") 'face 'font-lock-comment-face))

       ;; Might as well?  Make it really clear
       '(:eval (propertize
		(if defining-kbd-macro
		    "-K-" "") 'face 'font-lock-escape-face))


       ;; Buffer name.  Not using mode-line-buffer-identification but doing it
       ;; manually means that the mode-line-buffer-id face gets separated from
       ;; this.  Maybe that's not so bad?!  It'd be nice to truncate the buffer
       ;; name (e.g. `(format-mode-line '(-20 "%b ") font-lock-keyword-face)' or
       ;; whatever) but it's a drag for helpful mode, etc.  Not sure if there's
       ;; a good way to have this be mode-specific, or if it's worth it. TODO
       ;; Additionally, I want different a different face depending on whether
       ;; we're in light or night mode.  Also, this should really handle the
       ;; case where timu-macos-theme isn't available FIXME
       '(:eval (if (equal timu-macos-flavour "dark")
		   (propertize "%b " 'face 'font-lock-function-name-face)
		 (propertize "%b " 'face 'font-lock-keyword-face)))



       ;; The below largely amounts to a more concise way of doing
       ;; `mode-line-position', giving line and column and percent.

       ;; Line and (1-based) column.  Could do '%02' to always do two
       ;; characters, but it'll change (especially the line counter) at three
       ;; digits anyway, so I don't think I really care that much.
       "("
       (propertize "%l" 'face 'font-lock-keyword-face)
       ","
       (propertize "%02C" 'face 'font-lock-keyword-face)
       ") "
       ;; Position and file length/size
       "["
       ;; %p is percent of buffer above top of window (or Top/Bot/All), and %o
       ;; is travel?  Automatically accounts for two characters?  Ugh.  %q is
       ;; neat, does the range
       (propertize "%o" 'face 'font-lock-constant-face)
       ;; Total count of lines, formatted nicely
       '(:eval (concat "/" (propertize (file-size-human-readable (line-number-at-pos (point-max)) 'si) 'face 'font-lock-constant-face)))
       ;; Size in bytes, abbreviated.  Not particularly useful?
       ;; (concat "/" (propertize "%I" 'face 'font-lock-constant-face))
       "] "


       "["
       ;; The current major mode for the buffer; `mode-line-modes' is too much
       ;; Use builtin-face? FIXME Actually, reassess all the below faces TODO
       ;; '(:eval (propertize "%m" 'face 'font-lock-string-face))
       ;; '(:eval (propertize "%m" 'face 'font-lock-builtin-face))
       '(:eval (propertize "%m" 'face 'font-lock-keyword-face))
       ;; Should these be turned off for dashboard, paradox?  Weird to see 'em
       ;; there.... FIXME TODO

       ;; Was this buffer modified since the last save?  Differs from `mode-line-modified'
       '(:eval (when (buffer-modified-p)
		 (concat ","  (propertize "Mod" 'face 'font-lock-constant-face))))
       ;; A few other modes worth noting
       '(:eval (when defining-kbd-macro
		 (concat ","  (propertize "Macro" 'face 'font-lock-constant-face))))
       ;; Is this buffer read-only?
       '(:eval (when buffer-read-only
		 (concat ","  (propertize "RO" 'face 'font-lock-type-face))))
       ;; %n introduces a leading space, so do this instead
       '(:eval (when (buffer-narrowed-p)
		 (concat ","  (propertize "Nar" 'face 'font-lock-type-face))))
       '(:eval (when (bound-and-true-p follow-mode)
		 (concat ","  (propertize "Fol" 'face 'font-lock-type-face))))
       "]"

       ;; Flycheck status, manually since not using minor-mode-alist and because
       ;; it's smort.
       ;; Can't remove the annoying leading space?  Ugh.
       ;; What I want: gone when nothing? No leading space. Color.
       ;; Could put up with major mode?  Eh.  Wrap in <>?  Would have to
       ;; redefine 'cause of the space, but I might have to anyway... TODO
       ;; Maybe also include info in modeline?  Otherwise colored but no number,
       ;; is that weird?  Or expected?
       '(:eval (when (and
		      flycheck-mode
		      ;; Confirm flycheck isn't running but without a valid
		      ;; checker configuration
		      (not (string= "no-checker" flycheck-last-status-change))
		      (not (string= "not-checked" flycheck-last-status-change)))
		 ;; (flycheck-mode-line-status-text)))
		 (propertize (flycheck-mode-line-status-text) 'face 'flycheck-mode-line-color-face)))
       " "

       ;; Add the time, date, and emacs server uptime (maybe dumb).  The uptime
       ;; is dependent on server status, and although I like the emacsclient @
       ;; above (a la mode-line-client), this does the same thing, really, and
       ;; it's not super necessary.  The date/time/battery largely recapitulates
       ;; global-mode-string, but with some more sensible spacing and
       ;; arrangement.
       '(:eval (propertize (format-time-string "%R %a %h %-d")))
       '(:eval (when (frame-parameter nil 'client)
		 (concat ", Up " (emacs-uptime "%D, %z%h:%.2m"))))
       ;; This format always seems simpler but more opaque.  Spaces included.
       '(battery-mode-line-string (" " battery-mode-line-string))

       "--"

       ;; List of minor modes.  I don't really want them, but maybe it's useful to note them here?
       ;; Diminish still removes most of them, but maybe it's kind of
       ;; unnecessary?  In theory, though, would be good to know when certain
       ;; things are on or off...
       ;; minor-mode-alist

       mode-line-end-spaces ;; fill with '-'
       ))


;; Time.  Pointless given the above?
;; (setq display-time-day-and-date t
;;       display-time-24hr-format t
;;       display-time-default-load-average nil)
;; (display-time-mode t)

;; Show column-number, size in the mode line.  Both pointless given the above
;; (column-number-mode 1) ; performance hit?
;; (size-indication-mode t)


;; Add macOS keychain to auth sources?
(add-to-list 'auth-sources 'macos-keychain-generic)
(add-to-list 'auth-sources 'macos-keychain-internet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa" . 9)
	("gnu" . 6)
	("nongnu" . 3)))
;; Only include releases
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Manual package installs, ideally some can be removed
;; 'hide-comnt
;; 'keep-buffers
;; 'keywiz
;; 'kill-ring-ido
;; 'perltidy
;; 'pick-backup
;; 'tidy
;; 'u-mandelbrot

;; Packages and paradox customizations in custom.el
;; Set by custom rather than by hand to make installation easier

;; This must come before configurations of installed packages
(unless package--initialized
  (package-initialize))

;; Get list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Then install any missing ones, as long as they're compatible
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (if (assoc pkg package-archive-contents)
	(let* ((pkg-desc (cadr (assoc pkg package-archive-contents)))
	       (reqs (package-desc-reqs pkg-desc))
	       (emacs-req (assoc 'emacs reqs))
	       (min-version (and emacs-req (cadr emacs-req)))
	       (compatible (or (not min-version)
			       (version-list-<= min-version
						(version-to-list emacs-version)))))
	  (if compatible
	      (condition-case err
		  (package-install pkg)
		(file-error
		 (message "Error installing package %s: %s" pkg (error-message-string err))))
	    (message "Package %s requires Emacs %s or newer (you have %s). Skipping."
		     pkg
		     (package-version-join min-version)
		     emacs-version)))
      (message "NOTE: Package %s is not available in the archives." pkg))))


;; Should probably install auto-package-update
;; https://github.com/rranelli/auto-package-update.el

;; use-package https://github.com/jwiegley/use-package
;; install (if not already present) and require, must be around for everyone else
;; Should actually use this... FIXME TODO
;; (dolist (package '(use-package))
;;   (unless (package-installed-p package)
;;     (package-install package)
;;     (require 'use-package)
;;     (setq use-package-always-ensure t
;;	  use-package-verbose t
;;	  ;; In theory, calling `use-package-report' would display some info
;;	  use-package-compute-statistics t)))


;; paradox https://github.com/Malabarba/paradox
(require 'paradox)
(setq paradox-github-token
      ;; Stored in a place like ~/.authinfo.gpg, ~/.authinfo, etc.  See
      ;; https://github.com/Malabarba/paradox/issues/147#issuecomment-409336111
      ;; Off since the ability to automatically star (via
      ;; paradox-automatically-star) is slow and minimally valuable atm.
      ;; (cadr(auth-source-user-and-password "api.github.com" "paradox")))
      ;; Setting to t turns off asking
      t)

;; Quieter startup, see https://github.com/Malabarba/paradox/pull/183
;; (setq paradox-less-verbose t)
(defun paradox--override-definition (sym newdef)
  "Temporarily override SYM's function definition with NEWDEF.
Record that in `paradox--backups', but do nothing if
`paradox--backups' reports that it is already overriden."
  (unless (memq sym paradox--backups)
    (advice-add sym :override newdef '((name . :paradox-override)))
    (add-to-list 'paradox--backups sym)))

;; Enable sorting on the downloads column, https://github.com/Malabarba/paradox/pull/190
(paradox--define-sort paradox--column-name-download "d")

;; Refresh stars and downloads.  Probably only need to do once per session to
;; get around https://github.com/Malabarba/paradox/issues/176.  Can't seem to
;; hook this into something like `paradox-menu-mode-hook' or make use of
;; advice-add without causing errors, so just run manually for now.
;; (paradox--refresh-remote-data)

;; Can set a key, though.  Will only run if data not loaded
(defun my/paradox-menu-refresh-data ()
  "Refresh stars and downloads.  Will only run once."
  (interactive)
  ;; Hash table only populated once data refreshed
  (when (eq 0 (hash-table-count paradox--star-count))
    (paradox--refresh-remote-data)
    ;; Remove the mode-map definition now that we've run it once
    (define-key paradox-menu-mode-map "r" nil)))
(define-key paradox-menu-mode-map "r" #'my/paradox-menu-refresh-data)

;; Lately (since emacs 30+?) paradox has been throwing up an async error when
;; installing packages.  Paradox is old and creaky but I like it, so this (in
;; custom.el) is a placeholder until something better.  FIXME TODO

(paradox-enable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; themes ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I love the kaolin themes.  Some thoughts:
;; - ocean (low contrast, blues and greens; should turn background black)
;; - galaxy (high contrast, blues and greens; should turn background black)
;; - aurora (nah)
;; - bubblegum (eh.  Seems nice but blurs together)
;; - valley dark (like a better temple?)
;; - temple (okay?  if turn background black.  lotta pinks)
;; - dark (okay: low contrast, greens, kind of a low ocean)

;; Would use this were it not for timu-macos.  Still, should find a better
;; light-mode version of it.  Regardless, themes loaded automatically when using
;; auto-dark below.
;; (load-theme 'kaolin-galaxy)
(require 'timu-macos-theme)
(load-theme 'timu-macos)
;; There are a couple things I want tweaked in the timu theme, but differently
;; depending on whether we're in light or night mode, and at the moment,
;; timu-macos doesn't have a hook (it should!), so let's define some advice for
;; it.  We could define our own hook and have it run that, but, you know, six
;; and one half.  Right now it's different color highlighting (`region' face)
;; and not being unable to see orange in the light modeline.
(with-eval-after-load 'flycheck
  (advice-add 'timu-macos-toggle-dark-light :after
	      (lambda ()
		"Set face attribute for `region' based on `timu-macos-flavour'."
		;; equal not eq
		(if (equal timu-macos-flavour "dark")
		    (progn
		      (set-face-attribute 'region nil :background "color-237")
		      (set-face-attribute 'flycheck-fringe-warning nil :foreground 'unspecified :inherit 'timu-macos-orange-face))
		  (progn
		    (set-face-attribute 'region nil :background "color-252")
		    (set-face-attribute 'flycheck-fringe-warning nil :foreground 'unspecified :inherit 'timu-macos-yellow-face))))))


(require 'auto-dark)
(setq
 ;; ns-do-applescript not available.  Could also just set
 ;; `auto-dark-detection-method' to osascript, but this is the proper way
 auto-dark-allow-osascript t
 ;; The default of 5 seems like it should be fine, but it seems to have a quite
 ;; noticable slowdown.  Setting it higher isn't a big deal, so it's a
 ;; no-brainer.  Could definitely go higher!
 auto-dark-polling-interval-seconds 10
 ;; The timu-macos theme is weird, with just one actual theme but a function to
 ;; toggle between dark and light mode.  Thankfully, auto-dark offers hooks!
 ;; This is just in case?  Prior to 0.13 it was auto-dark-(dark|light)-theme
 auto-dark-themes '((timu-macos) (timu-macos)))
(add-hook 'auto-dark-dark-mode-hook #'timu-macos-toggle-dark-light)
(add-hook 'auto-dark-light-mode-hook #'timu-macos-toggle-dark-light)
;; I mean, whatever, but just in case I ever want to use the macOS application,
;; although it will need tweaking for emacs-plus or whatever.
(when (not window-system)
  (auto-dark-mode t))


;; Loading a theme is nice and all, but the previously enabled theme lingers,
;; which is kind of a drag.  This turns it off then enables the new one.
;; Happily taken from timu:
;; <https://macowners.club/posts/custom-functions-4-ui/#timu-ui-load-theme>
(defun swap-theme ()
  "`load-theme' without confirmation and with completion.
Disables the `custom-enabled-themes' first to start with a blank canvas."
  (interactive)
  (let ((next-theme
	 (completing-read "Load custom theme: "
			  (custom-available-themes))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern next-theme) t)))


;; When I was a child, I spake as a child,
;; I understood as a child, I thought as a child:
;; but when I became a man, I put away childish things.
;;   -- 1 Corinthians, 13:11
;; No menu bar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Prevent the startup message and splash screen
(setq inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen t)

;; But use a neat dashboard <https://github.com/emacs-dashboard/emacs-dashboard>
;; Options and functions:
;; <https://github.com/emacs-dashboard/emacs-dashboard/blob/master/docs/variables-and-functions.org>
(require 'dashboard)
;; Required for emacsclient
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

;; List files in current directory https://github.com/emacs-dashboard/dashboard-ls
;; (require 'dashboard-ls)
;; Doesn't work well for emacsclient since 'default-directory is set when emacs
;; is started.  Ideally, with something like the below (inspired by startup.el),
;; dashboard-ls could take a function to determine the working directory when
;; called, then do something like the above lambda here as well, but in practice
;; emacsclient doesn't have good, fresh access to the PWD on loading.  Maybe if
;; aliased to a function that takes the PWD when opening?  Awkward.

;; (let* ((path
;;	(cond ((stringp dashboard-ls-path)
;;	       (symbol-value 'dashboard-ls-path))
;;	      ((functionp dashboard-ls-path)
;;	       (funcall dashboard-ls-path))
;;	      ((booleanp dashboard-ls-path)
;;	       (symbol-value 'default-directory)))
;;	))
;;    ;; (message "%s" path)
;;    (setq dashboard-ls--record-path path))

;; Tip of the day taken from
;; https://github.com/emacs-dashboard/emacs-dashboard/issues/26 and
;; https://gist.github.com/saintaardvark/375aa054c15f02c42f45 Not perfect,
;; should ideally limit self to a modemap or something Would be neat to
;; customize the header message (`dashboard-banner-logo-title') with something
;; like fortune or my quotes, see also `dashboard-footer-messages' FIXME TODO
(defun totd()
  "Display a `Tip of the Day' message with a random command.
Used for insertion into the dashboard."
  (let* ((commands (cl-loop for s being the symbols
			    when (commandp s) collect s))
	 (command (nth (random (length commands)) commands)))
    (insert
     (format "** Tip of the day: **\nCommand: %s\n\n%s\n\nInvoke with: "
	     command (documentation command)))
    (where-is command t)))
(defun dashboard-insert-totd (list-size)
  "Shim to insert the Tip of the Day into the dashboard."
  (totd))
(add-to-list 'dashboard-item-generators '(totd . dashboard-insert-totd))

;; Expand recents; not using bookmarks or agenda, but keep here as reminder to
;; do so.  Should use projectile at some point
(setq dashboard-items '((recents  . 15)
			;; (ls-files . 5)
			;; (ls-directories . 3)
			(bookmarks . 5)
			;; (projects . 5)	; Depends on projectile
			;; (agenda . 5)
			;; (registers . 5)
			(totd . 1)))

;; Weird that this gets the startup time wrong with emacsclient, not sure of why
;; since it appears to be using the right hooks
(dashboard-setup-startup-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent if possible but complete otherwise
(setq-default tab-always-indent 'complete)

;; Start a split-below window at the same point.  Setting to nil shows more
;; information, and is what I've used for a while, but for someone reason this
;; feels more intuitive now, and I find I've been wanting it for a while.
(setq split-window-keep-point t)

;; Resize windows when splitting; annoying if want certain size, but let's be
;; honest, I usually want some balance.  Besides, on a small screen, it's not
;; a big difference, and on a big screen, it doesn't matter!
(setq-default window-combination-resize t)


;; ignore case when completing, including buffers and filenames
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Completion in mini-buffer
(icomplete-mode t)

;; Auto complete
;; http://cx4a.org/software/auto-complete/manual.html#Configuration
(require 'auto-complete-config)
(ac-config-default)
;; start automatically after 3, not 2 characters
(setq ac-auto-start 3)

;; Characters entered before started, up=efficient, down=slower
;; (setq ac-auto-start 5)			;Default 2
;; Turn auto menu off
;; (setq ac-auto-start nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yasnippet, loaded here to allow in hippie-expand
;; https://github.com/joaotavora/yasnippet and
;; https://github.com/AndreaCrotti/yasnippet-snippets
(require 'yasnippet)
(setq yas-wrap-around-region t		; Set region to $0
      yas-verbosity 2			; Fewer messages on startup
      yas-triggers-in-field t)		; Allow nested expansions
(define-key yas-minor-mode-map (kbd "C-c C-i") 'yas-insert-snippet) ; C-c tab
(yas-reload-all)
(yas-global-mode)


;; Default M-/ is `dabbrev-expand', but `hippie-expand' is better?  And then there's
;; `auto-complete'.  Hippie expand expands lines, kind of dabbrev but indiscriminate
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Unexpand.  Would be neat to have a `dabbrev-unexpand', sort of like
;; `unexpand-abbrev'
(defun hippie-unexpand ()
  "Unexpand a `hippie-expand' expansion."
  (interactive)
  (hippie-expand 0))
(global-set-key "\M-?" 'hippie-unexpand)

;; Sometimes hippie is a little TOO hip.  Reorder the list so that expand-line
;; and expand-list come much, much later, definitely after expand-dabbrev.
;; Consider searching `apropos-function' for `^try-'
(setq hippie-expand-try-functions-list '(yas-hippie-try-expand
					 try-expand-dabbrev
					 try-expand-dabbrev-visible
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-line
					 try-expand-list
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol
					 ))

;; Use ido for hippie-expand via C-c / -- not ideal.  Seems to swallow any
;; opening paren?  Seems also to be weird in elisp? FIXME TODO
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my-hippie-expand-completions)
	(last-command last-command)
	(buffer-modified (buffer-modified-p))
	(hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-letf (((symbol-function 'ding) #'ignore)) ; avoid the (ding) when hippie-expand exhausts its options.
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
	     (list (ido-completing-read "Completions: " options))
	   (list nil))))  ;; Ensure we always return a list
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

;; Default 10
(setq buffers-menu-max-size 30)

;; Subword mode (consider CamelCase chunks as words)
(global-subword-mode 1)


;; Get path from the system shell, not super necessary?
;; <https://github.com/purcell/exec-path-from-shell>
;; Could do something like
;; (require 'exec-path-from-shell)
;; (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
;;   (add-to-list 'exec-path-from-shell-variables var))
;; (when (daemonp)
  ;; (exec-path-from-shell-initialize))



;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
;; Set to personal map, so many options
(defvar my/multiple-cursors-map
  (let* ((map (make-sparse-keymap)))
    (define-key global-map (kbd "C-c m") map)
    (define-key map (kbd "a") 'mc/mark-all-like-this)
    (define-key map (kbd "n") 'mc/mark-next-like-this)
    (define-key map (kbd "p") 'mc/mark-previous-like-this)
    (define-key map (kbd "e") 'mc/edit-lines)
    (define-key map (kbd "r") 'mc/mark-all-in-region)
    (define-key map (kbd "d") 'mc/mark-all-dwim)
    (define-key map (kbd "C-a") 'mc/edit-beginnings-of-lines)
    (define-key map (kbd "C-e") 'mc/edit-ends-of-lines)
    (define-key map (kbd "x") 'mc/mark-more-like-this-extended)
    (define-key map (kbd "#") 'mc/insert-numbers)
    map)
  "Personal keymap for multiple cursors.")


;;;;;;;;;;;;;;;;;;;
;; JavaScript stuff
;; Use js2-mode instead of js-mode https://github.com/mooz/js2-mode
(require 'js2-mode)
(setq
 js-switch-indent-offset 8	    ; tab indent switch cases
 js-chain-indent t		    ; line up successive indents with .
 js-indent-level 2		    ; Not everyone's got an editorconfig, tighter
 js2-highlight-level 3		    ; highlight more built-in functions
 js2-mode-indent-ignore-first-tab t ; make first tab doesn't toggle between valid indents
 js2-strict-inconsistent-return-warning nil ; warning if both return and return foo
 js2-strict-trailing-comma-warning t)	    ; trailing commas in array/objects
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node\\|nodejs" . js2-mode))
;; js2-jump-to-definition takes this over, annoying given everyone else respects it
(define-key js2-mode-map (kbd "M-.") 'end-of-buffer)
;; Okay weird but maybe?  Lots of clobbering elsewhere...
(define-key js2-mode-map (kbd "C-c 2") 'js2-jump-to-definition)

;; Error checking can be slow on large files, slightly increase this
;; (setq js2-idle-timer-delay 0.5)
(setq js2-dynamic-idle-timer-adjust 10000)

;; Alternatively, just turn off error checking entirely and rely on flycheck's
;; javascript-eslint to let eslint handle things proper like.  Also js2-mode's
;; checker doesn't update for new errors?
(setq js2-mode-show-strict-warnings nil
      js2-mode-show-parse-errors nil)
;; If one weren't doing that and were letting js2-mode handle errors, make up
;; for the lack of eslint's `env' with the below.  Note that
;; `js2-include-browser-externs' is on by default.
;; (setq js2-include-node-externs t)	    ; require, exports, etc
;; Define jest, qunit, and mocha externals as with node.  Taken from
;; <https://github.com/sindresorhus/globals>, which is what eslint uses
(when js2-include-node-externs
  (defvar js2-qunit-externs
    (mapcar 'symbol-name
	    '(asyncTest deepEqual equal expect module notDeepEqual notEqual notOk
			notPropEqual notStrictEqual ok propEqual QUnit raises
			start stop strictEqual test throws))
    "QUnit externs.")
  (defvar js2-jest-externs
    (mapcar 'symbol-name '(afterAll afterEach beforeAll beforeEach describe expect
				    fdescribe fit it jest pit require test
				    xdescribe xit xtest))
    "Jest externs.")
  (defvar js2-mocha-externs
    (mapcar 'symbol-name
	    '(after afterEach before beforeEach context describe it mocha run
		    setup specify suite suiteSetup suiteTeardown teardown test
		    xcontext xdescribe xit xspecify))
    "Mocha externs.")
  (setq js2-global-externs (append js2-qunit-externs js2-jest-externs js2-mocha-externs)))


;; Part of js2-mode package
(require 'js2-imenu-extras)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(js2-imenu-extras-setup)
;; I *like* having them, but it's annoying that they're sorted first
(setq js2-imenu-show-other-functions nil)

;; js2-refactor https://github.com/magnars/js2-refactor.el
;; Requires yasnippet and multiple-cursors
;; Full list at https://github.com/magnars/js2-refactor.el#refactorings
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(setq js2r-prefered-quote-type 2)	; single, not double
(js2r-add-keybindings-with-prefix "C-c m")
;; js2-refactor does not work in a buffer that has Javascript parse
;; errors. This tells js2-mode to treat octothorpes and hashbangs as comments,
;; preventing them from causing parse errors
(setq js2-skip-preprocessor-directives t)

;; jsdoc https://github.com/mooz/js-doc
;; old and creaky, but okay enough for now.  Honestly, maybe yas would just be better?
(require 'js-doc)
(add-hook 'js2-mode-hook
	  #'(lambda ()
	      ;; clobbers js-set-js-context, whatever that did
	      (define-key js2-mode-map "\C-c\C-j" 'js-doc-insert-function-doc)
	      (define-key js2-mode-map "@" 'js-doc-insert-tag)))
;;;;;;;;;;;;;;;;;;;

;; emmet-mode expansions, super cool if I ever remember (use C-j)
;; https://github.com/smihica/emmet-mode
(autoload 'emmet-mode "emmet-mode")
(add-hook 'html-mode-hook  'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; json-mode https://github.com/joshwnj/json-mode
;; C-c C- map has fun things like reformatting, toggling booleans
;; Consider json-navigator https://github.com/DamienCassou/json-navigator
(autoload 'json-mode "json-mode" "Major mode for editing JSON files." t)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))


;; mhtml-mode probably better (default since 26), but some bug (maybe with
;; fic-mode?) makes it wig out. Regex taken from auto-mode-alist itself
(add-to-list 'auto-mode-alist '("\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . html-mode))

;; php-mode https://github.com/emacs-php/php-mode
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))


;; yaml-mode
(autoload 'yaml-mode "yaml-mode" "Simple mode to edit YAML." t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))


;; ess-mode aka R mode https://ess.r-project.org/
;; Should probably customize the linters used (like not whining about `=` for
;; assignment) but I don't use R enough to care, and certainly not in Emacs.
;; No need for flymake
(eval-after-load 'ess
  (setq ess-use-flymake nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flycheck stuff
;;; https://github.com/flycheck/flycheck and https://www.flycheck.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIXME TODO:
;;; Consider turning margin on?  Big change for everybody but maybe worth it long-term?
;;; Possible additional extensions: flycheck-inline, flycheck-elsa/elsa,
;;; flycheck-grammarly FIXME TODO
(require 'flycheck)
;; Turn on for everybody
(add-hook 'after-init-hook #'global-flycheck-mode)

;; I don't rightly care about ensuring lisps have the "proper" package comments
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(setq flycheck-emacs-lisp-load-path 'inherit
      flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch new-line mode-enabled)
      flycheck-relevant-error-other-file-minimum-level 'warning
      flycheck-standard-error-navigation t
      flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list
      flycheck-display-errors-delay 0.25
      flycheck-idle-buffer-switch-delay 2
      ;; Static analysis can be slow and delay text entry, besides rarely need
      ;; right away?  Maybe this is excessively cautious?
      flycheck-idle-change-delay 2
      ;; Pointless atm since no margin enabled...
      flycheck-indication-mode 'left-margin
      ;; '(flycheck-highlighting-mode 'lines
      ;; Slow in python mode, but neat regardless
      flycheck-highlighting-mode 'sexps

      flycheck-mode-line-prefix "Fly"	;default FlyC
      ;; Recently in <https://github.com/flycheck/flycheck/pull/2035> the
      ;; default behavior of no errors defaulted to showing ":0" so we can undo
      ;; that, but also maybe a checkmark is nice?  Not like me to use emoji.
      ;; flycheck-mode-success-indicator ""
      flycheck-mode-success-indicator "✔︎"

      flycheck-markdown-markdownlint-cli-config ".markdownlintrc"

      ;; Tell flycheck that sometimes Perl modules are in ./lib, or ../lib if
      ;; you're in a testing directory /t.  Used with perl -I
      flycheck-perl-include-path (list "./lib/" "../lib")
      flycheck-perlcritic-severity 2)

;; Some additional checkers; could probably just run these straight-up
(with-eval-after-load 'flycheck
  ;; Add hook to `recenter' frame after jumping to an error from the list, just
  ;; like `occur-mode-find-occurrence-hook'.  This seems to work just fine, and
  ;; I have the code written locally in list-jump-hook; I opened
  ;; https://github.com/flycheck/flycheck/issues/1874
  (defcustom flycheck-error-list-after-jump-hook nil
    "Functions to run after jumping to the error from the error list.

This hook is run after moving to the error.  A possible idea
is to adjust the frame to bring the full context into view.

This variable is a normal hook.  See Info node `(elisp)Hooks'."
    :group 'flycheck
    :type 'hook
    :risky t
    :package-version '(flycheck . "0.33"))

  (add-hook 'flycheck-error-list-after-jump-hook 'recenter)

  ;; Of course, it means overwriting `flycheck-error-list-goto-error'
  (defun flycheck-error-list-goto-error (&optional pos)
    "Go to the location of the error at POS in the error list.

POS defaults to `point'."
    (interactive)
    (when-let* ((error (tabulated-list-get-id pos)))
      (flycheck-jump-to-error error)))


  ;; flycheck-bashisms https://github.com/cuonglm/flycheck-checkbashisms
  ;; Ensure no bashisms in sh code, no shisms in bash code; mostly the former
  (add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup)
  (setq flycheck-checkbashisms-posix t
	flycheck-checkbashisms-newline t)
  ;; flycheck-relint https://github.com/purcell/flycheck-relint
  ;; Check elisp regexes with relint
  (flycheck-relint-setup)
  ;; flycheck-relint assumes that emacs-lisp-checkdoc is enabled, so we need to
  ;; manually set the checker to follow emacs-lisp
  (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-relint)

  ;; Enable flycheck-color-mode-line, with our custom face
  ;; https://github.com/flycheck/flycheck-color-mode-line
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (with-eval-after-load 'flycheck-color-mode-line
    (setq flycheck-color-mode-line-face-to-color 'flycheck-mode-line-color-face
	  flycheck-color-mode-line-show-running t
	  )))

;; Annoyingly, flycheck doesn't automatically include proselint as a
;; next-checker for markdown mode (okay, fine, I guess it makes sense), so let's
;; just make it so.  Clearly depends on the proselint executable (via pip,
;; macports, brew, etc). https://github.com/amperser/proselint/
;; Consider textlint as well?  Less popular/fewer stars, but more actively developed
;; https://github.com/textlint/textlint
(flycheck-add-next-checker 'markdown-markdownlint-cli 'proselint)

;; flycheck-hl-todo: Show fixmes and todos in flycheck
;; https://github.com/alvarogonzalezsotillo/flycheck-hl-todo
;; Should really figure out hl-todo below before turning this on, the two are
;; connected TODO
;; (require 'flycheck-hl-todo)
;; Should create a toggle function to disable via variable
;; `flycheck-hl-todo-enabled' rather than using flycheck-disable-checker

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; relint: Check elisp regexes https://elpa.gnu.org/packages/relint.html
;; `relint-buffer' is a better name than `relint-current-buffer', but that's taken
(defalias 'relint-buffer-current 'relint-current-buffer)
;; Good place to note pcre2el, mainly via rxt-mode then C-c /
;; Maybe add hook to elisp and perl modes? TODO
;; https://github.com/joddie/pcre2el

;; editorconfig https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(require 'editorconfig)
(editorconfig-mode 1)

;; Whitespace
(require 'whitespace)
;; Turn on globally, probably if better just for programming and text modes
;; (global-whitespace-mode t)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'text-mode-hook 'whitespace-mode)
;; Highlight empty lines, trailing whitespace, inappropriate indentation
;; trailing lines (lines or lines-tail) gets annoying, redundant to fci-mode
;; big-indent nice idea, but too quick to become an issue
(setq whitespace-style '(face empty trailing indentation space-before-tab space-after-tab))

;; Not needed in emacsen >= 23 because of lines-tail above
;; Auto-color lines over 80 in length .\{81\}
;; M-x highlight-lines-matching-regex or C-x w l
;; Unhighlight with unhighlight-regex or C-x w r
;; (add-hook 'emacs-lisp-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-green-b)))
;; (add-hook 'perl-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-red-b)))


;;; Highlight TODOs, FIXMEs, etc.
;; There are a lot of ways to do this.  Previously I used fic-mode
;; (<https://github.com/lewang/fic-mode>) which is fairly bare-bones.  It looks
;; like hl-todo (<https://github.com/tarsius/hl-todo>) is better
;; (more customization options, ability to jump to each in turn), as is
;; hl-prog-extra, but, honestly, I kind of just like the simple method here.  In
;; the end, that means something roughly at the level of fic-mode, but probably
;; less-well integrated.  If I really need something more full-fledged, I can
;; just use hl-todo, with something like the below:
;; (setq hl-todo '(t (:foreground "#e55c7a" :weight normal)))
;; (setq hl-todo-wrap-movement t)
;; (setq hl-todo-keyword-faces
;;       '(("TODO"  . "pink")
;;	("FIXME" . "#cc9393")
;;	("XXX"   . "#1E90FF")))
(defun my/add-watchwords ()
  "Function to highlight specific watchwords, rather than use `fic-mode' or
`hl-todo' or something."
  (font-lock-add-keywords
   ;; \\<, \\> are empty string at beginning, end of word
   nil '(("\\<\\(FIXME\\|TODO\\|XXX\\)\\>"
	  ;; 1 '((:inherit 'font-lock-keyword-face) (:weight bold)) t))))
	  ;; 1 '((:foreground (face-foreground 'font-lock-keyword-face)) (:weight bold)) t))))
	  ;; Want to inherit the foreground from font-lock-keyword-face, but not
	  ;; sure how to get that!  These don't work FIXME TODO
	  1 '((:foreground "pink") (:weight bold)) t))))
(add-hook 'prog-mode-hook 'my/add-watchwords)
(add-hook 'text-mode-hook 'my/add-watchwords)

;; FIXME
(font-lock-add-keywords 'emacs-lisp-mode '(("autoload" . font-lock-keyword-face)))


;; https://github.com/dgutov/highlight-escape-sequences
;; (require 'highlight-escape-sequences)
;; (setq hes-simple-modes '(emacs-lisp-mode))
;; (hes-mode)


;; Color for backslash and escapes in lisp regexps,
;; `font-lock-regexp-grouping-backslash' and
;; `font-lock-regexp-grouping-construct', set in custom.el.  Wish cperl-mode had
;; something similar...


;; (require 'applescript-mode)


;; Make life easier, in all lisps.  As of Emacs 29 we can do
;; `elisp-eval-region-or-buffer', which is bound by default to C-c C-e, so while
;; I'd rather do C-x e, it's just easier to keep these the same.
(if (fboundp 'elisp-eval-region-or-buffer)
    (progn
      (define-key lisp-mode-shared-map (kbd "C-c e") 'elisp-eval-region-or-buffer)
      (define-key lisp-mode-shared-map (kbd "C-c C-e") 'elisp-eval-region-or-buffer))
  (progn
      (define-key lisp-mode-shared-map (kbd "C-c e") 'eval-region)
      (define-key lisp-mode-shared-map (kbd "C-c C-e") 'eval-buffer)))


;; Use buffer name as window title for window-system
;; (setq frame-title-format "%b - emacs")
;; (setq frame-title-format '(buffer-file-name "%f" ("%b")))
;; (let ((name (assq 'name default-frame-alist)))
;;   (when (consp name)
;;     (setcdr name ())))
;; (modify-frame-parameters (selected-frame) '((name)))

;; Send buffer name to xterm directly to rename tabs, from:
;; https://www.emacswiki.org/emacs/FrameTitle#h5o-6
;; Behaves somewhat badly when multiple emacsclient instances are in use
(defun xterm-title-update ()
  "Rename terminal tabs by sending the buffer name to xterm directly."
  (interactive)
  ;; xterm escape sequences: https://tldp.org/HOWTO/Xterm-Title-3.html
  ;; 0: icon name and window title, 1 icon name, 2 window title
  (send-string-to-terminal (concat "\033]1; " (buffer-name) "\007"))
  (if buffer-file-name
      (send-string-to-terminal (concat "\033]2; " (buffer-file-name) "\007"))
    (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007"))))
(add-hook 'post-command-hook 'xterm-title-update)

;;; Autosave and backup
;; create a backup file directory.  ` rather than ' needed to selectively
;; evaluate item marked by , <https://emacs.stackexchange.com/a/7487/2051>
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
;; Save every on inputs and idle
(setq auto-save-interval 300 ; default 100
      auto-save-timeout 90   ; default 30
      auto-save-default t
      ;; Versions
      delete-old-versions t
      backup-by-copying t
      kept-new-versions 10 ; 6
      kept-old-versions 10 ; 2
      version-control t)

;; Allow diff/ediff with specific backup files
;; https://github.com/emacsmirror/pick-backup
(autoload 'pick-backup-and-diff "pick-backup" "Run Ediff on FILE and one of
its backups." t)
(autoload 'pick-backup-and-ediff "pick-backup" "Diff FILE with
one of its backups." t)
(autoload 'pick-backup-and-revert "pick-backup" "Replace FILE with one of its
backups." t)
(autoload 'pick-backup-and-view "pick-backup" "View one of FILE's backups." t)

;; Default -c, similar to u
(setq diff-switches "-u -w")

;; Saner ediff?
(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))


;; Options to pass to ls, default just -l
(setq list-directory-verbose-switches "-lh")

;; Updates buffer if file is changed elsewhere, DON'T save until the
;; other process has finished writing!!!  Set manually!
;; (auto-revert-tail-mode 1)

;; Follow real links to files.  Roughly mirrors vc-follow-symlinks, but for
;; non-vc managed files
(setq find-file-visit-truename t)

;; Some file names used here and in their respective modes, defined here to
;; avoid duplication of strings and mistakes like d1134df2
(defvar my/recentf-file "recentf")
(defvar my/ido-file "ido.last")
(defvar my/amx-file "amx-items")
(defvar my/desktop-file "emacs.desktop") ;; Rename to just desktop(.lock)???
(defvar my/saveplace-file "saved-places")

;; Recent files (~/.emacs.d/recentf)
;; Pointless if saving buffers as below?
;; Something busted, nothing saved when restarting server FIXME TODO
(require 'recentf)
;; Sometimes there are too many messages on startup
(advice-add 'recentf-mode :around
	    (lambda (orig-fun &rest args)
	      (let ((inhibit-message t))
		(apply orig-fun args))))
(recentf-mode 1)
(setq recentf-save-file (expand-file-name my/recentf-file user-emacs-directory))
;; Uses ~ instead of full path
(setq recentf-filename-handlers (quote abbreviate-file-name))
;; Same as above? FIXME TODO
;; (setq recentf-menu-filter (quote recentf-relative-filter))
;; Try not to save remote files
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 256)   ; 20 items ought to be enough for anybody
(setq recentf-max-menu-items 30)

;; Exclude boring files
(add-to-list 'recentf-exclude "\\.el.gz\\'")
(add-to-list 'recentf-exclude "\\.elc\\'")
;; Is there a variable for the directory emacs' files come in?
(add-to-list 'recentf-exclude ".*\\/opt.*\\/share\\/emacs.*\\'")
(add-to-list 'recentf-exclude (concat my/ido-file "\\'"))
(add-to-list 'recentf-exclude (concat my/amx-file "\\'"))
(add-to-list 'recentf-exclude (concat my/recentf-file "\\'"))
(add-to-list 'recentf-exclude "\\node_modules\\'")
;; Exclude symlinks
(add-to-list 'recentf-exclude
	     (lambda (f) (not (string= (file-truename f) f))))
;; Automatically save the list regularly, rather than wait for idle via
;; run-with-idle-timer.  Wait for init to be loaded though?
(add-hook 'after-init-hook
	  (lambda () (run-at-time nil (* 1 60) 'recentf-save-list)))
;; Cleanup every now and then when running via emacsclient daemon
(setq recentf-auto-cleanup (if (daemonp) 300 'never))

;; See also midnight-mode, which runs `midnight-hook', which defaults to just
;; `clean-buffer-list', so as to kill old buffers automatically.  Check if weird
;; with dashboard? FIXME TODO
(require 'midnight)

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
;; (add-to-list 'recentf-arrange-rules (quote (("Perl files (%d)" ".\\.t\\'"))))
;; (add-to-list 'recentf-arrange-rules (quote (("CGI files (%d)" ".\\.cgi\\'"))))


;; Markdown mode (also includes `gfm-mode')
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
	     '("\\.\\(markdown\\|mdml\\|mkdn\\|text\\|md\\)\\'" . markdown-mode))
(setq markdown-fontify-code-blocks-natively t ; Colorize code blocks
      markdown-hr-strings '("----"))	      ; I only ever want to use ----

;; Ensure git commit messages are edited in markdown, since this is mostly for
;; GitHub.  Not that common since magit usurps most of these (via
;; `with-editor-mode'), but occasionally they do popup, especially around
;; amend/fixup/squash.  markdownlint is awful when writing these, though, so
;; let's just turn `flycheck-mode' off when editing 'em.  See also the
;; customization of `git-commit-mode-hook', which does the same thing.
(add-to-list 'auto-mode-alist
	     '("/\\.git/COMMIT_EDITMSG\\'" . markdown-mode))
(add-to-list 'auto-mode-alist
	     '("/\\.git/PULLREQ_EDITMSG\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when (buffer-file-name)
	      ;; Actually, maybe this doesn't need to be for PULLREQ? FIXME TODO
	      (when (string-match "/\\.git/\\(COMMIT_EDITMSG\\|PULLREQ_EDITMSG\\)\\'" (buffer-file-name))
		(flycheck-mode 0)))))

;; Add a few items missing from block highlighting
(add-to-list 'markdown-code-lang-modes '("js\\|javascript" . js2-mode))
(add-to-list 'markdown-code-lang-modes '("css" . css-mode))
(add-to-list 'markdown-code-lang-modes '("json" . json-mode))
(add-to-list 'markdown-code-lang-modes '("perl" . cperl-mode))

;; Key bindings, probably needs tweaking; maybe make use of C-c C-s, etc.?
(define-key markdown-mode-map (kbd "C-M-f") 'forward-symbol)
(define-key markdown-mode-map (kbd "C-M-b") 'editutil-backward-symbol)

(define-key markdown-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
(define-key markdown-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
(define-key markdown-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
(define-key markdown-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
(define-key markdown-mode-map (kbd "C-c C-u") 'outline-up-heading)

;; Clobbered by flyspell, this makes more semantic sense anyway
(define-key markdown-mode-map (kbd "C-c `") 'markdown-edit-code-block)


;; Generate README.md markdown from header of elisp file for github
;; checkdoc might be useful beforehand
;; https://github.com/thomas11/md-readme
(autoload 'mdr-generate "md-readme" "Generate markdown READMEs from elisp" t)

(defun markdown-linkify ()
  "Make region or current word into a link to itself."
  (interactive nil markdown-mode)
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
;; https://github.com/hniksic/emacs-htmlize
(autoload 'htmlize-buffer "htmllize" "Convert buffer to HTML, preserving colors and decorations.")
(autoload 'htmlize-file "htmllize" "Load file, fontify it, convert it to HTML, and save the result.")
(autoload 'htmlize-region "htmllize" "Convert the region to HTML, preserving colors and decorations.")
(autoload 'htmlize-many-files "htmllize" "Convert files to HTML and save the corresponding HTML versions.")

(defun linkify-region-html (start end)
  "Wrap the region in `a href' for an HTML link."
  (interactive "r" html-mode)
  (let ((str (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert "<a href=\"\">" str "</a>")))

;; Tidy mode to judge your html
;; Maybe just in html-mode?  Rename?  FIXME TODO
;; http://www.emacswiki.org/emacs/tidy.el
;; Should get update elisp and tidy binary
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy_parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

;; Should rename to html-tidy? FIXME TODO
(defun tidy-then-indent ()
  "`tidy' a buffer's HTML, then indent it, as `tidy' leaves a buffer looking flat."
  (interactive nil html-mode)
  (tidy-buffer)
  (indent-buffer))

(autoload 'tidy-then-indent "tidy" "Run Tidy HTML parser then indent the
current buffer" t)
(defalias 'tidy-indent 'tidy-then-indent)

;; Bug hunter
;; https://github.com/Malabarba/elisp-bug-hunter
(autoload 'bug-hunter-init-file "bug-hunter" "Find bugs in your init file" t)


;; Auto-insert mode
(auto-insert-mode)
;; (setq auto-insert-query nil) ;; If you don't want to be prompted before
;; auto-insert mode skeletons
(eval-after-load 'autoinsert
  '(progn
     (defvar hashbang-env "#!/usr/bin/env ")
     ;; Perl
     (define-auto-insert '("\\.pl\\'" . "Perl skeleton")
       '(nil (concat hashbang-env "perl") \n
	     "# " (file-name-base) " by " user-full-name \n
	     "# " _ \n
	     \n "use 5.036;" \n		; just default there
	     ;; "use strict;" \n
	     ;; "use warnings;" \n
	     ;; "use English;" \n
	     \n "use diagnostics; # Remove after development TODO" \n))
     ;; Perl tests
     (define-auto-insert '("\\.t\\'" . "Perl test skeleton")
       '(nil (concat hashbang-env "perl") \n
	     ;; "# " (file-name-base) " by " user-full-name \n
	     ;; "# " _ \n
	     \n "use 5.036;" \n		; just default there
	     ;; "use English;" \n
	     \n "use Test::More;" \n))
     ;; shell script
     (define-auto-insert '("\\.\\(ba\\)?sh\\'" . "Bash skeleton")
       '(nil (concat hashbang-env "bash") \n
	     "# " (file-name-base) " by " user-full-name \n
	     "# " _ ))

     ;; Python
     (define-auto-insert '("\\.py\\'" . "Python skeleton")
       '(nil (concat hashbang-env "python") \n _ ))

     ;; Ruby
     (define-auto-insert '("\\.rb\\'" . "Ruby skeleton")
       '(nil (concat hashbang-env "ruby") \n _ ))))
;;; Can also define file, ie
;; (setq auto-insert-directory "~/.mytemplates/") ;; Note: trailing slash important
;; (define-auto-insert "\.py" "my-python-template.py")


;; If a region selected, typing replaces it
(delete-selection-mode t)

;; Wrap-region mode, wrap with quotes or braces
;; https://github.com/rejeep/wrap-region.el
(require 'wrap-region)
;; (wrap-region-mode t)
(wrap-region-global-mode t)
(wrap-region-add-wrappers
 '(
   ("/* " " */" "#" (java-mode js2-mode js-mode css-mode))
   ("*" "*" nil markdown-mode)
   ("`" "`" nil (markdown-mode ruby-mode))
   ("/" "/" nil ruby-mode)))
(add-to-list 'wrap-region-except-modes 'ibuffer-mode)


(defun visit-most-recent-file ()
  "Visits the most recently open file in `recentf-list'
that is not already being visited."
  (interactive)
  (let ((buffer-file-name-list (mapcar 'buffer-file-name (buffer-list)))
	(most-recent-filename nil))
    (catch 'found
      (dolist (filename recentf-list)
	(unless (member filename buffer-file-name-list)
	  (setq most-recent-filename filename)
	  (throw 'found nil))))
    (when most-recent-filename
      (find-file most-recent-filename))))
(global-set-key (kbd "C-x C-r") 'visit-most-recent-file)

;; Reveal file or folder in finder
;; https://github.com/kaz-yos/reveal-in-osx-finder
(autoload 'reveal-in-osx-finder "reveal-in-osx-finder" "Reveal file/folder in finder" t)
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)


;; Requires some tweaking to be fine with emacsclient, but works okay
;; In theory, should confirm exactly which ones need tweaking for emacsclient
;; and then set accordingly.  Hell, I could probably make more things contingent
;; on emacs-server or not, especially lazy-loading stuff (`server-running-p') FIXME TODO
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-save 't			; Always save, nicer for emacsclient
      desktop-restore-eager 2		; Load this many buffers, rest when lazy
      desktop-lazy-idle-delay 2		; Okay not that lazy
      desktop-load-locked-desktop 't	; Always load, nicer for emacsclient
      desktop-restore-forces-onscreen nil ; Don't restore frames onto the screen
      desktop-base-file-name my/desktop-file	  ; Not .emacs.desktop
      desktop-base-lock-name (concat my/desktop-file ".lock") ; Not .emacs.desktop.lock
      ;; Don't try to save if file is locked, I'll always be quick
      desktop-not-loaded-hook '(desktop-save-mode-off) ; Pointless with the above for emacsclient
      desktop-path (list user-emacs-directory "~")) ; Just in case
(add-to-list 'desktop-modes-not-to-save 'helpful-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
;; Save a bunch of variables to the desktop file.  For lists specify the len
;; of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 100)
		(file-name-history        . 100)
		(grep-history             . 30)
		(compile-history          . 30)
		(minibuffer-history       . 100)
		(query-replace-history    . 50)
		(read-expression-history  . 30)
		(regexp-history           . 60)
		(regexp-search-ring       . 30)
		(search-ring              . 30)
		(shell-command-history    . 50)
		tags-file-name
		register-alist)))


;; Open at last place visited in a file
;; Any overlap with desktop or persistency? Not great with emacsclient?
;; Do something like 'recenter in save-place-find-file-hook??
(require 'saveplace)
(add-hook 'after-init-hook #'save-place-mode)
(setq save-place-file (expand-file-name my/saveplace-file user-emacs-directory))


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
;; Fuck I should use these more
(require 'bookmark)
(global-set-key "\C-xrd" 'bookmark-delete)
;; where to save the bookmarks
(setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
;; each command that sets a bookmark will also save your bookmarks
(setq bookmark-save-flag 1)


;; scrolling
(setq
 scroll-margin 0                        ;; do smooth scrolling
 scroll-conservatively 9001		;; ... the defaults ...
 scroll-up-aggressively 0.0             ;; ... are very ...
 scroll-down-aggressively 0.0           ;; ... annoying
 scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

;; Set recentering positions, will be used for below as well
;; I'd like to set scroll-margin to 2, but it's also active here, which is annoying
(setq recenter-positions '(middle 5 top bottom))
;; Move pointer to center/top/bottom of buffer, compare to C-l
(global-set-key (kbd "C-c l") 'move-to-window-line-top-bottom)

;; Keep cursor away from edges when scrolling up/down
;; https://github.com/aspiers/smooth-scrolling/
(require 'smooth-scrolling)
(setq smooth-scroll-margin 3) ;; default 10

;; case INsensitive search.  Once turned off somehow?
(setq-default case-fold-search t)

;; Regex search... Always.  `isearch' face customized in custom.el to be
;; different from `lazy-highlight', as it should be.  Annoyingly, `isearch' and
;; `vr/' (visual-regexp) should be aligned too?  I dunno which to prefer but
;; it's a little odd they're different? FIXME TODO
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

;; Allow scrolling (not off-screen) during search
;; Kind of weird if going up?
(setq isearch-allow-scroll t
      ;; Add search commands to history, allow resuming
      isearch-resume-in-command-history t)

;; Scroll history in isearch, same as minibuffer
(define-key isearch-mode-map (kbd "M-p") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "M-n") 'isearch-ring-advance)

;; Grab the full word for searching
(define-key isearch-mode-map (kbd "C-M-w") 'isearch-forward-symbol-at-point)

;; Activate occur easily inside isearch, also M-s o
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; Search the project for isearch's current search term
;; <https://blog.chmouel.com/posts/emacs-isearch/#do-a-project-search-from-a-search-term>
(defun project-search-from-isearch ()
  (interactive)
  (let ((query (if isearch-regexp
		   isearch-string
		 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (project-find-regexp query)))
(define-key isearch-mode-map (kbd "C-f") 'my-project-search-from-isearch)



;; Visual feedback for regex replace
;; https://github.com/benma/visual-regexp.el
;; Also setup isearch, isearch regexp for this style?  Probably
;; Replace is fast, query asks
;; Only works DOWN a buffer
;; Sourced by below -steroids
;; (require 'visual-regexp)
;; Python-style regexp instead of stupid-ass friggin' crazy escapes
;; https://github.com/benma/visual-regexp-steroids.el
(require 'visual-regexp-steroids)
;; The default vr/engine is python as in python2, not python3.  pcre2el is
;; available and easy.  One *could* set vr/command-python to python3, but
;; honestly that's annoying given that, installed via MELPA, the path to
;; regexp.py will change with any updates (not that this is actually actively
;; updated, but still), so this is probably fine.
(setq vr/engine 'pcre2el)
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

;; Display number of matches in modeline for isearch.  Use anzu instead of
;; visual regexp since it does much the same thing??? FIXME TODO
;; (global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
;; (global-set-key (kbd "C-x M-%") 'anzu-query-replace-at-cursor)
;; (global-set-key (kbd "C-x %") 'anzu-replace-at-cursor-thing)
;; https://github.com/syohex/emacs-anzu
(require 'anzu)
(global-anzu-mode t)
(setq anzu-deactivate-region t
      anzu-search-threshold 1000
      anzu-replace-to-string-separator " => ")
(set-face-attribute 'anzu-mode-line nil :weight 'normal)


;; Jump to word (char, line, with C-u, C-u C-u)
;; https://github.com/winterTTr/ace-jump-mode
;; Old and unmaintained; should use avy instead FIXME TODO
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(setq ace-jump-mode-scope 'frame)	; Only look in current frame, not all windows

;; Reorder so char with one prefix, line with two
(setq ace-jump-mode-submode-list
      '(ace-jump-word-mode        ; first one always maps to: C-c SPC
	ace-jump-char-mode        ; second one always maps to: C-u C-c SPC
	ace-jump-line-mode))      ; third one always maps to: C-u C-u C-c SPC

;; Jump back with C-x spc
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(add-to-list 'debug-ignored-errors "\\[AceJump\\].*")


;; recenter after occur
(add-hook 'occur-mode-find-occurrence-hook 'recenter)
;; occur-mode-display-occurrence is nice, but C-o is annoying
(define-key occur-mode-map (kbd "d") 'occur-mode-display-occurrence)

;; Utilize system's trash can
(setq-default delete-by-moving-to-trash t
	      trash-directory "~/.trash/emacs")

;; minibuffer window expands vertically as necessary to hold the text that you
;; put in the minibuffer
(setq resize-mini-windows t
      ;; Let it grow a bit even; default 0.25
      max-mini-window-height 0.33)

;; Save minibuffer history
(require 'savehist)
(add-hook 'after-init-hook #'savehist-mode)
(setq
 savehist-autosave-interval 180 ; Default 300
 savehist-ignored-variables (quote (ido-cur-list)) ; Not sure what do...
 savehist-additional-variables '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring))

;; Long history, remove dupes.  Could do `t' for `history-length' to never
;; truncate, but even though it'd be fine, there's no need?
(setq history-length 1000
      history-delete-duplicates t)


;; Allows hiding of comments
(autoload 'hide/show-comments-toggle "hide-comnt"
  "Toggle hiding/showing of comments in the active region or whole buffer." t)
(defalias 'toggle-comments 'hide/show-comments-toggle)
(global-set-key (kbd "C-x M-;") 'toggle-comments)

;; Make M-w a bit like C-w, etc.
(require 'whole-line-or-region)
(whole-line-or-region-global-mode 1)
(global-set-key (kbd "M-w") 'whole-line-or-region-kill-ring-save)
;; whole-line-or-region-comment-dwim-2 fully takes over comment-dwim, but I like
;; the "new comment" behavior from comment-dwim, so this undoes that a bit. In
;; practice, I think I'd prefer M-; to be wlr and C-x ; to c-d, but whatever.
;; comment-dwim is moderately better than comment-line, which was new in Emacs
;; 25.  I miss moving to the next line, though?? FIXME TODO
(substitute-key-definition 'whole-line-or-region-comment-dwim-2 'comment-dwim whole-line-or-region-local-mode-map)
(global-set-key (kbd "C-x ;") 'whole-line-or-region-comment-dwim-2)

;; Related, neat: kill first comment on line
(global-set-key (kbd "M-k") 'comment-kill)
;; Will add a comment if no comment, could just take over M-; FIXME TODO
(global-set-key (kbd "M-i") 'comment-indent)
;; Previously M-i was tab-to-tab-stop, which I never used but is neat, whereas
;; C-x i was ido-insert-file, which I *don't* need a command for
(global-set-key (kbd "C-x i") 'tab-to-tab-stop)

;; zap-to-char kills the character, zap-up-to-char is more intuitive
(global-set-key (kbd "M-z") 'zap-up-to-char)


;;;;;;;;;;;;;;;;;;;
;; Dired, file browser and then some
;; Should really look at dired-mode-map for the full suite of commands
;; Like, v and o are dope!
;; Check out dired-hacks for filter, sort, narrow, colors, etc. TODO
;; https://github.com/Fuco1/dired-hacks
(require 'dired)
(setq dired-auto-revert-buffer t
      dired-dwim-target t		; seems useful?
      dired-hide-details-hide-symlink-targets nil
      ;; Don't need disk space info
      dired-free-space nil
      ;; Sorting by size means I can use `dired-sort-toggle-or-edit' (s) in
      ;; dired to toggle sorting by size or time, which is what I want
      dired-listing-switches "-FlagoSh"
      dired-clean-confirm-killing-deleted-buffers nil
      ;; https://lmno.lol/alvaro/emacs-git-rename-courtesy-of-dired
      dired-vc-rename-file t)
(when (eq system-type 'darwin)
  (setq-default dired-ls-F-marks-symlinks t ; OSX uses @ after symlinks
		dired-use-ls-dired nil))    ; OSX ls doesn't support --dired
(add-hook 'dired-mode-hook 'hl-line-mode)
;; Use `a' rather than `return' so as not to open up so many damn windows
;; Don't warn/disable/whatever
(put 'dired-find-alternate-file 'disabled nil)
;; Seems neat
(define-key dired-mode-map (kbd "F") 'dired-create-empty-file)
;; Remap ^ to use find-alternate-file to move up, thus not opening another dired
;; buffer.  In theory that might be nice, but in practice I'm just passing through.
;; Could probably just use dired-single https://github.com/crocket/dired-single
(defun my/dired-move-up-directory ()
  "Move up one directory without opening yet another Dired buffer.  Mimics
`dired-find-alternate-file', but uses `find-alternate-file' it its place so as
to explicitly provide `..' as an argument.  Will be remapped to `^'."
  (interactive nil dired-mode)
  (set-buffer-modified-p nil)
  (find-alternate-file ".."))
(define-key dired-mode-map (kbd "^") 'my/dired-move-up-directory)

;; Rename dired buffers to absolute directory name, courtesy of
;; https://emacs.stackexchange.com/a/2154/2051
(add-hook 'dired-after-readin-hook
	  (lambda ()
	    ;; Uniquify-esque or Magit-esque?
	    (rename-buffer (generate-new-buffer-name (concat "dired: " dired-directory)))))

;; wdired lets you rename files
(require 'wdired)
(setq wdired-allow-to-change-permissions 'advanced)
;; Available as C-x C-q, but nice to be able to toggle more easily
(define-key dired-mode-map (kbd "C-w") 'wdired-change-to-wdired-mode)
;; The mode-map isn't around until loaded
(with-eval-after-load "wdired"
  (define-key wdired-mode-map (kbd "C-w") 'wdired-abort-changes)
  (define-key wdired-mode-map (kbd "C-c k") 'wdired-abort-changes))

;; dired-x: ignore uninteresting files, get `dired-jump'
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
;; Feels weird that dired-jump is part of dired-x...
(global-set-key (kbd "C-x C-d") 'dired-jump) ; C-x d already dired...
;; By default excludes ., ., autosave files, and lockfiles(?); add stupid OSX
;; .DS_Store files.  See also `dired-omit-lines'
(setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|\\`.DS_Store\\'")

;; dired-sidebar https://github.com/jojojames/dired-sidebar
;; Works nicely with ibuffer-sidebar via sidebar-toggle (defined elsewhere)
(require 'dired-sidebar)
(setq dired-sidebar-use-one-instance t
      dired-sidebar-should-follow-file t
      dired-sidebar-theme 'none
      dired-sidebar-use-magit-integration t)
;; Disallow commands in the sidebar
(push 'rotate-windows dired-sidebar-toggle-hidden-commands)
(push 'toggle-windows-split dired-sidebar-toggle-hidden-commands)
;; Revert to the buffer's directory on a delay
;; (dired-sidebar-stale-buffer-time-idle-delay).  If a buffer doesn't have a
;; file associated with it (customize group, help, etc.) uses the file from the
;; buffer it was called from.  Can be weird.
;; Fix? FIXME TODO https://github.com/jojojames/dired-sidebar/commits/master/
;; and https://github.com/jojojames/dired-sidebar/issues/58
(add-hook 'dired-sidebar-mode-hook
	  (lambda ()
	    (unless (file-remote-p default-directory)
	      (auto-revert-mode))))

;; dired-gitignore https://github.com/johannes-mueller/dired-gitignore.el
(define-key dired-mode-map (kbd "H") #'dired-gitignore-mode)
(add-hook 'dired-mode-hook 'dired-gitignore-mode)


;;;;;;;;;;;;;;;;;;;
;; IDO, Interactively Do Things
;; interactively do things makes better buffer/find-file menus
;; C-s, C-r cycle
;; C-f, C-b switch to file or buffer mode
;; C-j new file with entered text
(require 'ido)
(ido-mode t) ; is this the best place for this?

;; https://github.com/scottjad/ido-hacks
(require 'ido-hacks)
(ido-hacks-mode 1)
;; Complete space/hyphen in ido like M-x
;; https://github.com/doitian/ido-complete-space-or-hyphen
(require 'ido-complete-space-or-hyphen)

;; Fuzzy-ish matching
(setq ido-enable-flex-matching t)
;; Better fuzzy matching, does the above need to be turned on?
;; Was slower, but now... just not as good? TODO
;; (require 'flx-ido)
;; (flx-ido-mode 1)


;; Not exactly sure but it sounds nice, right?  Use ido-completing-read+
(setq ido-everywhere t)
;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
;; Formerly ido-ubiquitous
(require 'ido-completing-read+)
(ido-ubiquitous-mode t)
;; Fix for weird issue https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28774
(defun ido-name (item)
  "Return file name for the current item, whether in a normal list or a merged work
  directory list."
  (concat (if (consp item) (car item) item)))

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

;; Specify save file in ~/.emacs.d/ folder
(setq ido-save-directory-list-file (expand-file-name my/ido-file user-emacs-directory)
      ;; Kind of keeps buffer names around via recentf in case things get closed
      ido-use-virtual-buffers t
      ;; Probably good/useful
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ;; Use / to enter directory if it's first, not just unique
      ido-enter-matching-directory 'first)
;; Not exactly sure but it sounds nice, right?
(ido-load-history)

(add-to-list 'ido-ignore-files "\\.DS_Store")
(add-to-list 'ido-ignore-files "\\.el.gz\\'")
(add-to-list 'ido-ignore-files "\\.elc\\'")
(add-to-list 'ido-ignore-directories "node_modules")

(defun ido-sort-mtime ()
  "Sort an ido filelist by modified time instead of alphabetically, and bury `.'."
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
;; http://whattheemacsd.com/setup-ido.el-02.html
(add-hook
 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
	       (kbd "~")
	       (lambda ()
		 (interactive)
		 (if (eq (char-before) ?/)
		     (insert "~/")
		   (call-interactively 'self-insert-command))))))

;; C-k to kill buffer, C-b to bury it
;; http://endlessparentheses.com/Ido-Bury-Buffer.html
(defun endless/define-ido-bury-key ()
  "Define key binding for burying buffer in ido."
  (define-key ido-completion-map
    (kbd "C-b") 'endless/ido-bury-buffer-at-head))
(add-hook 'ido-setup-hook #'endless/define-ido-bury-key)

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


;; Should really figure all this stuff out better FIXME TODO
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
;; (global-set-key (kbd "C-x f") 'recentf-open-files)
;; (global-set-key (kbd "C-x f") 'recentf-open)
(global-set-key (kbd "C-x f") 'ido-choose-from-recentf)

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
;; Clobbered by `elisp-byte-compile-buffer' in Elisp mode FIXME
(global-set-key (kbd "C-c C-b") 'ido-switch-buffer-other-window)
;; Open buffer in another window, don't select it
(global-set-key (kbd "C-c b") 'ido-display-buffer)



;; buffer-menu is crap, use ibuffer instead
;; bs-show (with prefix) and electric-buffer-list (sorta) also options
;; Can do a lot with it!  occur, regex search, etc.
;; Okay, it's ibuffer, but these recursively call each other and this is useful
;; for extra things like ibuffer-never-show-predicates
(require 'ibuf-ext)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Want to bury ibuffer after leaving https://emacs.stackexchange.com/a/53862/2051
;; but can probably use a hook?
;; ibuffer-vc would be dope too https://github.com/purcell/ibuffer-vc
;; Try this:
;; (add-hook 'ibuffer-hook
;;		  (lambda ()
;;		    (ibuffer-vc-set-filter-groups-by-vc-root)
;;		    (unless (eq ibuffer-sorting-mode 'alphabetic)
;;		      (ibuffer-do-sort-by-alphabetic))))

(setq ibuffer-always-show-last-buffer :nomini
      ;; Sounds nice, but runs `fit-window-to-buffer' as long as the Ibuffer
      ;; buffer is visible, so this is trash without a hook to kill that buffer
      ;; upon leaving the menu or selecting a buffer
      ibuffer-default-shrink-to-minimum-size nil
      ibuffer-jump-offer-only-visible-buffers nil
      ibuffer-show-empty-filter-groups nil
      ibuffer-use-other-window t
      ;; Keep updated; toggle with C-c C-a in ibuffer
      ibuffer-mode-hook '(ibuffer-auto-mode))
(add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)
;; Never show Ibuffer itself; why doesn't this work? FIXME TODO
(add-to-list 'ibuffer-never-show-predicates "\\*Ibuffer\\*")

;; Add column for last-viewed time
;; https://emacs.stackexchange.com/a/64210/2051
(define-ibuffer-column last-viewed
  (:name "Last-viewed" :inline t)
  (with-current-buffer buffer
    (format-time-string "%Y-%m-%d %R" buffer-display-time)))
;; Insert after mode
(setq ibuffer-formats
      '((mark modified read-only locked " "
	      (name 18 18 :left :elide)
	      " "
	      (size 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      (last-viewed 18 -1 :left)
	      " " filename-and-process)
	(mark " "
	      (name 16 -1)
	      " " filename)))
;; `ibuffer-do-sort-by-recency' doesn't work super well?  Maybe because it
;; derives from `buffer-list' so then interacts poorly with `ibuffer-auto-mode'?
;; Regardless, it doesn't seem to reliably sort by buffer-display-time, so use a
;; custom sorter function to override that.
(define-ibuffer-sorter last-viewed
  "Sort the buffers by last viewed time."
  (:description "last viewed")
  (string-lessp (with-current-buffer (car a)
		  (format-time-string "%Y-%m-%d %R" buffer-display-time))
		(with-current-buffer (car b)
		  (format-time-string "%Y-%m-%d %R" buffer-display-time))))
(define-key ibuffer-mode-map (kbd "s v") 'ibuffer-do-sort-by-last-viewed)

;; ibuffer-sidebar https://github.com/jojojames/ibuffer-sidebar
;; Kind of neat, I guess?  Annoying with ibuffer-auto-mode
(require 'ibuffer-sidebar)
(setq ibuffer-sidebar-width 30)		; Little smaller from default of 35
;; Does work well with dired-sidebar though
(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

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


;; Highest priority for a given buffer is shown
;; https://stackoverflow.com/questions/35770525/how-to-color-entries-in-emacss-ibuffer-by-buffer-type
(setq
 ibuffer-fontification-alist
 '(;; read-only buffers
   (10 buffer-read-only font-lock-constant-face)
   ;; Compressed files
   (15 (and buffer-file-name (string-match ibuffer-compressed-file-name-regexp buffer-file-name)) font-lock-doc-face)
   ;; emacs special buffers
   (20 (string-match "^\\*" (buffer-name)) font-lock-keyword-face)
   ;; hidden buffers
   (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name)) italic)
   ;; help buffers
   (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face)
   ;; dired
   (35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
   ;; locked
   (40 (and (boundp 'emacs-lock-mode) emacs-lock-mode) ibuffer-locked-buffer)
   ))


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
;; Minibuffer stuff
;;;;;;;;;;;;;;;;;;;
;; Display depth indicator in minibuffer, kind of weird but may be useful.
;; `minibuffer-prompt' face set in custom.el to be consistent (hell yeah
;; purple).  Could set `enable-recursive-minibuffers' but don't want?
(setq minibuffer-depth-indicate-mode t)

;; Amx, better M-x (using ido or anything)
;; <https://github.com/DarwinAwardWinner/amx>.  Replaces Smex
;; <https://github.com/nonsequitur/smex/>.  I repeat (amx-initialize) later
;; since having it so early misses functions loaded via autoload.
;; `amx-show-unbound-commands': frequently used commands without a key binding
(require 'amx)
;; Specify save file in ~/.emacs.d/, the default but just in case
(setq amx-save-file (expand-file-name my/amx-file user-emacs-directory))
;; Do I need both?  I dunno
(amx-initialize)
(amx-mode)
;; `execute-extended-command' is the old M-x
(global-set-key (kbd "M-x") 'amx)
;; Should probably use this one all the time? FIXME TODO Would be nice to be
;; able to cycle between them like `execute-extended-command' can in Emacs 29.1
(global-set-key (kbd "M-X") 'amx-major-mode-commands)
(setq amx-prompt-string "Amx "
      amx-history-length 512)
;; Redefine to use helpful rather `describe-function', nicer
(defun amx-describe-function ()
  "Exit the minibuffer and call `helpful-function' on selected item."
  (interactive)
  (amx-do-with-selected-item (lambda (chosen)
			       (helpful-function chosen)
			       (pop-to-buffer "*Help*"))))



;; Cleaner, more meaningful narrow-to-region
;; https://github.com/Malabarba/fancy-narrow
;; (require 'fancy-narrow)

;; Autowrap comments but only comments (longlines-mode for all)
(setq-default fill-column 80 ; Default is 70
	      comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook 'auto-fill-mode)
;; https://github.com/alpaker/Fill-Column-Indicator
;; Disabled to make copying easier, wish it could be just for comments
;; Maybe can use trailing lines for that?
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#000000"
;;       fci-rule-column '80)
;; (add-hook 'prog-mode-hook 'fci-mode)


;; I can't tell if this does anything different from M-q but the author
;; certainly seems to think so... https://snarfed.org/fillcode
(autoload 'fillcode-mode "fillcode" "A minor mode to enhance fill functions
when in source code modes such as python-mode or perl-mode" t)

(defun unfill-paragraph ()
  "Unfill a paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  "Unfill the selection region."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-region)


;; font-lock-comment-delimiter-face, the comment-start color, typically themed
;; to be the same as font-lock-comment-face, but adjusted in custom.el to
;; inherit the foreground only, keeping a normal italicization


(defalias 'bash-mode 'sh-mode)
(add-to-list 'auto-mode-alist
	     '("\\.completion" . bash-mode))
(remove-hook 'sh-mode-hook 'sh-electric-here-document-mode t)

;; Default mode
(setq-default major-mode 'text-mode)

;; Make RET aka C-m tab as well, C-j just newline
;; (global-set-key "\C-m" 'newline-and-indent)

;; Cleaner: Reindent current line, insert newline, indent newline
(global-set-key "\C-m" 'reindent-then-newline-and-indent)
(global-set-key "\C-j" 'newline)
(global-set-key (kbd "M-o m") 'newline-and-indent) ; Enter on number pad

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


;; See what you type in real-time
(setq echo-keystrokes 0.01)

;; Max lines in *Messages*, default 1000
(setq message-log-max 1500)
;; 2305843009213693951
;; (setq message-log-max most-positive-fixnum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Point out changes
;; I hate this
;; (highlight-changes-mode 1)

;; Magit stuff
;; Manual: https://magit.vc/manual/magit.html
(require 'magit)
;; C-x g for status, but C-x M-g for magit-dispatch: trigger command directly
;; C-c M-g: magit-file-dispatch; super convenient, so let's make it more so
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
;; Actually, maybe that'd be better as a prefix, then have C-c g f or something
;; for this and C-c g l for git-link, etc.  Maybe?

;; From within an ido prompt, open that file into magit-status.  Doesn't seem
;; like there are other options?
(define-key ido-common-completion-map
  (kbd "C-x C-g") 'ido-enter-magit-status)
;; Open log, etc. in separate window?  Like vc-print-log
;; Look into tweaking faces?
;; Prior to magit, I turned off built-in vc handling, preferring manual git:
;; (delete 'Git vc-handled-backends) ;; delete git from list of backends
;; (setq vc-handled-backends nil) ;; delete all backends
;; Probably don't need to turn it off if using magit, and apparently doing so
;; can muck up magit so it won't follow symlinks?  It seems like just setting
;; vc-follow-symlinks does the trick?
;; https://github.com/magit/magit/issues/2250#issuecomment-138906601
(setq vc-follow-symlinks t
      ;; I dunno, why not?
      vc-make-backup-files t
      ;; Unrelated, but I'm just using git, no need for any others
      vc-handled-backends '(Git))
;; git-commit-style-convention-checks take overlong-summary-line if want to
;; ensure git commit is within guidance (git-commit-summary-max-length)
;; git-commit-turn-on-flyspell ???
(setq git-commit-major-mode 'markdown-mode
      git-rebase-confirm-cancel nil
      ;; This is the default less git-commit-turn-on-auto-fill, since I don't
      ;; want to wrap lines in commit messages by default (GitHub don't care)
      git-commit-setup-hook
      '(git-commit-save-message git-commit-setup-changelog-support git-commit-propertize-diff bug-reference-mode with-editor-usage-message))
;; Edit git messages in markdown as these are mostly targeted for GitHub
(add-hook 'git-commit-mode-hook 'markdown-mode)
;; But turn off flycheck since markdownlint is awful in `git-commit-mode'
(add-hook 'git-commit-mode-hook
	  #'(lambda () (flycheck-mode 0)) t)

(setq magit-log-section-commit-count 25 ; default 10
      ;; Display buffers in same buffer, except for diffs
      ;; Eh, they multiply too much, things get
      ;; lost. 'magit-display-buffer-traditional is better
      ;; magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
      ;; Turn this off if/when ivy
      magit-completing-read-function 'magit-ido-completing-read
      ;; Not entirely sure what this does, but seems worthwhile
      magit-diff-refine-hunk t)

;; Add any ongoing merge-log to status sections; check out other magit-insert functions
(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-merge-log)
;; magit-section-initial-visibility-alist to customize initial visibility

;; Log stylin', just minor tweak to author length; L l/d to toggle
;; (setq
;;  magit-status-margin '(nil age magit-log-margin-width nil 12)  ; magit-status
;;  magit-log-margin '(t age magit-log-margin-width t 12)	       ; magit-log
;;  magit-log-select-margin '(t age magit-log-margin-width t 12))
;; Set directories using bash environment (currently from .bash_profile)
;; ` quotes but means , evaluates; using ' to quote puts in directly
(setq magit-repository-directories
      `(("~/dotfiles" . 0)
	(,(getenv "GIT_PERS_DIR") . 1)
	(,(getenv "GIT_EXTL_DIR") . 1))
      ;; Swap order so N is last
      magit-repolist-column-flag-alist
      '((magit-unstaged-files . "U")
	(magit-staged-files . "S")
	(magit-untracked-files . "N"))
      magit-repolist-columns
      '(("Name" 20 magit-repolist-column-ident nil)
	("B<U" 3 magit-repolist-column-unpulled-from-upstream
	 ((:right-align t)
	  (:help-echo "Upstream changes not in branch")))
	("B>U" 3 magit-repolist-column-unpushed-to-upstream
	 ((:right-align t)
	  (:help-echo "Local changes not in upstream")))
	("+/-" 3 magit-repolist-column-flag nil)
	("St#" 3 magit-repolist-column-stashes nil)
	("Branch" 15 magit-repolist-column-branch nil)
	;; ("Version" 10 magit-repolist-column-version nil)
	("Path" 99 magit-repolist-column-path nil))
      )
;; Make magit use delta, if present https://github.com/dandavison/magit-delta
;; https://github.com/dandavison/delta
(when (executable-find "delta")
  (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))

;; Should really tweak some of the magit faces, such as magit-blame-* to match
;; what's in my gitconfig FIXME TODO

;; abridge-diff https://github.com/jdtsmith/abridge-diff
;; Show refined, abridged diff hunks in magit
;; Really neat but doesn't work with magit-delta
;; Honestly this might be better for magit than delta, should investigate
;; (with-eval-after-load "magit"
;;   (require 'abridge-diff)
;;   (abridge-diff-mode 1))

;; magit-todos https://github.com/alphapapa/magit-todos
;; (require 'magit-todos)
;; ;; Could adjust section so not last via `magit-status-sections-hook'
;; ;; See also https://github.com/alphapapa/magit-todos/issues/8
;; ;; Hmm: https://github.com/alphapapa/magit-todos/commit/9a8e1f584cef577cd77ce3aa623e2d850f7a4f86
;; (let ((inhibit-message t))
;;   (setq
;;    ;; Include hidden files like .emacs
;;    magit-todos-rg-extra-args '("--hidden")
;;    ;; Only go one level below the repo directory
;;    magit-todos-depth 1
;;    ;; .git/ is the default
;;    magit-todos-exclude-globs '(".git/" ".emacs.d/")
;;    ;; hl-todo defines XXX+, a regex, which doesn't work
;;    ;; https://github.com/alphapapa/magit-todos/issues/101
;;    ;; See also https://github.com/alphapapa/magit-todos/commit/34b2a3df3d1056489c237f0ee92e004587600e92
;;    ;; But this doesn't either for some reason? FIXME TODO
;;    magit-todos-keywords (append '("XXX") (map-keys hl-todo-keyword-faces)))
;;   ;; Quiet message about *not* binding jT
;;   (magit-todos-mode 1)
;;   ;; Rebind jT to jump to TODOs list
;;   ;; Problematic since transient https://github.com/alphapapa/magit-todos/issues/95
;;   (transient-append-suffix 'magit-status-jump '(0 0 -1)
;;     '("T " "Todos" magit-todos-jump-to-todos)))

;; git modes for gitignore, gitconfig, and gitattributes
;; https://github.com/magit/git-modes
(autoload 'gitconfig-mode "gitconfig-mode" "A major mode for editing .gitconfig files." t)
;; Add my specific extra files, see [include] in .gitconfig
(add-to-list 'auto-mode-alist '("\\.?local-gitconfig\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.?priv-gitconfig\\'" . gitconfig-mode))
(autoload 'gitignore-mode "gitignore-mode" "A major mode for editing .gitignore files." t)
(autoload 'gitattributes-mode "gitattributes-mode" "A major mode for editing .gitattributes files." t)


;; Ability to mark commits in magit; not hugely used, but neat
;; https://codeberg.org/ideasman42/emacs-magit-commit-mark
;; Should maybe consider adjusting faces? FIXME TODO
(with-eval-after-load 'magit
  (add-hook 'magit-mode-hook 'magit-commit-mark-mode))
(with-eval-after-load 'magit-log
  (define-key magit-log-mode-map (kbd ";") 'magit-commit-mark-toggle-read)
  (define-key magit-log-mode-map (kbd "M-;") 'magit-commit-mark-toggle-star)
  ;; C-; no good?
  (define-key magit-log-mode-map (kbd "M-'") 'magit-commit-mark-toggle-urgent))


;; git-timemachine <https://codeberg.org/pidu/git-timemachine>
;; Pretty cool!  t to search by commit, b to blame, c to view commit in magit
(with-eval-after-load "git-timemachine"
  (setq git-timemachine-abbreviation-length 8)
  ;; Use some magit faces for parallelism
  (set-face-attribute 'git-timemachine-commit nil :inherit 'magit-hash)
  (set-face-attribute 'git-timemachine-minibuffer-author-face nil :foreground 'unspecified :inherit 'magit-log-author)
  (set-face-attribute 'git-timemachine-minibuffer-detail-face nil :foreground 'unspecified :inherit 'magit-blame-summary))
(global-set-key (kbd "C-x v t") 'git-timemachine)

;; Useful for git related work, although maybe try find-file-in-repo
;; (require 'find-file-in-project)
;; (global-set-key (kbd "C-x f") 'find-file-in-project)
;; Still, maybe just use all the project stuff?  `C-x p' prefix
(setq project-kill-buffers-display-buffer-list t)

;; smerge-mode, for resolving git conflicts/merges
;; default prefix is C-c ^, which is a pain
(setq smerge-command-prefix "\C-ce")

;; Highlight matching/mismatching parens.  Customized `show-paren-match' face in
;; custom.el.  `region' customized there as well, to be different.  Both could
;; use some work TODO
(show-paren-mode t)
;; Could used 'mixed here, but highlight-parentheses better?
(setq show-paren-style 'expression
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen 'overlay
      show-paren-delay 0.3)


;; Highlight parens we're currently between
;; https://sr.ht/~tsdh/highlight-parentheses.el
;; rainbow-delimiters good but not quite what I want
(require 'highlight-parentheses)
;; Max both out to a depth of 12
(setq highlight-parentheses-attributes (cl-loop repeat 12 collect '(:weight bold)))
;; Various shades of red?  Ugh.  Take the colors from certain faces and expand
;; the repertoire.  Should keep pace with whatever theme is used, but need the
;; colors to be defined first
(add-hook
 'font-lock-mode-hook
 #'(lambda ()
     ;; Go 12 deep
     (setq highlight-parentheses-colors
	   (cl-loop repeat 3 append
		    (list (face-attribute 'error :foreground)
			  (face-attribute 'warning :foreground)
			  (face-attribute 'success :foreground)
			  (face-attribute 'link :foreground))))

     ;; Reload, now that they're defined
     (highlight-parentheses--color-update)))
(add-hook 'prog-mode-hook #'highlight-parentheses-mode)
(add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)

;; Electric-pair parentheses.  Maybe consider paredit or smartparens?  TODO
(add-hook 'prog-mode-hook #'electric-pair-mode)


;; Allow highlighting of phrases.
;; Maybe hook for prog modes (perl, etc.)  ;;;;;;;; ####### FIXME TODO
;; https://github.com/nschum/highlight-symbol.el
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 1) ; default 0.5
;; (setq highlight-symbol-list
;; ;;;;;; ##### FIXME TODO FIX COLORS BEFORE GOING FORWARD
;; (highlight-symbol-mode t)




;; Easily indent line/region according to mode, or move line/region up or down
;; https://github.com/hbin/smart-shift
(require 'smart-shift)
(global-smart-shift-mode 1)
;; Take these off so they can be used for windmove below
(define-key smart-shift-mode-map (kbd "C-c <up>") nil)
(define-key smart-shift-mode-map (kbd "C-c <down>") nil)
(define-key smart-shift-mode-map (kbd "C-c <left>") nil)
(define-key smart-shift-mode-map (kbd "C-c <right>") nil)

;; Swap/flip/flop/transpose buffers easily. Will be added to M-t prefix
(require 'transpose-frame)

;; Move buffers around https://github.com/lukhas/buffer-move
;; Will be added to my/meta-g-map
(require 'buffer-move)

;;;;;;;;;;;;;;;;;;;;;;;
;; Personal keymaps
;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reclaim C-t as a general transposition map
;; Maybe better as C-c t?  M-t?
;; Why on earth does this not show up in which-key but C-q does??? FIXME TODO
;; Maybe doing what flycheck-keymap-prefix suggests would work...
;; Maybe add transpose-frame stuff? FIXME TODO
;; Unset a few scattered here n' there; maybe a 'bind or something would be better?
(global-unset-key (kbd "C-t"))	   ;; was transpose-chars
(global-unset-key (kbd "C-x C-t")) ;; was transpose-lines
(global-unset-key (kbd "M-t"))	   ;; was transpose-words
(defvar my/ctrl-t-transpose-map
  (let* ((map (make-sparse-keymap)))
    (define-key global-map (kbd "C-t") map)
    (define-key map (kbd "c") 'transpose-chars)
    (define-key map (kbd "w") 'transpose-words)
    (define-key map (kbd "l") 'transpose-lines)
    (define-key map (kbd "s") 'transpose-sexps)
    (define-key map (kbd "p") 'transpose-paragraphs)
    map)
  "Personal keymap for transpositions.")


;; Add to/take over the goto-map
;; goto-line-with-feedback (better goto-line, the original M-g M-g, etc.) takes
;; goto-line bindings in amory-manipulate.el
;; (define-key goto-map (kbd "g") 'goto-line)
(define-key goto-map (kbd "c") 'move-to-column)
(define-key goto-map (kbd "<up>") 'buf-move-up)
;; Good way to turn off error message?  Or move to just messaging? FIXME TODO
(define-key goto-map (kbd "<down>") 'buf-move-down)
(define-key goto-map (kbd "<left>") 'buf-move-left)
(define-key goto-map (kbd "<right>") 'buf-move-right)
;; `next-error' and `previous-error' are in here, and are useful for flycheck as
;; well, but let's turn off the annoying message
(setq next-error-verbose nil)


;; I rarely use quoted-insert (only to avoid annoying electric pairing), so I
;; might as well make something more useful.
;; A lot of this might be happy in the original M-s search-map???? FIXME TODO
(defvar my/ctrl-q-map
  (let* ((map (make-sparse-keymap)))
    (define-key global-map (kbd "C-q") map)
    (define-key map (kbd "C-q") 'quoted-insert)
    (define-key map (kbd "C-c") 'column-highlight-mode)
    (define-key map (kbd "C-a") 'align-regexp)
    (define-key map (kbd "q") 'qrr)
    (define-key map (kbd "r") 'rr)
    (define-key map (kbd "a") 'align) ;This is shit, change it FIXME TODO
    (define-key map (kbd ".") 'highlight-symbol-at-point)
    (define-key map (kbd "?") 'highlight-symbol-remove-all)
    (define-key map (kbd "/") 'highlight-symbol-remove-all)
    ;; But see also M-{ and M-}, plus M-a and M-e
    (define-key map (kbd "p") 'backward-paragraph)
    (define-key map (kbd "n") 'forward-paragraph)
    ;; Maybe move these to C-t???
    (define-key map (kbd "<up>") 'smart-shift-up)
    (define-key map (kbd "<down>") 'smart-shift-down)
    (define-key map (kbd "<left>") 'smart-shift-left)
    (define-key map (kbd "<right>") 'smart-shift-right)
    map)
  "My personal keymap, bound to C-q.")


;; M-# to move to windows
;; https://github.com/nschum/window-numbering.el
(require 'window-numbering)
(window-numbering-mode t)
(set-face-attribute 'window-numbering-face nil :inherit 'mode-line-buffer-id)

;; Easily switch to specific window using numbers (if >2)
;; https://github.com/abo-abo/ace-window
;; Semi-redundant with the above, but can also delete (x), swap (m), move (M),
;; copy (c), select (j), select previous (n), switch buffer other (u), split
;; fairly (F), split vertical (v), split horizontal (b), maximize current (o)
(autoload 'ace-window "ace-window" "Quickly switch windows" t)
(global-set-key (kbd "C-x o") 'ace-window)
(eval-after-load 'ace-window
  '(progn
     (set-face-attribute 'aw-leading-char-face nil
			 :foreground 'unspecified
			 :weight 'bold
			 :inherit 'font-lock-keyword-face)))

;; windmove stuff, in case the above simply isn't enough
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(setq windmove-wrap-around t)

;; ace-link: use o to quickly jump in help(ful) and info modes
;; https://github.com/abo-abo/ace-link
(require 'ace-link)
(ace-link-setup-default)

;; communinfo, provide more url associations for various OSS
;; <https://codeberg.org/mekeor/communinfo>
(require 'communinfo)
(setopt Info-url-alist communinfo-url-alist)

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
(setq use-short-answers t)

;; No mouse
(setq use-dialog-box nil
      use-file-dialog nil)

;; Don't mess with font size <https://xenodium.com/hey-mouse-dont-mess-with-my-emacs-font-size/>
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; Was exchange-point-and-mark
(global-set-key (kbd "C-x C-x") 'delete-other-windows)
;; aka C-x C-SPC
(global-set-key (kbd "C-x C-@") 'exchange-point-and-mark)
;; After C-u C-@/SPC to pop the mark (ie jump back), a simple C-SPC/@ does so again
(setq set-mark-command-repeat-pop t)

;; C-x u for tree, C-_ to undo, M-_ to redo, etc.
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t		 ;; default is off
      undo-tree-visualizer-relative-timestamps t ;; default is on
      ;; Save history
      ;; Holy fuck the messages for saving history are annoying FIXME TODO
      ;; Maybe better if ELPA ever updates and presents latest versions?
      undo-tree-auto-save-history t
      undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-emacs-directory)))
      ;; Neat mini diff
      undo-tree-visualizer-diff t
      ;; Larger size limits for undo, this might get unwieldy now that I'm
      ;; saving the history across sessions
      ;; FIXME TODO Actually understand these
      undo-limit 32000000	  ;; 160000
      undo-outer-limit 24000000   ;; 24000000
      undo-strong-limit 60000000) ;; 240000

;; Keep region when undoing in region http://whattheemacsd.com/my-misc.el-02.html
(define-advice undo-tree-undo (:around (orig-fun &rest args) keep-region)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
	    (p (set-marker (make-marker) (point))))
	(apply orig-fun args)
	(goto-char p)
	(set-mark m)
	(set-marker p nil)
	(set-marker m nil))
    (apply orig-fun args)))

;; Way more likely to remember
(defalias 'uppercase-dwim 'upcase-dwim)
(defalias 'uppercase-word 'upcase-word)
(defalias 'uppercase-region 'upcase-region)
(defalias 'lowercase-dwim 'downcase-dwim)
(defalias 'lowercase-word 'downcase-word)
(defalias 'lowercase-region 'downcase-region)
;; Don't warn
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)
;; More useful
(global-set-key "\M-c" 'capitalize-dwim)
(global-set-key "\M-u" 'uppercase-dwim)
(global-set-key "\M-l" 'lowercase-dwim)


;; Caser: convert to camelCase, snake_case, or dash-case
;; https://hg.sr.ht/~zck/caser.el
(require 'caser)


;; Highlight region undo, yanked, etc., is awesome
;; https://github.com/k-talo/volatile-highlights.el
(require 'volatile-highlights)
(volatile-highlights-mode t)
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move 'undo-tree-undo)
(vhl/install-extension 'undo-tree)


;; smartscan, go to next/previous symbol at point with M-n/M-p
;; <https://github.com/mickeynp/smart-scan> Was I really not using M-n/M-p in
;; prog modes beforehand?  Maybe remove FIXME TODO
(require 'smartscan)
(setq smartscan-symbol-selector "symbol")
(add-hook 'prog-mode-hook 'smartscan-mode)


(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapc #'kill-buffer (buffer-list))
  (delete-other-windows))

(defun indent-buffer ()
  "Indent the entire buffer."
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use ido to select which window based off window name
(defun rotate-list (list count)
  "Rotate the LIST by COUNT elements."
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
  "Interactively jump to another visible window based on it's `buffer-name'
using `ido-completing-read'."
  (interactive)
  (let* ((visible-buffers (mapcar #'(lambda (window) (window-buffer window)) (window-list)))
	 (sorted-visible-buffers (dka-sort-by-other-list visible-buffers (buffer-list)))
	 (rotated-buffer-list (rotate-list sorted-visible-buffers 1))
	 (visible-buffer-names (mapcar (lambda (buffer) (buffer-name buffer)) rotated-buffer-list))
	 (buffer-name (ido-completing-read "Enter buffer to jump to: "
					   visible-buffer-names
					   nil t))
	 (window-of-buffer
	  (delq nil
		(mapcar #'(lambda (window)
			    (if (equal buffer-name (buffer-name (window-buffer window)))
				window nil)) (window-list)))))
    (select-window (car window-of-buffer))))

;; Clobbered in js2, but not using anyway FIXME TODO
(global-set-key (kbd "C-c C-o") 'jump-to-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; bs-cycling is way better
(require 'bs)
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
(add-to-list 'auto-mode-alist '("\\.ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)


;; Ensure M-x shell uses login.  Not that I ever use M-x shell...
(setq explicit-bash-args '("--login"))
;; C-c s do a one-liner shell command
(global-set-key [(control c) (s)] 'shell-command)
;; C-c a to go to a terminal shell (ansi-term)
(global-set-key [(control c) (a)] 'ansi-term)

;; Exit in (ansi-)term returns to the emacs buffer
(define-advice term-sentinel (:around (orig-fun proc msg) my-advice-term-sentinel)
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
	(funcall orig-fun proc msg)
	(kill-buffer buffer))
    (funcall orig-fun proc msg)))

;; Always use new bash, don't ask
(defvar my-term-shell shell-file-name)
(define-advice ansi-term (:before (&rest _) force-bash)
  (interactive (list my-term-shell)))
;; Make links in man pages, etc., work, in ansi-mode
(add-hook 'term-mode-hook 'goto-address-mode)

;; Add some coloring to manpages, not much tbh
(with-eval-after-load "man"
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-builtin-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-function-name-face :underline t))


;; Copy current buffer file contents to clipboard
(defun pbcopy-buffer ()
  "Copy the contents of the current buffer to the GUI clipboard."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "pbcopy")
  (message "Copied contents of %s" buffer-file-name))
(global-set-key [(control c) (p)] 'pbcopy-buffer)
(defun pbcopy-region (start end)
  "Copy the contents of the selected region to the GUI clipboard."
  (interactive "r")
  (shell-command-on-region start end "pbcopy")
  (message "Copied selected region"))
(global-set-key [(control c) (control p)] 'pbcopy-region)


;; Convert DOS/Mac `^M' end of lines to Unix end of lines.  See also
;; set-buffer-file-coding-system (C-x RET f) with unix.
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
(defalias 'mac2unix 'dos-to-unix)
(defalias 'mactounix 'dos-to-unix)

(defun replace-smart-quotes ()
  "Replace smart quotes with normal quotes in this buffer."
  (interactive)
  (save-excursion
    (let ((fixes '((342396 . "\"") (342392 . "'") (342387 . "--")
		   (8220 . "\"") (8221 . "\"")
		   (342397 . "\"") (342393 . "'") (8217 . "'"))))
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((this (char-after (point)))
	       (match (assq this fixes)))
	  (when match
	    (delete-char 1)
	    (insert (cdr match))))
	(forward-char 1)))))


;; Might remember this better
(defalias 'reload-buffer 'revert-buffer)
(global-set-key (kbd "C-x M-r") #'(lambda ()
				   "Revert buffer without asking"
				   (interactive)
				   (reload-buffer 0 1)
				   (message "Buffer reverted")))

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
  "Insert the current date in ISO format.  With prefix argument, add include the
day of the week.  With two prefix arguments, add day of week and time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
		      ((equal prefix '(4)) "%Y-%m-%d %a")
		      ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
    (insert (format-time-string format))))


;; On duplicate filenames, show paths not <2>
;; Default in 24.4?
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward
      uniquify-ignore-buffers-re "^\\*")

(defun checksum-region (s e)
  "Print a checksum (currently md5) of the region."
  (interactive "r")
  (message (md5 (buffer-substring s e))))
(defalias 'md5-region 'checksum-region)

(defun region-length ()
  "Calculate length of the selected region."
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; Stock tracker <https://github.com/beacoder/stock-tracker/tree/master>
;; Just do `stock-tracker-start', can customize `stock-tracker-list-of-stocks',
;; otherwise saved in desktop file
(require 'stock-tracker)
(setq stock-tracker-refresh-interval 3	;default 1, is N*10 seconds
      stock-tracker-up-red-down-green nil)


(defun loan-payment-calculator (amount rate years)
  "Calculate the payment for a loan of AMOUNT dollars when annual percentage
rate is RATE and the term of the loan is YEARS years.  The RATE should expressed
in terms of the percentage \(i.e. \'8.9\' instead of \'.089\'\) and must contain
a decimal point.  The total amount of interest charged over the life of the loan
is also given."
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
    (message "%s" (/ (round (* multiplier value)) multiplier))))


;; Guess keybindings game
;; Maybe tweak to get a list for studying?
(autoload 'keywiz "keywiz" "Keywiz keybindings guessing game" t)
;; Poker
(autoload 'poker "poker" "Play a game of texas hold ='em poker" t)
;; Mandelbrot set
(autoload 'u-mandelbrot "u-mandelbrot" "Make a mandelbrot fractal" t)

;; Typing game https://github.com/lujun9972/el-typing-game
(autoload 'typing-game "typing-game" "Typing game" t)

;; emacs-fireplace by @johanvts.  Because emacs
;; https://github.com/johanvts/emacs-fireplace
(autoload 'fireplace "fireplace" "Light a cozy fire.")

;; Stop unicode trolls https://github.com/camsaul/emacs-unicode-troll-stopper
(autoload 'unicode-troll-stopper-mode "unicode-troll-stopper" "Stop unicode
trolls" t)


;; Requires howdoi to be installed (python)
(autoload 'howdoi "howdoi" "Instant SX answers" t)

;; Locate takes forever...
(setq locate-command "mdfind")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idling stuff

;; Zone out
(require 'zone)
;; After 3 idle minutes
;; (zone-when-idle zone-idle)
;; (setq zone-idle (* 60 3))

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


;; define-word https://github.com/abo-abo/define-word
;; uses https://wordnik.com/ by default, but can customize
;; define-word-default-service to openthesaurus, webster, or offline-wiktionary
;; (if I download something); define-it uses https://www.collinsdictionary.com/
;; lexic also neat, although sdcv... https://github.com/tecosaur/lexic
;; Prefer define-wap over define-word
(defalias 'lookup-word 'define-word-at-point)
(global-set-key (kbd "C-c d") 'define-word-at-point)
;; Might as well help out with builtin dictionary stuff, more detailed
(global-set-key (kbd "C-c C-d") 'dictionary-search)

;; writegood-mode https://github.com/bnbeckwith/writegood-mode
(autoload 'writegood-mode "writegood-mode" "Colorize issues with the writing
in the buffer." t)

;; webjump for searching easily
(require 'webjump)
(global-set-key (kbd "C-x j") 'webjump)
;; webjump doesn't take at-point/region by default, so overwrite the
;; relevant function with a tiny bit of code to help it do so
(defun webjump-read-string (prompt)
  (let ((input
	 (read-string (concat prompt ": ")
		      (if mark-active
			  (buffer-substring (region-beginning) (region-end))
			(thing-at-point 'symbol t)))))
    (if (webjump-null-or-blank-string-p input) nil input)))

;; Add some missing items to the webjump catalog
(add-to-list 'webjump-sites
	     '("Urban Dict" .
	       [simple-query
		"www.urbandictionary.com" "https://www.urbandictionary.com/define.php?term=" ""]))
(add-to-list 'webjump-sites
	     '("IMDB" .
	       [simple-query
		"www.imdb.com" "https://www.imdb.com/find?q=" "&s=all"]))
(add-to-list 'webjump-sites
	     '("Google translate" .
	       [simple-query
		"translate.google.com" "https://translate.google.com/?sl=auto&tl=en&text="""]))
(add-to-list 'webjump-sites
	     '("DOI" .
	       [simple-query
		"www.dx.doi.org/" "www.dx.doi.org/" ""]))
(add-to-list 'webjump-sites
	     '("Youtube" .
	       [simple-query
		"www.youtube.com" "https://www.youtube.com/results?search_query=" ""]))
(add-to-list 'webjump-sites
	     '("MediaWiki API" .
	       [simple-query
		"www.mediawiki.org" "https://www.mediawiki.org/wiki/API:" ""]))
(add-to-list 'webjump-sites
	     '("MDN" .
	       [simple-query
		"developer.mozilla.org" "https://developer.mozilla.org/en-US/search?q=" ""]))
(add-to-list 'webjump-sites
	     '("devdocs" .
	       [simple-query
		"devdocs.io" "https://devdocs.io/#q=" ""]))
(add-to-list 'webjump-sites
	     '("MetaCPAN" .
	       [simple-query
		;; Jump right to the pod page, rather than search; liable to be fine
		"metacpan.org" "https://metacpan.org/pod/" ""]))

;; Some wiki defaults aren't great
(add-to-list 'webjump-sites
	     '("Emacs Wiki" .
	       [simple-query
		"www.emacswiki.org" "https://duckduckgo.com/?q=" "+site%3Aemacswiki.org"]))
(add-to-list 'webjump-sites
	     '("Wikipedia" .
	       [simple-query
		"en.wikipedia.org" "https://en.wikipedia.org/wiki/Special:Search/" ""]))

;; Redefine so it's first, or at least before google translate
;; Needs to be slightly different than the previous one, hence the https
(add-to-list 'webjump-sites
	     '("Google" .
	       [simple-query
		"https://www.google.com" "www.google.com/search?q=" ""]))


;; browse-url-of-buffer will render the url assigned to a buffer.  This tells
;; Emacs how to map a given filename to a url. Check out skewer
;; https://github.com/skeeto/skewer-mode
(setq browse-url-filename-alist
      '(("^/\\(ftp@\\|anonymous@\\)?\\([^:]+\\):/*" . "ftp://\2/")
	("^/\\([^:@]+@\\)?\\([^:]+\\):/*" . "ftp://\1\2")
	("^/+" . "file:/")))

(setq browse-url-default-scheme "https")
(global-set-key (kbd "C-c v") 'browse-url)
(global-set-key (kbd "C-c C-v") 'browse-url)

;; There are lots of ways to view a file/commit/whatever at some web address,
;; each with different features, but ideally I'd have one for url browsing and
;; one for kill-ring copying.  There are some issues with browse-at-remote
;; (https://github.com/rmuslimov/browse-at-remote) around some remote styles,
;; and I'd prefer not to muck about with gitconfig just for that.  git-link
;; (http://github.com/sshaw/git-link) does so successfully, as well as the
;; kill-ring bit, so let's keep them separated, at least until it gets annoying.
(require 'git-link)
(setq git-link-default-remote 'origin)
(global-set-key (kbd "C-c C-g") 'git-link)
;; FIXME TODO SEE NOTE ABOVE WITH MAGIT ABOUT C-c g l
;; FIXME TODO MAYBE USE browse-at-remote NOW?! C-x v w

;; github-explorer https://github.com/TxGVNN/github-explorer
;; Browse files from a repo locally
;; Latest version a right pain when using IDO!  Can't get tree ("") at all, need
;; to disable ido-ubiquitous-mode or something FIXME TODO
(defalias 'browse-github-repo 'github-explorer)

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
	 (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
	  ((search-forward "<html" nil t) (html-mode)))))

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


;; Make the scratch buffer blank
;; (setq initial-scratch-message "")
;; Use haikus instead
(require 'amory-emacs-haiku)
;; Initialize *scratch* buffer with a random Emacs haiku
(setq initial-scratch-message (amory-random-emacs-haiku))
;; Clear out the scratch buffer, initialized with whatever major mode we were
;; just in and with any selected region; improved version of
;; `get-scratch-buffer-create' (sort of))
(defun new-scratch-buffer nil
  "Initialize the scratch buffer with the highlighted region,
setting whatever major mode was active."
  (interactive)
  (let* ((mode major-mode)
	 (region (with-current-buffer (current-buffer)
		   (if (region-active-p)
		       (buffer-substring-no-properties
			(region-beginning)
			(region-end))))))

    (switch-to-buffer (get-buffer-create "*scratch*"))
    (delete-region (point-min) (point-max))
    (funcall mode)
    ;; Rather than muck about with buffer-local variables - not to mention
    ;; elisp's `;` or `;;` identity crises - just comment it after the fact
    (insert (amory-random-emacs-haiku ""))
    (comment-region (point-min) (point-max))
    ;; Insert region, or just advance
    (if region (insert region))
    (goto-char (point-max))))


;; AI crap
(require 'gptel)
(setq gptel-api-key (getenv "OPENAI_API_KEY"))
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;; Or just run `gptel-end-of-response'
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
;; Add a Perl-specific one
(add-to-list 'gptel-directives '(perl . "You are an expert Senior Perl Developer. You are always correct and helpful. Try to provide brief explanations only, favoring code where appropriate."))


;;;;;;;;;;;;;;;;;;;;
;; cperl-mode: Better than perl-mode
(require 'cperl-mode)
;; Hairy affects most of the cperl options; read individual docs for more info.
;; Of note, the options themselves will (annoyingly) still display their
;; individual setting, and will be quietly overridden by `cperl-hairy':
;; `cperl-font-lock', `cperl-electric-lbrace-space', `cperl-electric-parens'
;; `cperl-electric-linefeed', `cperl-electric-keywords', `cperl-lazy-help-time'
;; `cperl-info-on-command-no-prompt', `cperl-clobber-lisp-bindings'.  See also
;; `cperl-style-examples' and `cperl-file-style'
(setq cperl-hairy t)
;; `cperl-hairy' overrides `cperl-electric-parens'; electric-parens, however,
;; takes care of things, so certain characters like ( and { can end up doubling
;; their mate.  Setting this explicitly to null takes care of things
(setq cperl-electric-parens 'null)
;; Default is 5s with `cperl-hairy'.  Can do C-h v (except when clobbered by
;; lisp, see below) for immediate help.  See also `cperl-mode-map' for more
;; along that line
(setq cperl-lazy-help-time 1)
;; Don't mess with C-h; would be useful but for the above
;; Not working since helpful defined below?  Blah.  FIXME TODO
(setq cperl-clobber-lisp-bindings t)
;; Treat _ as word character, probably counter-intuitive.  cperl-under-as-char
;; is deprecated, so use superword-mode (well, don't, but you know)
;; (setq cperl-under-as-char t)

;; Good?
(setq cperl-highlight-variables-indiscriminately t)

;; cperl-array-face and cperl-hash-face adjusted in custom.el, to avoid them
;; flipping back and forth

;; Was cperl-next-interpolated-REx-1
(define-key cperl-mode-map (kbd "C-c C-y") nil)
;; Was auto-fill-mode
(define-key cperl-mode-map (kbd "C-c C-f") nil)
;; Was cperl-find-bad-style
(define-key cperl-mode-map (kbd "C-c C-b") nil)

;; cperl always better than perl.  As of emacs 29.1 can use
;; `major-mode-remap-alist', although it's mainly for tree-sitter?
(defalias 'perl-mode 'cperl-mode)
;; Not complete but pod can be let through
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))
;; Ensure cgi ends up there too
(add-to-list 'auto-mode-alist '("\\.[cC][gG][iI]\\'" . cperl-mode))
;; Really?
(add-to-list 'interpreter-mode-alist '("\\(mini\\)?perl5?" . cperl-mode))

;; Perldoc in emacs
(defalias 'perldoc 'cperl-perldoc)

;; Used by the below functions, far from perfect
(defun cperl--get-current-subroutine-name ()
  "Get the name of the current subroutine in cperl-mode."
  (when (derived-mode-p 'cperl-mode)
    (save-excursion
      (re-search-backward "^sub[[:space:]]+\\([[:word:]]+\\)" nil t)
      (match-string 1))))

(defun cperl-get-current-subroutine ()
  "Display the current subroutine name from cperl-mode.
Uses `cperl--get-current-subroutine-name'."
  (interactive nil cperl-mode)
  (let ((current-subroutine (cperl--get-current-subroutine-name)))
    (when current-subroutine
      (message "Current subroutine: %s" current-subroutine))))
(define-key cperl-mode-map (kbd "C-c C-s") 'cperl-get-current-subroutine)

;; This is a monstrosity mostly to ensure the file is created in the right
;; place.  Vaguely useful!
(defun cperl-create-testfile-for-subroutine ()
  "Create a new test file in the t/ directory named after the current subroutine."
  (interactive nil cperl-mode)
  (let ((subroutine-name (cperl--get-current-subroutine-name)))
    (when subroutine-name
      (let* ((current-directory (file-name-directory buffer-file-name))
	     (lib-directory (locate-dominating-file current-directory "lib/"))
	     (base-directory (if lib-directory
				 (file-name-as-directory (expand-file-name lib-directory))
			       current-directory))
	     (test-file-directory (concat base-directory "t/"))
	     (test-file-name (format "%s/%s.t" test-file-directory subroutine-name)))
	(unless (file-exists-p test-file-directory)
	  (make-directory test-file-directory t))
	(find-file test-file-name)
	;; (insert (format "# Test for subroutine: %s\n\n" subroutine-name))
	(save-buffer)
	(message (format "Test file %s created." test-file-name))))))
(define-key cperl-mode-map (kbd "C-c C-t") 'cperl-create-testfile-for-subroutine)



;; Perltidy: https://github.com/emacsmirror/emacswiki.org/blob/master/perltidy.el
;; Requires stand-alone command line program; uses ~/.perltidyrc
;; Requires loading tramp beforehand, or should I just patch? FIXME TODO
(require 'tramp)
;; Required?  Hook into cperl mode or something? FIXME TODO
(require 'perltidy)
;; Possibly perltidy-buffer better for this, but we'll see
(define-key cperl-mode-map (kbd "C-c t") 'perltidy-dwim)
;;;;;;;;;;;;;;;;;;;;


;; Don't have ruby-mode auto-insert coding utf-8 info on files
(require 'ruby-mode)
(setq ruby-insert-encoding-magic-comment nil)


;;; Should probably figure out a way to diminish these fuckers TODO
;; Colors numbers
;; https://github.com/Fanael/highlight-numbers
;; Should... do elsewhere?  Remove?  prog-mode-map? Customize color??? FIXME TODO
(autoload 'highlight-numbers-mode "highlight-numbers" "Highlight numeric literals in source code" t)
;; Color identifiers based on their name, less useful with fully-powered theme coloring
;; https://github.com/Fanael/rainbow-identifiers
(autoload 'rainbow-identifiers-mode "rainbow-identifiers" "Color identifiers based on their name" t)
;; Color identifies uniquely
;; Redundant and broken? ;;;;;; ##### FIXME TODO
;; https://github.com/ankurdave/color-identifiers-mode
(autoload 'color-identifiers-mode "color-identifiers-mode" "Color identifiers uniquely" t)
;; Note rainbow-mode here?  Nothing to do... TODO

;; Get perldoc after C-h via P
(define-key 'help-command "P" 'perldoc)

(require 'apropos)
;; Search more than just commands but eh, I never use...
(define-key 'help-command "a" 'apropos)
;; Enhance apropos, slowly
(setq apropos-do-all t)

;; helpful, a better help buffer
;; https://github.com/Wilfred/helpful
(require 'helpful)
;; See also `find-function-on-key', `find-function', `find-variable', and
;; `find-library'
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)
(global-set-key (kbd "C-h v") #'helpful-variable)
;; Unrelated but: C-h w shows key binding for command
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c h") #'helpful-at-point)
;; No need for `view-hello-file'
(global-set-key (kbd "C-h h") #'helpful-at-point)
;; Maybe add helpful regex to clean-buffer-list-kill-regexps ??


;; which-key has better sorting than guide-key
;; Originally <https://github.com/justbur/emacs-which-key>
;; Now in Emacs <https://elpa.gnu.org/packages/which-key.html>
(require 'which-key)
;; The default behavior of `which-key-setup-side-window-bottom' is just fine.
;; In theory, I want top for `which-key-show-prefix', but it appears to devolve
;; to the same results as echo.
(add-hook 'after-init-hook #'which-key-mode)
;; There are a lot of these, all have merits
;; https://github.com/justbur/emacs-which-key#sorting-options
(setq which-key-sort-order 'which-key-key-order-alpha
      which-key-idle-delay 0.25		   ; default 1.0
      which-key-idle-secondary-delay 0.05  ; default nil, this is faster
      which-key-side-window-max-height 0.5 ; default 0.25
      which-key-prefix-prefix "=>+"	   ; default +
      ;; which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL") ; default nil
      which-key-show-early-on-C-h t
      which-key-show-remaining-keys t
      which-key-lighter nil  ; Don't rely on diminish, which doesn't work anyway
      which-key-compute-remaps t)
;; https://github.com/justbur/emacs-which-key#face-customization-options
(set-face-attribute 'which-key-separator-face nil :inherit 'font-lock-keyword-face)
;; font-lock-preprocessor-face and which-key-key-face also good options
(set-face-attribute 'which-key-note-face nil :inherit 'font-lock-comment-face)
(set-face-attribute 'which-key-group-description-face nil :inherit 'which-key-key-face)
(set-face-attribute 'which-key-command-description-face nil :inherit nil)


;; Display what function block if I'm in in certain modes reenable?
;; prog-mode-hook? FIXME TODO
(set-face-attribute 'which-func nil :foreground 'unspecified :weight 'bold :inherit 'font-lock-builtin-face)
;; Slow it down a bit
(setq which-func-update-delay 1)
;; (add-hook 'sh-mode-hook 'which-function-mode)
;; (add-hook 'emacs-lisp-mode-hook 'which-function-mode)


;; Jump to a definition in the current file (holy shit this is awesome)
;; Does this automatically use ido?  Others think it doesn't but I do...  Also
;; set to M-g i now
(global-set-key (kbd "C-c i") 'imenu)
(setq-default
 imenu-auto-rescan t ; Always rescan buffers
 imenu-auto-rescan-maxout 90000 ; Only if they're under 90k
 imenu-sort-function 'imenu--sort-by-name
 imenu-after-jump-hook 'recenter)
;; There's no interactive to force rescanning?  Fine but seems weird
(defun imenu-rescan ()
  "Manually rescan and build the buffer's `imenu' definitions."
  (interactive)
  (imenu--menubar-select imenu--rescan-item))
(global-set-key "\C-cI" 'imenu-rescan)


;; Which line, probably not hugely useful, C-x l more useful
;; (global-set-key (kbd "C-x w") 'what-line)


;; Print face at point
;; M-x describe-face to get list of properties
;; M-x what-cursor-position with a prefix (aka C-u C-x =) also gives face (along
;; with other info)
(defun what-face (pos)
  "Print the name of the face at the given point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defalias 'elisp-mode 'emacs-lisp-mode)

;; Give info at point in elisp mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.25)		;Default is 0.5
;; Collect and display all available documentation immediately
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
;; Not really useful, but at least I'll remember this
(defalias 'elisp-repl 'ielm)


;; Make scripts executable after they have been saved.  Probably covered with my
;; bash newscript function, but just in case
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)


;; Enable wildcard open files
(setq find-file-wildcards t)


;;;;;;;;;;;;;;;;;;;;
;; Spelling stuff
;;;;;;;;;;;;;;;;;;;;
;; Custom SCOWL dictionary: size 70, variants 2, diacritics both.  See:
;;; http://app.aspell.net/create
;;; http://aspell.net/man-html/Creating-an-Individual-Word-List.html
;;; http://aspell.net/man-html/Format-of-the-Personal-and-Replacement-Dictionaries.html
;; Test: 60: autobiographic 70: biltongs  80: cellulolytic personal: xkcd
;; Things like ispell-dictionary and ispell-personal dictionary should be
;; handled by default and by .aspell.conf, assuming the dictionary is installed
;; properly.  I've also put sug-mode in there too, but again, you never know.
(require 'ispell)
(setq ispell-dictionary "en-custom"
      ispell-personal-dictionary "~/.aspell.en.pws"
      ;; Don't ask to save to personal dictionary
      ispell-silently-savep t
      ;; --camel-case is neat, but since I'm using flyspell-prog-mode - only
      ;; strings and comments - it's unlikely to be necessary
      ispell-extra-args '("--sug-mode=ultra"))


;; Flyspell spell checking
;; Lots of ispell process instances being started then killed, especially
;; around git/magit?? FIXME TODO
;; Faces don't ever seem to take precedence?  FIXME TODO
(require 'flyspell)
(with-eval-after-load "flyspell"
  (setq flyspell-highlight-properties t
	flyspell-issue-message-flag nil
	flyspell-issue-welcome-flag nil

	;; Turns out `flyspell-auto-correct-previous-word' is often/always
	;; better than `flyspell-auto-correct-word'.  Can't set C-M-I for
	;; `flyspell-auto-correct-word' since that registers as C-M-i also.
	flyspell-auto-correct-binding (kbd "C-M-i")

	flyspell-duplicate-distance 100) ;Default is 400000

  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;; Ew
  (with-eval-after-load 'auto-complete
    (ac-flyspell-workaround))

  ;; There's no flyspell-goto-prev-error?  Dumb.
  (defun flyspell-goto-prev-error ()
    "Go to closest prior detected error.  Derived from FLYSPELL-GOTO-NEXT-ERROR."
    (interactive)
    (let* ((arg 1))
      (while (not (= 0 arg))
	(let ((pos (point))
	      (min (point-min)))
	  (if (and (eq (current-buffer) flyspell-old-buffer-error)
		   (eq pos flyspell-old-pos-error))
	      (progn
		(if (= flyspell-old-pos-error min)
		    ;; goto end of buffer
		    (progn
		      (message "Restarting from end of buffer")
		      (goto-char (point-max)))
		  (backward-word 1))
		(setq pos (point))))
	  ;; seek the previous error
	  (while (and (> pos min)
		      (let ((ovs (overlays-at pos))
			    (r '()))
			(while (and (not r) (consp ovs))
			  (if (flyspell-overlay-p (car ovs))
			      (setq r t)
			    (setq ovs (cdr ovs))))
			(not r)))
	    (backward-word 1)
	    (setq pos (point)))
	  ;; save the current location for next invocation
	  (setq arg (1- arg))
	  (setq flyspell-old-pos-error pos)
	  (setq flyspell-old-buffer-error (current-buffer))
	  (goto-char pos)
	  (if (= pos min)
	      (progn
		(message "No more miss-spelled words!")
		(setq arg 0)))))))

  ;; Add working flyspell bindings, since C-, and C-; don't
  ;; Could even have a fly prefix, taking over C-c f or C-c C-f that I don't use much?
  ;; Good place for a hydra, actually, since often want flys to continue to next...
  ;; Is there a way to hook flyspell and flycheck into same key?  Next of either?
  (define-key flyspell-mode-map (kbd "C-c '") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "C-c ;") 'flyspell-goto-prev-error))


;; abbrev-mode isn't really a spell-checker and doesn't quite belong here, but
;; with a long-ass list of commonly-misspelled words
;; (<https://www.masteringemacs.org/article/correcting-typos-misspellings-abbrev>),
;; it functions much the same.  `edit-abbrevs' to add to the table, and honestly
;; I could be using it more.
(abbrev-mode t)
;;;;;;;;;;;;;;;;;;;;


;; restclient https://github.com/pashky/restclient.el
;; Consider hurl-mode, verb, or plz-see
(setq restclient-same-buffer-response-name "*Restclient Response*")


;; FUCKS SHIT UP ;;;;;;; #########
;; Use pretty symbols in buffer
;; (require 'pretty-mode)


;; I don't use the calendar, but useful?  `sunrise-sunset' and `lunar-phases'
;; are neat.  `calendar-location-name' just breaks things, and the default uses
;; lat-long anyway.  Defined in priv-env.bash
(require 'solar)
(eval-after-load
    'solar
  '(progn
     ;; 24-hour time better
     (setq calendar-time-display-form
	   '(24-hours ":" minutes am-pm
		      (if time-zone " (")
		      time-zone
		      (if time-zone ")")))
     (setq calendar-latitude (string-to-number (getenv "LATITUDE")))
     (setq calendar-longitude (string-to-number (getenv "LONGITUDE")))))


;; Some potentially useful stuff from Magnars
;; <https://github.com/magnars/.emacs.d> Rename file and buffer.  Maybe
;; duplicated by `rename-visited-file' in Emacs 29.1, but maybe this is still
;; better.
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))
;; Don't use ido for this, would defeat the purpose
(put 'rename-current-buffer-file 'ido 'ignore)


;; Inspired by the above
(defun copy-current-buffer-file ()
  "Creates a copy of the current buffer's file, then opens it."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "Copy to: " nil nil nil (file-name-nondirectory filename))))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (copy-file filename new-name 1)
	  (find-file new-name)
	  (message "File '%s' successfully copied to '%s' and opened"
		   (file-name-nondirectory filename) (file-name-nondirectory new-name)))))))
;; As above
(put 'copy-current-buffer-file 'ido 'ignore)


;; http://whattheemacsd.com/file-defuns.el-02.html
(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
	(ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
	(delete-file filename)
	(kill-buffer buffer)
	(message "File '%s' successfully removed" filename)))))
;; likewise
(put 'delete-current-buffer-file 'ido 'ignore)

(defun insert-file-name ()
  "Does what it says on the tin."
  (interactive)
  (insert (file-name-nondirectory
	   (buffer-file-name
	    (if (minibufferp)
		(window-buffer (minibuffer-selected-window))
	      (current-buffer))))))

;; See also `file-name-base'
(defun copy-current-file-path ()
  "Add current file path to kill ring."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new filename)
    (message "'%s' copied to kill ring" filename)))
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


;; Maybe put in amory-manipulate?  Or *gasp* use? TODO
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


;; From Marcin Borkowski: http://mbork.pl/2022-10-03_Converting_words_and_sentences_to_identifiers
;; Tweaked to also remove from beginning of string
(defun identifierify (beg end)
  "Convert region to an identifier-ish."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (downcase-region beg end)
    ;; Remove from beginning
    (replace-regexp-in-region "\\`[^a-z]+" "" (point-min) (point-max))
    ;; Remove from end
    (replace-regexp-in-region "[^a-z]+$" "" (point-min) (point-max))
    ;; Slugify
    (replace-regexp-in-region "[^a-z]+" "_" (point-min) (point-max))))
(defalias 'slugify 'identifierify)

;; See how annoying it truly is
;; (setq garbage-collection-messages t)


;; Send notifications via growl
;; https://github.com/jwiegley/alert
;; Use to give note about startup?  ########## ;;;;;;;;; FIXME TODO
(autoload 'alert "alert" "Notification system for Emacs similar to Growl")
(setq alert-default-style 'notifier)

;; Emacs should just have code that automatically sets this threshold
;; according to some function involving a constant, the current date, and
;; Moore's Law.
;; (setq large-file-warning-threshold 50000000)

;; Just in case
(setq warning-suppress-types nil)

;; Browse kill ring, set key to auto-complete with ido
;; https://github.com/browse-kill-ring/browse-kill-ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)
(eval-after-load 'browse-kill-ring
  '(progn
     (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
     (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
     (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous)))
;; Prefer M-y to kill-ring-ido rather than browse-kill-ring (via
;; browse-kill-ring-default-keybindings function)
(autoload 'kill-ring-ido "kill-ring-ido" "Kill-ring browsing with ido" t)
(global-set-key (kbd "M-y") 'kill-ring-ido)


;;; Byte-compile everthing in .emacs.d folder
;; Required for byte-compilation not to fail???
(setq compilation-scroll-output 'first-error)
;; In shell: emacs -batch -f batch-byte-compile ~/.emacs.d/**/*.el
;; Aliased(ish) to recompile_emacs
(defun byte-compile-init-dir ()
  "Byte-compile everything in your `user-emacs-directory'."
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


;; discover-my-major https://framagit.org/steckerhalter/discover-my-major
;; Old and imperfect, but kinda neat
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-h M-m") 'discover-my-mode)


(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "xx" 'amx)
(key-chord-define-global "jj" 'ace-jump-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "ii" 'indent-buffer)
(key-chord-define-global "ww" 'whitespace-cleanup)
(key-chord-define-global "yy" 'browse-kill-ring)
(key-chord-define-global "uu" 'undo-tree-undo)
(key-chord-define prog-mode-map "--" 'undo-tree-redo) ; not in text or conf mode?
(key-chord-define js2-mode-map ";;" "\C-e;")
(key-chord-define cperl-mode-map ";;" "\C-e;")

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

;; Accurate?  Lots of fundamental mode buffers...
(defun buffer-mode-histogram ()
  "Display a histogram of buffer modes."
  (interactive)
  (let* ((ht (make-hash-table :test 'equal))
	 (number-of-buffers (cl-loop for buffer being the buffers
				  for mode-name = (symbol-name (buffer-local-value 'major-mode buffer))
				  do (cl-incf (gethash mode-name ht 0))
				  count 1))
	 (totals (sort (cl-loop for key being the hash-keys of ht
			     using (hash-values value)
			     collect (list key value))
		       (lambda (x y) (if (eql (second x) (second y))
					 (string-lessp (first x) (first y))
				       (> (second x) (second y)))))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
		     number-of-buffers (length totals)))
      (cl-loop for (key count) in totals
	    do (princ (format "%2d %20s %s\n"
			      count
			      (if (equal (substring key -5) "-mode")
				  (substring key 0 -5) key)
			      (make-string count ?+)))))))


;; wc-mode to display chars, words, lines in mode-line
(autoload 'wc-mode "wc-mode" "Toggle word-count mode." t)
;; wc-goal-mode shows original+added/total words
;; https://github.com/bnbeckwith/wc-goal-mode
;; wc-goal-word/line/char-goal to set a target
(autoload 'wc-goal-mode "wc-goal-mode" "Toggle wc-goal-mode." t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulate lines and comments, move around
;; FIXME TODO
(require 'amory-manipulate)
;; A bunch of reference functions, mainly from:
;; http://svn.red-bean.com/repos/kfogel/trunk/.emacs
;; Includes genetic code stuff that needs to be fixed like whoa
;; FIXME TODO
;; Also, other things need cleaning up.  Not all docstrings work? FIXME TODO
(require 'amory-reference-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Not sure these really need to be separate, but maybe it's better
(defun fixme-insert ()
  "Signal something that needs to be dealt with."
  (interactive)
  (insert "FIX" "ME"))
(defalias 'insert-fixme 'fixme-insert)
(defun todo-insert ()
  "Signal something that needs to be dealt with."
  (interactive)
  (insert "TO" "DO"))
(defalias 'insert-todo 'todo-insert)

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

;; FIXME TODO
;; (defun my-tab-fix ()
;; (global-set-key "\t" 'indent-or-expand))
;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; (add-hook 'perl-mode-hook 'my-tab-fix)
;; (add-hook 'sh-mode-hook 'my-tab-fix)

;; Same thing
;; (global-set-key "\t" 'indent-or-expand)
;; (global-set-key (kbd "TAB") 'indent-or-expand)
;; (global-set-key (kbd "C-SOMETHING") 'indent-or-expand)


;; Diminish
;; Hide extraneous minor modeline crap I don't like
;; At the end so that nothing throws an error
(require 'diminish)
(diminish 'auto-complete-mode "ac")
(diminish 'flymake-mode "flym")
(diminish 'flyspell-mode "sp")
(eval-after-load "emmet-mode" '(diminish 'emmet-mode "emm"))
(diminish 'isearch-mode)
(diminish 'abbrev-mode "abv")
(diminish 'editorconfig-mode)
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)
(diminish 'auto-fill-function)
;; (diminish 'font-lock-mode "Fn") ;; Unused
(diminish 'visual-line-mode "vl")
(diminish 'fic-mode)
(diminish 'hungry-delete-mode)
(diminish 'auto-revert-mode)
(diminish 'whole-line-or-region-local-mode)
(diminish 'highlight-parentheses-mode)
(diminish 'undo-tree-mode)
(diminish 'highlight-symbol-mode "hls")
(diminish 'volatile-highlights-mode)
(diminish 'wrap-region-mode)
(diminish 'anzu-mode)
(diminish 'ace-jump-mode "Ace")
;; (diminish 'which-key-mode) ;; Only works transiently, but which-key-lighter customized above
;; (diminish 'guide-key-mode) ;; Unused
;; (diminish 'fancy-narrow-mode) ;; Unused
(diminish 'subword-mode)
(diminish 'with-editor-mode)
;; Diminish doesn't work here??  Dumb
(setq eldoc-minor-mode-string nil)

;; Function to shorten major modes in modeline
;; http://whattheemacsd.com/appearance.el-01.html
(defmacro rename-modeline (package-name mode new-name)
  "Rename the modeline lighter of PACKAGE-NAME to NEW-NAME"
  `(eval-after-load ,package-name
     '(define-advice ,mode (:after (&rest _) rename-modeline)
	(setq mode-name ,new-name))))
(rename-modeline "lisp-mode" emacs-lisp-mode "Elisp")
(rename-modeline "sh-script" sh-mode "Shell")
(rename-modeline "js2-mode" js2-mode "Js2")
(rename-modeline "js" js-mode "Js")

;; Follow-mode on the mode line
;; https://stackoverflow.com/q/11326350/2521092
(add-to-list 'minor-mode-alist '(follow-mode " follow"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(amx-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; END
;; This should probably be in after-init-hook or emacs-startup-hook??
;; https://www.emacswiki.org/emacs/BenchmarkInit
(message "Emacs loaded at %s in %s and %d garbage collections."
	 (format-time-string "%T %a %d %b %y")
	 (emacs-init-time "%.2f seconds")
	 gcs-done)


;;;;Package todos
;; CONSIDER
;; More ace-jump-stuff
;; more ido stuff
;; more auto-complete stuff, company mode?
;; discover-js2-refactor
;; More js/2 stuff?  node/npm mode?
;; ivy?
;; More yasnippet stuff? (helm/ivy)
;; frecentf and other recentf stuff?
;; highlight stuff
;; Sort words but include symbol???? not whitespace https://www.emacswiki.org/emacs/SortWords
;; frog-menu and jump-buffer?
;; magit, github, etc https://endlessparentheses.com/it-s-magit-and-you-re-the-magician.html
;; howdoyou instead of howdoi
;; window-number or numbering????
;; applescript-mode or apples?
;; Try out flx-ido, see if better
;; Try replacements for hl-line+ https://www.emacswiki.org/emacs/HighlightCurrentLine
;; shut-up, maybe for scripts?? https://github.com/cask/shut-up
;; pianobar

;; MAYBE REMOVE
;; more require->autoload stuff (like for fireplace, eprime, dna, etc.)
;; fancy-narrow
;; btc ticker?? Maybe switch
;; Remove typing game, etc?
