;;; .emacs -*- lexical-binding: t; -*-
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
;; "Anyone with a 4-line .emacs file is suspicious." -Dino Chiesa             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Notes

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
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Regexp-Backslash.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enter debugger on error
(setq debug-on-error t)

;; Local lisp, will take precedence over elpa directory
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
;; Prefer newer files even if not .elc
(setq load-prefer-newer t)

;; Customizations from within emacs interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Themes
(setq
 ;; Custom theme directory, in case
 custom-theme-directory (expand-file-name "themes" user-emacs-directory)
 ;; Treat all themes as safe
 custom-safe-themes t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
(require 'package)
;; Fix for annoying ELPA failure, supposedly fixed upstream but still an issue
;; for me on 27.1; see https://github.com/syl20bnr/spacemacs/issues/12535 and
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; Something to do with a specific GNUTLS version (currently 30615), this exact check is from
;; https://github.com/syl20bnr/spacemacs/blob/d46eacd83842815b24afcb2e1fee5c80c38187c5/core/core-emacs-backports.el
(unless (<= libgnutls-version 30603)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
;; In theory it'd be nice to set package-archive-priorities, but in practice
;; there's no overlap between MELPA and GNU ELPA.  Added in 25.1
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
(package-initialize)

;; Should probably install auto-package-update
;; https://github.com/rranelli/auto-package-update.el

;; use-package https://github.com/jwiegley/use-package
;; install (if not already present) and require, must be around for everyone else
;; Should actually use this...
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)
    (require 'use-package)
    (setq use-package-always-ensure t
	  use-package-verbose t)))


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

(paradox-enable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'kaolin-galaxy)

;; When I was a child, I spake as a child,
;; I understood as a child, I thought as a child:
;; but when I became a man, I put away childish things.
;;   -- 1 Corinthians, 13:11
;; No menu bar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))
;; Prevent the startup message and splash screen
(setq inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen t)

;; UTF-8 always, always, always
(set-default-coding-systems 'utf-8)
(setq-default locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
;; (setq-default buffer-file-coding-system 'utf-8-unix)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent if possible but complete otherwise
(setq-default tab-always-indent 'complete)

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

;; Characters entered before started, up=efficient, down=slower
(setq ac-auto-start 5)
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
(defun setup-ac-for-html ()
  (require 'ac-html)
  ;; I guess?
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-html-setup)
  (setq ac-sources '(ac-source-html-tag
		     ac-source-html-attr
		     ac-source-html-attrv))
  (auto-complete-mode))
(add-hook 'html-mode-hook 'setup-ac-for-html)
(add-hook 'mhtml-mode-hook 'setup-ac-for-html)
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
(setq hippie-expand-try-functions-list '(yas-hippie-try-expand
					 try-complete-file-name-partially
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
    (cl-flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
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

;; Default 10
(setq buffers-menu-max-size 30)

;; Subword mode (consider CamelCase chunks as words)
(global-subword-mode 1)


;;;;;;;;;;;;;;;;;;;
;; JavaScript stuff
;; Use js2-mode instead of js-mode https://github.com/mooz/js2-mode
(require 'js2-mode)
(setq
 js-switch-indent-offset 8	    ; tab indent switch cases
 js2-highlight-level 3		    ; highlight more built-in functions
 js2-mode-indent-ignore-first-tab t ; make first tab doesn't toggle between valid indents
 js2-strict-inconsistent-return-warning nil ; warning if both return and return foo
 js2-strict-trailing-comma-warning t)	    ; trailing commas in array/objects
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist
	     '(("node" . js2-mode)
	       ("nodejs" . js2-mode)))
;; js2-jump-to-definition takes this over, annoying given everyone else respects it
(define-key js2-mode-map (kbd "M-.") 'end-of-buffer)
;; Okay weird but maybe?  Lots of clobbering elsewhere...
(define-key js2-mode-map (kbd "C-c 2") 'js2-jump-to-definition)

;; Add globals for Twinkle development, temporary until I get flycheck up and running
(add-hook 'js2-mode-hook
	  (lambda ()
	    (when (buffer-file-name)
	      (when (string-match "twinkle@azatoth" (buffer-file-name))
		(setq js2-additional-externs
		      '("$" "mw" "Morebits" "Twinkle"))))))
;; Part of js2-mode package
(require 'js2-imenu-extras)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(js2-imenu-extras-setup)
;; I *like* having them, but it's annoying that they're sorted first
(setq js2-imenu-show-other-functions nil)

;; js2-refactor https://github.com/magnars/js2-refactor.el
;; Requires yasnippet and multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
;; Should probably learn more of these https://github.com/magnars/js2-refactor.el#refactorings
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(setq js2r-prefered-quote-type 2)	; single, not double
(js2r-add-keybindings-with-prefix "C-c m")
;; js2-refactor does not work in a buffer that has Javascript parse
;; errors. This tells js2-mode to treat octothorpes and hashbangs as comments,
;; preventing them from causing parse errors
;; (setq js2-skip-preprocessor-directives t)

;; jsdoc https://github.com/mooz/js-doc
;; old and creaky, but okay enough for now.  Honestly, maybe yas would just be better?
(require 'js-doc)
(add-hook 'js2-mode-hook
	  #'(lambda ()
	      ;; clobbers js-set-js-context, whatever that did
	      (define-key js2-mode-map "\C-c\C-j" 'js-doc-insert-function-doc)
	      (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; flymake-eslint
;; Nice to have but slow when enabled, maybe turn on manually?
(autoload 'flymake-eslint-enable "flymake-eslint" "Enable Flymake and add flymake-eslint as a buffer-local Flymake backend." t)
;; (add-hook 'js2-mode-hook
;;   (lambda ()
;;     (flymake-eslint-enable)))
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
;; Relies on flymake-php: https://github.com/purcell/flymake-php
;; which in turn relies on flymake-easy: https://github.com/purcell/flymake-easy
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


;; yaml-mode
(autoload 'yaml-mode "yaml-mode" "Simple mode to edit YAML." t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flymake stuff
;; Really should use flycheck if emacs >= 24 and revert back to flymake
;; otherwise Require flymake. ;;;;;; ##### FIXME TODO
;; flymake is newer on MELPA, as is eldoc
(require 'flymake)
;; Remove annoying logging.  Doesn't quite get all of 'em?  Last one is for
;; `elisp-flymake-checkdoc' on startup, wrong-type-argument integer-or-marker-p nil
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;; Good?  Maybe just for cperl (done there) and elisp, given js2 is fine for js
;; Then again, if this is present, given the presence of flymake-proselint in
;; text-mode-hook, flymake would be on basically everywhere...
;; (add-hook 'prog-mode-hook 'flymake-mode)

;; Deal with stupid jshint/javascript/csslint stuff
;; I really need to migrate to flycheck
(delete '("\\.js\\'" flymake-javascript-init) flymake-allowed-file-name-masks)
(delete '("\\.css\\'" flymake-css-init) flymake-allowed-file-name-masks)

;; Static analysis can be slow, so only run flymake if I've not been typing for
;; 5 seconds.  It will still run on save or hitting return.
(setq flymake-no-changes-timeout 3)

;; Make flymake faces pop
(set-face-attribute 'flymake-note nil
		    :background "DarkGreen" :foreground "white")
(set-face-attribute 'flymake-warning nil
		    :background "DarkBlue" :foreground "white")
(set-face-attribute 'flymake-error nil
		    :background "Firebrick4" :foreground "white")

;;Turn on automatically
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; perlcritic stuff, stored from when flymake-perlcritic was a thing.  Could
;; maybe write a quick miniversion based on flymake-easy or flymake-quickdef?
;; https://github.com/illusori/emacs-flymake-perlcritic
;; (when (executable-find "perlcritic")
;;   (require 'flymake-perlcritic)
;;   (setq flymake-perlcritic-severity 2))

;; https://github.com/purcell/flymake-easy
(require 'flymake-easy)

;; https://github.com/purcell/flymake-php
(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)


;; C-c C-v to go to next error
(global-set-key (kbd "C-c '") 'flymake-goto-next-error)
;; C-c C-C to go to prev error
(global-set-key (kbd "C-c ;") 'flymake-goto-prev-error)

;; Perltidy: https://github.com/emacsmirror/emacswiki.org/blob/master/perltidy.el
;; requires stand-alone command line program; uses ~/.perltidyrc
(require 'perltidy)


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
;; like hl-todo (<hl-todo better? https://github.com/tarsius/hl-todo>) is better
;; (more customization options, ability to jump to each in turn), but, honestly,
;; I kind of just like the simple method here.  In the end, that means something
;; roughly at the level of fic-mode, but probably less-well integrated.  If I
;; really need something more full-fledged, I'll just use hl-todo, with
;; something like the below:
;; (setq hl-todo '(t (:foreground "#e55c7a" :weight normal)))
;; (setq hl-todo-keyword-faces
;;       '(("TODO"  . "pink")
;;	("FIXME" . "#cc9393")
;;	("XXX"   . "#1E90FF")))
(defun my/add-watchwords ()
  (font-lock-add-keywords
   ;; \\<, \\> are empty string at beginning, end of word
   nil '(("\\<\\(FIXME\\|TODO\\|XXX\\)\\>"
	  ;; Consider: rebeccapurple, purple, magenta, violet, pink, etc.
	  ;; These are good candidates for font-lock-function-name-face whenever
	  ;; I customize my theme...
	  1 '((:foreground "pink") (:weight bold)) t))))
(add-hook 'prog-mode-hook 'my/add-watchwords)
(add-hook 'text-mode-hook 'my/add-watchwords)

(font-lock-add-keywords 'emacs-lisp-mode
			'(("autoload" . font-lock-keyword-face)))


;; https://github.com/dgutov/highlight-escape-sequences
;; (require 'highlight-escape-sequences)
;; (setq hes-simple-modes '(emacs-lisp-mode))
;; (hes-mode)

;; Really should figure out and group font-lock stuff ;;;;; #### FIXME TODO
;; Regexp color for backslash, and... escapes?
(set-face-attribute 'font-lock-regexp-grouping-backslash nil
		    :foreground "#ff1493")
(set-face-attribute 'font-lock-regexp-grouping-construct nil
		    :foreground "#ff8c00")


;; (require 'applescript-mode)


;; Make life easier, in all lisps
(define-key lisp-mode-shared-map (kbd "C-x e") 'eval-region)
(define-key lisp-mode-shared-map (kbd "C-x C-e") 'eval-buffer)


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
  (interactive)
  ;; xterm escape sequences: https://tldp.org/HOWTO/Xterm-Title-3.html
  ;; 0: icon name and window title, 1 icon name, 2 window title
  (send-string-to-terminal (concat "\033]1; " (buffer-name) "\007"))
  (if buffer-file-name
      (send-string-to-terminal (concat "\033]2; " (buffer-file-name) "\007"))
    (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007"))))
(add-hook 'post-command-hook 'xterm-title-update)


;; create a backup file directory
;; ` rather than ' needed to selectively evaluate item marked by ,
;; https://emacs.stackexchange.com/a/7487/2051
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
;; Save every on inputs and idle
(setq auto-save-interval 300 ; default 100
      auto-save-timeout 90   ; default 30
      auto-save-default t
      ;; Versions
      delete-old-versions t
      backup-by-copying t
      kept-new-versions 20 ; 6
      kept-old-versions 4  ;2
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
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Better ediff coloring?  Slightly more subtle?  Do for smerge...
(eval-after-load 'ediff
  '(progn
     (set-face-foreground 'ediff-odd-diff-B "#ffffff")
     (set-face-background 'ediff-odd-diff-B "#292521")
     (set-face-foreground 'ediff-even-diff-B "#ffffff")
     (set-face-background 'ediff-even-diff-B "#292527")

     (set-face-foreground 'ediff-odd-diff-A "#ffffff")
     (set-face-background 'ediff-odd-diff-A "#292521")
     (set-face-foreground 'ediff-even-diff-A "#ffffff")
     (set-face-background 'ediff-even-diff-A "#292527")))


;; Options to pass to ls, default just -l
(setq list-directory-verbose-switches "-lh")

;; Updates buffer if file is changed elsewhere, DON'T save until the
;; other process has finished writing!!!  Set manually!
;; (auto-revert-tail-mode 1)

;; Recent files (~/.emacs.d/.recentf)
;; Pointless if saving buffers as below?
(require 'recentf)
(recentf-mode 1)
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
;; Uses ~ instead of full path
(setq recentf-filename-handlers (quote abbreviate-file-name))
;; Same as above?
;; (setq recentf-menu-filter (quote recentf-relative-filter))
;; Try not to save remote files
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 256)   ; 20 items ought to be enough for anybody
(setq recentf-max-menu-items 30)

;; Exclude boring files
(add-to-list 'recentf-exclude "\\.el.gz\\'")
(add-to-list 'recentf-exclude "\\.elc\\'")
(add-to-list 'recentf-exclude "\\/opt\\/local\\/share\\/emacs.*\\'")
(add-to-list 'recentf-exclude "\\.ido.last\\'")
(add-to-list 'recentf-exclude "\\.smex-items\\'")
(add-to-list 'recentf-exclude "\\.recentf\\'")
(add-to-list 'recentf-exclude "\\node_modules\\'")

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


;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
	     '("\\.\\(markdown\\|mdml\\|mkdn\\|text\\|md\\)\\'" . markdown-mode))
;; Edit git commit messages in markdown, since mostly for GitHub.  Not that
;; common since magit/git-commit-mode usurps most of these, but occasionally
;; they do popup, especially around amend/fixup/squash
(add-to-list 'auto-mode-alist
	     '("/\\.git/COMMIT_EDITMSG\\'" . markdown-mode))
(add-to-list 'auto-mode-alist
	     '("/\\.git/PULLREQ_EDITMSG\\'" . markdown-mode))

;; Should add some more here
;; Also need to make interactives for bold, italics, headers, etc
;; Stop clobbering flymake-goto error commands FIXME TODO
(eval-after-load "markdown-mode"
  '(progn
     ;; Colorize code blocks
     (setq markdown-fontify-code-blocks-natively t)
     ;; Add a few missing from block highlighting
     (add-to-list 'markdown-code-lang-modes '("js\\|javascript" . js2-mode))
     (add-to-list 'markdown-code-lang-modes '("css" . css-mode))
     (add-to-list 'markdown-code-lang-modes '("json" . json-mode))
     (add-to-list 'markdown-code-lang-modes '("perl" . cperl-mode))

     ;; key bindings
     (define-key markdown-mode-map (kbd "C-M-f") 'forward-symbol)
     (define-key markdown-mode-map (kbd "C-M-b") 'editutil-backward-symbol)

     (define-key markdown-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
     (define-key markdown-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
     (define-key markdown-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
     (define-key markdown-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
     (define-key markdown-mode-map (kbd "C-c C-u") 'outline-up-heading)))

;; Generate README.md markdown from header of elisp file for github
;; checkdoc might be useful beforehand
;; https://github.com/thomas11/md-readme
(autoload 'mdr-generate "md-readme" "Generate markdown READMEs from elisp" t)

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
;; https://github.com/hniksic/emacs-htmlize
(autoload 'htmlize-buffer "htmllize" "Convert buffer to HTML, preserving colors and decorations.")
(autoload 'htmlize-file "htmllize" "Load file, fontify it, convert it to HTML, and save the result.")
(autoload 'htmlize-region "htmllize" "Convert the region to HTML, preserving colors and decorations.")
(autoload 'htmlize-many-files "htmllize" "Convert files to HTML and save the corresponding HTML versions.")

(defun linkify-region-html (start end)
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert "<a href=\"\">" str "</a>")))

;; Tidy mode to judge your html
;; http://www.emacswiki.org/emacs/tidy.el
;; Should get update elisp and tidy binary
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

;; Bug hunter
;; https://github.com/Malabarba/elisp-bug-hunter
(autoload 'bug-hunter-init-file "bug-hunter" "Find bugs in your init file" t)


;; Auto-insert mode
(auto-insert-mode)
;; (setq auto-insert-query nil) ;; If you don't want to be prompted before
;; auto-insert mode skeletons
(eval-after-load 'autoinsert
  '(progn
     (defvar hashbang-env "#!/usr/bin/env")
     ;; Perl
     (define-auto-insert '("\\.pl\\'" . "Perl skeleton")
       '(nil (concat hashbang-env " perl") \n
	     "# " (file-name-base) " by " user-full-name \n
	     "# " _ \n
	     \n "use strict;" \n
	     "use warnings;" \n
	     "use diagnostics;" \n))

     ;; shell script
     (define-auto-insert '("\\.\\(ba\\)?sh\\'" . "Bash skeleton")
       '(nil (concat hashbang-env " bash") \n
	     "# " (file-name-base) " by " user-full-name \n
	     "# " _ ))

     ;; Python
     (define-auto-insert '("\\.py\\'" . "Python skeleton")
       '(nil (concat hashbang-env " python") \n _ ))

     ;; Ruby
     (define-auto-insert '("\\.rb\\'" . "Ruby skeleton")
       '(nil (concat hashbang-env " ruby") \n _ ))))
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
;; https://github.com/kaz-yos/reveal-in-osx-finder
(autoload 'reveal-in-osx-finder "reveal-in-osx-finder" "Reveal file/folder in finder" t)
(global-set-key (kbd "C-c z") 'reveal-in-osx-finder)

;; Save a list of open files in ~/.emacs.d/emacs.desktop(.lock)
;; Rename to just desktop(.lock)???
;; Not great with emacsclient server?
;; Load the desktop *after* all init stuff is done.
(eval-after-load "init"
  '(progn
     (desktop-read)
     (desktop-save-mode 1)))
;; Automatically unless nonexistent
(setq desktop-save 'ask-if-new
      desktop-restore-eager 2		; Load this many buffers, rest when lazy
      desktop-load-locked-desktop 'ask	; Just as a reminder
      desktop-restore-forces-onscreen nil ; Don't restore frames onto the screen
      desktop-dirname user-emacs-directory
      desktop-base-file-name "emacs.desktop"	  ; Not .emacs.desktop
      desktop-base-lock-name "emacs.desktop.lock" ; Not .emacs.desktop.lock
      ;; Don't try to save if file is locked, I'll always be quick
      desktop-not-loaded-hook (quote (desktop-save-mode-off))
      desktop-path (quote (user-emacs-directory "~"))) ; Just in case

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

;; Save the desktop automatically whenever Emacs has been idle for some time
(defvar desktop-save-time
  (run-with-idle-timer 60 t (lambda ()
			      (garbage-collect)
			      (my/save-desktop-with-message)))
  "idle-timer (see \\[run-with-idle-timer]) to save desktop \\[cancel-timer] to cancel it.")
(defun my/save-desktop-with-message ()
  (interactive)
  (desktop-save desktop-dirname)
  (message "Desktop saved at %s, memory-limit %x."
	   (format-time-string "%T %a %d %b %y")
	   (memory-limit)))

;; Open at last place visited in a file
;; Any overlap with desktop or persistency? Not great with emacsclient
(require 'saveplace)
(save-place-mode)
(setq save-place-file (expand-file-name "saved-places" user-emacs-directory))


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

;; Modeline customizations
;; Time
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode t)

;; Battery percentage
(when (not (equal "Battery status not available"
		  (battery)))
  (display-battery-mode t)
  (setq battery-update-interval 180 ;; Default 60s
	battery-mode-line-limit 95  ;; Default 100
	battery-load-low 20	    ;; Default 25
	battery-load-critical 10))  ;; Default 10

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

;; Allow scrolling (not off-screen) during search
;; Kind of weird if going up?
(setq isearch-allow-scroll t
      ;; Add search commands to history, allow resuming
      isearch-resume-in-command-history t)

;; Scroll history in isearch, same as minibuffer
(define-key isearch-mode-map (kbd "M-p") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "M-n") 'isearch-ring-advance)

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

;; Visual feedback for regex replace
;; Should I alias the above to this?  Maybe. ;;;;;; ####### FIXME TODO
;; Also setup isearch, isearch regexp for this style?  Probably
;; Replace is fast, query asks
;; Only works DOWN a buffer
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

;; Display number of matches in modeline for isearch.  Use anzu instead of
;; visual regexp since it does much the same thing??? ;;;;;;; ##### FIXME TODO
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
(setq ace-jump-mode-scope 'frame)	; Only look in current frame, not all windows

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



;; Utilize system's trash can
(setq-default delete-by-moving-to-trash t
	      trash-directory "~/.trash/emacs")

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

;; Related, neat
(global-set-key (kbd "M-k") 'comment-kill)
;; Will add a comment if no comment, could just take over M-; FIXME TODO
(global-set-key (kbd "M-i") 'comment-indent)
;; Previously M-i was tab-to-tab-stop, which I never used but is neat, whereas
;; C-x i was ido-insert-file, which I *don't* need a command for
(global-set-key (kbd "C-x i") 'tab-to-tab-stop)


;;;;;;;;;;;;;;;;;;;
;; Dired, file browser and then some
;; Should really look at dired-mode-map for the full suite of commands
;; Like, v and o are dope!
;; Check out dired-hacks for filter, sort, narrow, colors, etc https://github.com/Fuco1/dired-hacks
(require 'dired)
(setq dired-auto-revert-buffer t
      dired-dwim-target t		; seems useful?
      dired-hide-details-hide-symlink-targets nil
      dired-listing-switches "-Flagoth")
(when (eq system-type 'darwin)
  (setq-default dired-ls-F-marks-symlinks t ; OSX uses @ after symlinks
		dired-use-ls-dired nil))    ; OSX ls doesn't support --dired
(add-hook 'dired-mode-hook 'hl-line-mode)
;; Use a rather than return so as not to open up so many damn windows
;; Don't warn/disable/whatever
(put 'dired-find-alternate-file 'disabled nil)
;; Remap ^ to use find-alternate-file to move up, thus not opening another dired
;; buffer.  In theory that might be nice, but in practice I'm just passing through.
;; Could probably just use dired-single https://github.com/crocket/dired-single
(defun my/dired-move-up-directory ()
  "Move up one directory without opening yet another Dired buffer.  Mimics
`dired-find-alternate-file', but uses `find-alternate-file' it its place so as
to explicitly provide `..' as an argument.  Will be remapped to `^'."
  (interactive)
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
(setq wdired-allow-to-change-permissions 'advanced)
;; Available as C-x C-q, but nice to be able to toggle more easily
(define-key dired-mode-map (kbd "C-w") 'wdired-change-to-wdired-mode)
;; The mode-map isn't around until loaded
(with-eval-after-load "wdired"
  (define-key wdired-mode-map (kbd "C-w") 'wdired-abort-changes)
  (define-key wdired-mode-map (kbd "C-c k") 'wdired-abort-changes))

;; dired-x: ignore uninteresting files
(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
;; Feels weird that dired-jump is part of dired-x...
(global-set-key (kbd "C-x C-d") 'dired-jump) ; C-x d already dired...
;; By default excludes ., ., autosave files, and lockfiles(?); add stupid OSX
;; .DS_Store files
(setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|\\`.DS_Store\\'")


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
;; Was slower, but now... just not as good?
;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; (setq ido-use-faces nil)

;; Not exactly sure but it sounds nice, right?  Use ido-completing-read+
(setq ido-everywhere t)
;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
;; Formerly ido-ubiquitous
(require 'ido-completing-read+)
(ido-ubiquitous-mode t)
;; Fix for weird issue https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28774
(defun ido-name (item)
  ;; Return file name for current item, whether in a normal list
  ;; or a merged work directory list.
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
(setq ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-directory)
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
;; (global-set-key (kbd "C-x f") 'recentf-open-files)
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


;; Really need to figure these out FIXME TODO
;; C-x C-b is crap
;; Better/easier buffer menu???
;; (global-set-key (kbd "C-c C-b") 'bs-show)
;; What about ibuffer?!  Also iswitchb-mode for switching...
;; (global-set-key (kbd "C-c C-b") 'ibuffer
;; Consider this from http://emacs.stackexchange.com/q/2177/2051
;; (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
;; ibuffer-vc would be dope too https://github.com/purcell/ibuffer-vc
;; Try this:
;; (add-hook 'ibuffer-hook
;;		  (lambda ()
;;		    (ibuffer-vc-set-filter-groups-by-vc-root)
;;		    (unless (eq ibuffer-sorting-mode 'alphabetic)
;;		      (ibuffer-do-sort-by-alphabetic))))

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
(setq smex-save-file (expand-file-name "smex-items" user-emacs-directory))
(smex-initialize)

;; Only a slight speed enhancement, but let's be honest: I'm not loading tons
;; of code building new functions all the time here
(setq smex-auto-update nil)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is the old M-x
(global-set-key (kbd "C-c M-X") 'execute-extended-command)
(setq smex-prompt-string "Smx ")
(setq smex-history-length 512)

;; Cleaner, more meaningful narrow-to-region
;; https://github.com/Bruce-Connor/fancy-narrow
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
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-region)

;; Comment colors
(set-face-attribute 'font-lock-comment-face nil :foreground "black")
;; Comment-starter color
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "red")


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
;; Customize hl-line, etc. colors
(with-eval-after-load "hl-line"
  (set-face-attribute 'hl-line nil :background "black" :foreground "white")
  ;; Doesn't perfectly hidge the fringe lines, but sobeit
  (set-face-attribute 'vertical-border
		      nil :background "black" :foreground "black"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Point out changes
;; I hate this
;; (highlight-changes-mode 1)


;; Magit stuff
;; C-x g for status, but C-x M-g for magit-dispatch: trigger command directly
;; C-c M-g: magit-file-dispatch; super convenient, so let's make it more so
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
;; Open log, etc. in separate window?  Like vc-print-log
;; Look into tweaking faces?
;; Prior to magit, I turned off built-in vc handling, preferring manual git:
;; (delete 'Git vc-handled-backends) ;; delete git from list of backends
;; (setq vc-handled-backends nil) ;; delete all backends
;; Probably don't need to turn it off if using magit, and apparently doing so
;; can muck up magit so it won't follow symlinks?  It seems like just setting
;; vc-follow-symlinks does the trick?
;; https://github.com/magit/magit/issues/2250#issuecomment-138906601
(setq vc-follow-symlinks t)
;; git-commit-style-convention-checks take overlong-summary-line if want to
;; ensure git commit is within guidance (git-commit-summary-max-length)
;; git-commit-turn-on-flyspell ???
(setq git-commit-major-mode 'markdown-mode
      git-rebase-confirm-cancel nil)
;; Edit git messages in markdown as these are mostly targeted for GitHub
(add-hook 'git-commit-mode-hook 'markdown-mode)
(setq magit-log-section-commit-count 25 ; default 10
      ;; Display buffers in same buffer, except for diffs
      ;; Eh, they multiply too much, things get
      ;; lost. 'magit-display-buffer-traditional is better
      ;; magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
      ;; Turn this off if/when ivy
      magit-completing-read-function 'magit-ido-completing-read
      ;; Not entirely sure what this does, but seems worthwhile
      magit-diff-refine-hunk t)

;; Various items not available right away since not magit require-d...
(with-eval-after-load "magit"
  ;; Add any ongoing merge-log to status sections; check out other magit-insert functions
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-merge-log)
  ;; This is the default less git-commit-turn-on-auto-fill, since I don't want
  ;; to wrap lines in commit messages by default (GitHub don't care)
  (setq git-commit-setup-hook
	'(git-commit-save-message git-commit-setup-changelog-support git-commit-propertize-diff bug-reference-mode with-editor-usage-message)))
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
	(,(getenv "PERL_PERS_DIR") . 1))
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
;; (let ((inhibit-message t))
;;   (setq
;;    ;; Include hidden files like .emacs
;;    magit-todos-rg-extra-args '("--hidden")
;;    ;; Only go one level below the repo directory
;;    magit-todos-depth 1
;;    ;; .git should be handled by the depth, but still.  See also:
;;    ;; https://github.com/alphapapa/magit-todos/pull/115
;;    magit-todos-exclude-globs '(".git/" ".emacs.d/")
;;    ;; hl-todo defines XXX+, a regex, which doesn't work
;;    ;; https://github.com/alphapapa/magit-todos/issues/101
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
(autoload 'gitignore-mode "gitignore-mode" "A major mode for editing .gitignore files." t)
(autoload 'gitattributes-mode "gitattributes-mode" "A major mode for editing .gitattributes files." t)


;; Useful for git related work, although maybe try find-file-in-repo
;; (require 'find-file-in-project)
;; (global-set-key (kbd "C-x f") 'find-file-in-project)

;; smerge-mode, for resolving git conflicts/merges
;; Should really tweak the faces used... FIXME TODO
;; default prefix is C-c ^, which is a pain
(setq smerge-command-prefix "\C-ce")

;; Highlight ( and ) Highlight phrase if no matching paren.
(show-paren-mode t)
;; Could used 'mixed here, but highlight-parentheses better?
(setq show-paren-style 'expression
      show-paren-delay 0.5)
;; Highlight parens currently between
;; https://github.com/nschum/highlight-parentheses.el
(require 'highlight-parentheses)
(highlight-parentheses-mode t)
;; Bold 'em, Color mismatched differently
;; These two share faces
(progn
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'bold)
  (set-face-attribute 'show-paren-match nil :background "black"
		      :foreground nil)
  (set-face-attribute 'show-paren-mismatch nil :background "red")
  (setq highlight-parentheses-colors (quote ("red" "white" "green" "cyan"
					     "red" "white" "green" "cyan"))))

;; Electric-pair parentheses
(add-hook 'prog-mode-hook #'electric-pair-mode)


;; Allow highlighting of phrases.
;; Maybe hook for prog modes (perl, etc.)  ;;;;;;;; ####### FIXME TODO
;; https://github.com/nschum/highlight-symbol.el
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 1) ; default 0.5
;; (setq highlight-symbol-list
;; ;;;;;; ##### FIXME TODO FIX COLORS BEFORE GOING FORWARD
;; (highlight-symbol-mode t)


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
  ;; Should be more clear, need 256 colors FIXME TODO
  (set-face-attribute 'mode-line-inactive nil :background "black"))

;; Display depth indicator, kind of weird but may be useful
(setq minibuffer-depth-indicate-mode t)


;; Easily indent line/region according to mode, or move line/region up or down
;; https://github.com/hbin/smart-shift
(require 'smart-shift)
(global-smart-shift-mode 1)
;; Take these off so they can be used for windmove below
(define-key smart-shift-mode-map (kbd "C-c <up>") nil)
(define-key smart-shift-mode-map (kbd "C-c <down>") nil)
(define-key smart-shift-mode-map (kbd "C-c <left>") nil)
(define-key smart-shift-mode-map (kbd "C-c <right>") nil)

;; Remap C-t to C-x prefix
;; Maybe make a C-t map, put in transpose-frame stuff from manipulate M-t? FIXME TODO
;; (bind "C-t" (lookup-key global-map (kbd "C-x")))

;; Ctrl-q map.  I'll never use quoted-insert, so I might as well make this
;; thing more useful.  Should add to this. ;;;;; #### FIXME TODO
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
    (define-key map (kbd "p") 'backward-paragraph)
    (define-key map (kbd "n") 'forward-paragraph)
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

;; Easily switch to specific window using numbers (if >2)
;; https://github.com/abo-abo/ace-window
;; (require 'ace-window)
(autoload 'ace-window "ace-window" "Quickly switch windows" t)
(global-set-key (kbd "C-x o") 'ace-window)
(eval-after-load 'ace-window
  '(progn
     (set-face-attribute 'aw-leading-char-face nil
			 ;; Make purple FIXME TODO
			 :foreground "deep sky blue"
			 :weight 'bold
			 :height 3.0)))

;; windmove stuff, in case the above simply isn't enough
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(setq windmove-wrap-around t)

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
;; C-x C-SPC
(global-set-key (kbd "C-x C-@") 'exchange-point-and-mark)

;; C-x u for tree, C-_ to undo, M-_ to redo, etc.
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t		 ;; default is off
      undo-tree-visualizer-relative-timestamps t ;; default is on
      ;; Save history
      undo-tree-auto-save-history t
      undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-emacs-directory)))
      ;; Neat mini diff
      undo-tree-visualizer-diff t
      ;; Larger size limits for undo, this might get unwieldy now that I'm
      ;; saving the history across sessions
      undo-limit 32000000	 ;; 160000
      undo-outer-limit 24000000) ;; 24000000

;; Way more likely to remember
(defalias 'uppercase-region 'upcase-region)

;; Highlight region undo, yanked, etc., is awesome
;; https://github.com/k-talo/volatile-highlights.el
(require 'volatile-highlights)
(volatile-highlights-mode t)


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
;; Copy current buffer file contents to clipboard
(defun pbcopy-buffer ()
  "Copy the contents of the current buffer to the GUI clipboard"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "pbcopy")
  (message "Copied contents of %s" buffer-file-name))
(global-set-key [(control c) (p)] 'pbcopy-buffer)


;; Exit in (ansi-)term returns to the emacs buffer
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc))) ad-do-it
	   (kill-buffer buffer)) ad-do-it))
(ad-activate 'term-sentinel)

;; Always use /opt/local/bin/bash, don't ask
(defvar my-term-shell "/opt/local/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
;; Make links in man pages, etc., work, in ansi-mode
(add-hook 'term-mode-hook 'goto-address-mode)

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
(global-set-key (kbd "C-x M-r") '(lambda ()
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
  "Insert the current date in ISO format. With prefix-argument, add day of
week. With two prefix arguments, add day of week and time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
		      ((equal prefix '(4)) "%Y-%m-%d %a")
		      ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
    (insert (format-time-string format))))


;; On duplicate filenames, show paths not <2>
;; Default in 24.4?
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-buffer-name-style 'post-forward)

(defun checksum-region (s e)
  "Print a checksum (currently md5) of the region."
  (interactive "r")
  (message (md5 (buffer-substring s e))))
(defalias 'md5-region 'checksum-region)

(defun region-length ()
  "Calculate length of the selected region."
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;;; Modeline tickers, require request.el
;; Stock tickers, https://github.com/hagleitn/stock-ticker
(autoload 'stock-ticker-global-mode "stock-ticker" "Display stock prives in
the mode line" t)
(setq stock-ticker-display-interval 6)
;; BTC ticker
(autoload 'btc-ticker-mode "btc-ticker" "Minor mode to display
the latest BTC price." t)
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
    (message "%s" (/ (round (* multiplier value)) multiplier))))


;; Guess keybindings game
;; Maybe tweak to get a list for studying?
(autoload 'keywiz "keywiz" "Keywiz keybindings guessing game" t)
;; Poker
(autoload 'poker "poker" "Play a game of texas hold 'em poker" t)
;; Mandelbrot set
(autoload 'u-mandelbrot "u-mandelbrot" "Make a mandelbrot fractal" t)

;; Typing game https://github.com/lujun9972/el-typing-game
(autoload 'typing-game "typing-game" "Typing game" t)

;; emacs-fireplace by @johanvts.  Because emacs
;; https://github.com/johanvts/emacs-fireplace
(autoload 'fireplace "fireplace" "Light a cozy fire.")

;; Stop unicode trolls https://github.com/camsaul/emacs-unicode-troll-stopper
;; MELPA help FIXME TODO
(autoload 'unicode-troll-stopper-mode "unicode-troll-stopper" "Stop unicode
trolls" t)

;; From https://github.com/purcell/emacs-xkcd
;; (require 'xkcd);

;; Requires howdoi to be installed (python)
(autoload 'howdoi "howdoi" "Instant SX answers" t)

;; Locate takes forever...
(setq locate-command "mdfind")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idling stuff

;; Zone out
(autoload 'zone "zone" "Zone out, completely" t)
(autoload 'zone-when-idle "zone" "Zone out when Emacs has been
idle for SECS seconds." t)
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

;; writegood-mode https://github.com/bnbeckwith/writegood-mode
(autoload 'writegood-mode "writegood-mode" "Colorize issues with the writing
in the buffer." t)
;; eprime-mode https://github.com/AndrewHynes/eprime-mode
(autoload 'eprime-mode "eprime-mode" "Check text conforms to E', disallowing forms of \"to be\".")

;; webjump for searching easily
(global-set-key (kbd "C-x j") 'webjump)
(eval-after-load "webjump"
  '(progn
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
		  '("Urban Dict" .  [simple-query
				     "www.urbandictionary.com"
				     "https://www.urbandictionary.com/define.php?term="
				     ""]))
     (add-to-list 'webjump-sites
		  '("IMDB" .  [simple-query
			       "www.imdb.com"
			       "https://www.imdb.com/find?q="
			       "&s=all"]))
     (add-to-list 'webjump-sites
		  '("Google translate" .  [simple-query
					   "translate.google.com"
					   "https://translate.google.com/?sl=auto&tl=en&text="
					   ""]))
     (add-to-list 'webjump-sites
		  '("DOI" .  [simple-query
			      "www.dx.doi.org/"
			      "www.dx.doi.org/"
			      ""]))
     (add-to-list 'webjump-sites
		  '("Youtube" .  [simple-query
				  "www.youtube.com"
				  "https://www.youtube.com/results?search_query="
				  ""]))
     (add-to-list 'webjump-sites
		  '("MediaWiki API" .  [simple-query
					"www.mediawiki.org"
					"https://www.mediawiki.org/wiki/API:"
					""]))
     (add-to-list 'webjump-sites
		  '("MDN" .  [simple-query
			      "developer.mozilla.org"
			      "https://developer.mozilla.org/en-US/search?q="
			      ""]))
     (add-to-list 'webjump-sites
		  '("devdocs" .  [simple-query
				  "devdocs.io"
				  "https://devdocs.io/#q="
				  ""]))

     ;; Some wiki defaults aren't great
     (add-to-list 'webjump-sites
		  '("Emacs Wiki" .  [simple-query
				     "www.emacswiki.org"
				     "https://duckduckgo.com/?q="
				     "+site%3Aemacswiki.org"]))
     (add-to-list 'webjump-sites
		  '("Wikipedia" .  [simple-query
				    "en.wikipedia.org"
				    "https://en.wikipedia.org/wiki/Special:Search/"
				    ""]))))


;; browse-url-of-buffer will render the url assigned to a buffer.  This tells
;; Emacs how to map a given filename to a url. Check out skewer
;; https://github.com/skeeto/skewer-mode
(setq browse-url-filename-alist
      '(("^/\(ftp@\|anonymous@\)?\([^:]+\):/*" . "ftp://\2/")
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
;; Clear out the scratch buffer, initialized with whatever major mode we were
;; just in and with any selected region
(defun new-scratch-buffer nil
  "Initialize the scratch buffer with the highlighted region, setting whatever major mode was active."
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

;;;;;;;;;;;;;;;;;;;;
;; cperl-mode: Better than perl-mode
;; All the cperl options, bad?  Read docs for more info
;; Affects:
;; cperl-font-lock, cperl-electric-lbrace-space, cperl-electric-parens
;; cperl-electric-linefeed, cperl-electric-keywords, cperl-lazy-help-time
;; cperl-info-on-command-no-prompt, cperl-clobber-lisp-bindings
(setq cperl-hairy t)
;; Help in cperl, default is 5s, must be integer.  Can do C-h v (when not
;; clobbering lisp) for immediate help.  See also `cperl-mode-map' for more
;; along that line
(setq cperl-lazy-help-time 1)
;; Don't mess with C-h; would be useful but for the above
;; Not working???
(setq cperl-clobber-lisp-bindings 1)
;; flymake in cperl
(add-hook 'cperl-mode-hook 'flymake-mode)
;; Treat _ as word character, probably counter-intuitive.  cperl-under-as-char
;; is deprecated, so use superword-mode (well, don't, but you know)
;; (setq cperl-under-as-char t)

;; Good?
(setq cperl-highlight-variables-indiscriminately t)

(eval-after-load "cperl-mode"
  '(progn
     ;; Do some color fixin' in cperl-mode, check out cperl-tips-faces
     ;; Really need to play around with these when themin' FIXME TODO
     (set-face-attribute 'cperl-array-face nil :background "nil" :foreground
			 "blue") ; arrays
     (set-face-attribute 'cperl-hash-face nil :background "nil" :foreground
			 "red") ; hashes
     ;; (set-face-attribute 'cperl-nonoverridable-face nil :background "nil"
     ;;				 :foreground "nil") ; `print`, etc.

     ;; Was cperl-next-interpolated-REx-1
     (define-key cperl-mode-map (kbd "C-c C-y") nil)
     ;; Was auto-fill-mode
     (define-key cperl-mode-map (kbd "C-c C-f") nil)
     ;; Was cperl-find-bad-style
     (define-key cperl-mode-map (kbd "C-c C-b") nil)))

;; cperl always better than perl
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'interpreter-mode-alist '("\\(mini\\)?perl5?" . cperl-mode))
;; Not complete but pod can be let through
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
;; Ensure cgi ends up there too
(add-to-list 'auto-mode-alist '("\\.[cC][gG][iI]$" . cperl-mode))

;; Perldoc in emacs
(defalias 'perldoc 'cperl-perldoc)
;;;;;;;;;;;;;;;;;;;;


;; Don't have ruby-mode auto-insert coding utf-8 info on files
(setq ruby-insert-encoding-magic-comment nil)


;;; Should probably figure out a way to diminish these fuckers
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

;; Get perldoc after C-h via P
(define-key 'help-command "P" 'perldoc)

;; Search more than just commands but eh, I never use...
(define-key 'help-command "a" 'apropos)

;; helpful, a better help buffer
;; https://github.com/Wilfred/helpful
(require 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)


;; which-key has better sorting than guide-key
;; https://github.com/justbur/emacs-which-key
(require 'which-key)
(with-eval-after-load "which-key"
  ;; https://github.com/justbur/emacs-which-key#face-customization-options
  ;; (set-face-attribute 'which-key-key-face nil :foreground "magenta")
  (set-face-attribute 'which-key-separator-face nil :foreground "magenta")
  (set-face-attribute 'which-key-note-face nil :foreground "black")
  ;; (set-face-attribute 'which-key-special-key-face nil :foreground "red")
  ;; color of prefix command
  (set-face-attribute 'which-key-group-description-face nil :foreground "blue" :bold nil)
  (set-face-attribute 'which-key-command-description-face nil :inherit nil)

  ;; These first few are from the which-key-setup-side-window-bottom
  ;; function, but here so as not to separate out/overwrite preferences
  (which-key--setup-echo-keystrokes)
  ;; https://github.com/justbur/emacs-which-key#other-options
  (setq which-key-popup-type 'side-window
	which-key-side-window-location 'bottom
	which-key-show-prefix 'top	; default echo
	;; There are a lot of these, all have merits
	which-key-sort-order 'which-key-local-then-key-order
	which-key-side-window-max-height 0.5 ; default 0.25
	which-key-idle-delay 0.25		 ; default 1.0
	which-key-prefix-prefix "=> "	 ; default +
	which-key-show-remaining-keys t
	which-key-lighter nil	; Diminish not working properly...
	))
(which-key-mode)

;; Display what function block if I'm in in certain modes
;; reenable?  prog-mode-hook?
;; (set-face-attribute 'which-func nil
;;                     :foreground "LightPink3" :weight 'bold)
;; (add-hook 'sh-mode-hook 'which-function-mode)
;; (add-hook 'emacs-lisp-mode-hook 'which-function-mode)


;; Jump to a definition in the current file (holy shit this is awesome)
;; Does this automatically use ido?  Others think it doesn't but I do...
(global-set-key (kbd "C-c i") 'imenu)
(setq-default
 imenu-auto-rescan t ; Always rescan buffers
 imenu-auto-rescan-maxout 90000 ; Only if they're under 90k
 imenu-sort-function 'imenu--sort-by-name
 imenu-after-jump-hook 'recenter)
;; There's no interactive to force rescanning?  Fine but seems weird
(defun imenu-rescan ()
  (interactive)
  (imenu--menubar-select imenu--rescan-item))
(global-set-key "\C-cI" 'imenu-rescan)

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
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;; Give info at point in elisp mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.25)		;Default is 0.5
;; Not really useful, but at least I'll remember this
(defalias 'elisp-repl 'ielm)


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


;; Custom SCOWL dictionary: size 70, variants 2, diacritics both
;; http://app.aspell.net/create
(setq ispell-dictionary "en-custom"
      ;; This should be the default for aspell, but just in case...
      ;; See also:
      ;; http://aspell.net/man-html/Creating-an-Individual-Word-List.html
      ;; http://aspell.net/man-html/Format-of-the-Personal-and-Replacement-Dictionaries.html
      ispell-personal-dictionary "~/.aspell.en.pws"
      ;; --camel-case is neat, but since I'm using flyspell-prog-mode - only
      ;; strings and comments - it's unlikely to be necessary
      ispell-extra-args '("--sug-mode=ultra"))

;; Flyspell spell checking
;; Lots of ispell process instances being started then killed, especially
;; around git/magit?? FIXME TODO
(require 'flyspell)
(setq flyspell-highlight-properties t
      flyspell-issue-message-flag nil
      flyspell-issue-welcome-flag nil)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'fundamental-mode-hook 'flyspell-mode)


;; flymake-proselint https://github.com/manuel-uberti/flymake-proselint
;; Clearly depends on the proselint executable (pip, macports, brew, etc)
;; https://github.com/amperser/proselint/
(add-hook 'text-mode-hook
	  (lambda ()
	    ;; With this, flymake is on for (c)perl and text modes, pretty close
	    ;; to everything I care about...
	    (flymake-mode +1)
	    (flymake-proselint-setup)))


;; FUCKS SHIT UP ;;;;;;; #########
;; Use pretty symbols in buffer
;; (require 'pretty-mode)


;; Some potentially useful stuff from Magnars https://github.com/magnars/.emacs.d
;; Rename file and buffer
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
;; likewise
(put 'delete-current-buffer-file 'ido 'ignore)

(defun insert-file-name ()
  (interactive)
  (insert (file-name-nondirectory
	   (buffer-file-name
	    (if (minibufferp)
		(window-buffer (minibuffer-selected-window))
	      (current-buffer))))))

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



;; See how annoying it truly is
;; (setq garbage-collection-messages t)

;; TEST FOR EMACS VERSION USE FOR IDO AND SMEX
;; ;;;;;;;;;;;;;; ############ FIXME TODO
;; (if (and (>= emacs-major-version 25) (>= emacs-minor-version 2))
;;     (message "Emacs loaded at %s." (format-time-string "%T %a %d %b %y"))
;;   (message "Emacs dumb at %s." (format-time-string "%T %a %d %b %y")))

;; (when (> emacs-major-version 23)
;;   (message "Emacs is >23"))

;; Send notifications via growl
;; https://github.com/jwiegley/alert
;; Use to give note about startup?  ########## ;;;;;;;;; FIXME TODO
(autoload 'alert "alert" "Notification system for Emacs similar to Growl")

;; Emacs should just have code that automatically sets this threshold
;; according to some function involving a constant, the current date, and
;; Moore's Law.
;; (setq large-file-warning-threshold 50000000)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold (* 8 1024 1024)
      gc-cons-percentage 0.2)

;; Just in case
(setq warning-suppress-types nil)

;; Browse kill ring, set key to auto-complete with ido
;; https://github.com/browse-kill-ring/browse-kill-ring
(autoload 'browse-kill-ring "browse-kill-ring" "Browse kill ring" t)
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
(setq compilation-scroll-output t)
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
(key-chord-define-global "yy" 'browse-kill-ring)
(key-chord-define-global "xx" 'er/expand-region)
(key-chord-define-global "zz" 'er/contract-region)
(key-chord-define-global "uu" 'undo-tree-undo)
;; (key-chord-define prog-mode-map "rr" 'undo-tree-redo) ; not in text
(key-chord-define prog-mode-map "--" 'undo-tree-redo) ; not in text or conf mode?

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
;; Just total line count
(eval-after-load "wc-mode" '(setq wc-modeline-format "%tl"))
;; wc-goal-mode shows original+added/total words
;; wc-goal-word/line/char-goal to set a target
(autoload 'wc-goal-mode "wc-goal-mode" "Toggle wc-goal-mode." t)


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
  (insert "F\IXME T\ODO"))
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


;; Diminish
;; Hide extraneous minor modeline crap I don't like
;; At the end so that nothing throws an error
(require 'diminish)
(diminish 'auto-complete-mode "ac")
(diminish 'flymake-mode "fly")
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
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
	(setq mode-name ,new-name))))

(rename-modeline "lisp-mode" emacs-lisp-mode "Elisp")
(rename-modeline "sh-script" sh-mode "Shell")
(rename-modeline "js2-mode" js2-mode "Js2")
(rename-modeline "js" js-mode "Js")

;; Follow-mode on the mode line
;; https://stackoverflow.com/q/11326350/2521092
(add-to-list 'minor-mode-alist '(follow-mode " follow"))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(smex-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline shit
;; Maybe check out moody or doom...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(message "Emacs loaded at %s." (format-time-string "%T %a %d %b %y"))


;;;;Package todos
;; CONSIDER
;; More ace-jump-stuff
;; more ido stuff
;; more auto-complete stuff, company mode?
;; discover-js2-refactor
;; More js/2 stuff?  node/npm
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

;; flymake-perlcritic is using an old version of flymake, can't delete from
;; site-lisp until moving to flycheck
