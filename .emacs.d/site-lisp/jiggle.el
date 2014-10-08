;;; jiggle.el --- Minor mode to jiggle cursor when changing buffers

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Mo 16 Feb 98
;; Version: 0.44, Th 29 Oct 98
;; Keywords: frames, hardware, jiggle, cursor, shake

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; When small fonts are used to pack a maximum amount of text onto the
;; screen, the steady box cursor used by default with Emacs under windowing
;; systems can be hard to see (unlike the blinking cursor that is common
;; under terminal systems).  We discern motion more easily than color or
;; shape, so jiggling the cursor on a large frame lets it be seen instantly
;; without searching.

;; This package implements an interactive function, jiggle-cursor, that does
;; that, selecting the direction of the jiggle to avoid repositioning the
;; buffer in the window and to constrain the movement to a small area of the
;; screen.  If this is impossible, such as when point is between tabs on the
;; middle line of three text lines each of which is so long that it wraps
;; around to at least two vertical screen lines, that function is a NOP, so
;; simpler functions, jiggle-cursor-{left,right,laterally}, are provided that
;; just do what they say, even if the result looks strange.  There is also a
;; minor mode, jiggle-mode, that runs jiggle-cursor every time you switch
;; buffers.  That's implemented in terms of a hook that's run every time you
;; switch buffers; no such hook is built into GNU Emacs 19, so its
;; implementation is also part of this package's interface, and using that
;; hook might be a sufficient reason to load the package, even if you don't
;; want cursor jiggling.

;; This package was developed under the Windows NT port of GNU Emacs 19.34.1,
;; where it was tested under Windows 95 with an 800x600 VGA screen and under
;; NT 4.0 with resolutions from 1024 to 1280.  Andre Srinivasan
;; <andre@visigenic.com> reported that it works with XEMacs 20.3, but I've
;; had other reports that it failed under XEMacs 20.4; I don't use XEMacs (&
;; it may be a while before that changes) so I can't hack the problems
;; myself.  There was a bug in the 19.34.1 version that caused a failure
;; under 19.34.6, displaying an unwanted text-mode menu bar; that's been
;; fixed.  I haven't tried this under GNU Emacs 20 yet.

;; You can use this package either interactively or from your .emacs file.
;; In either case, first you'll need to copy this file to a directory that
;; appears in your load-path.  `load-path' is the name of a variable that
;; contains a list of directories Emacs searches for files to load.
;; To prepend another directory to load-path, put a line like
;; (add-to-list 'load-path "c:/My_Directory") in your .emacs file.

;; Then, put the line
;;   (require 'jiggle)
;; in your .emacs file.  Then, if you want the cursor jiggled automatically
;; every time you switch buffers, put the line
;;   (jiggle-mode 1)
;; after it.  If you want to be able to jiggle the cursor manually, bind
;; jiggle-cursor to a key; for example,
;;   (global-set-key (read-kbd-macro "C-c C-SPC") 'jiggle-cursor)
;; binds it to <Ctrl-c><Ctrl-Space>.  Automatic and manual jiggling are
;; independent of each other; jiggle-cursor works even if jiggle-mode is
;; disabled, and you don't need to bind jiggle-cursor to use jiggle-mode.

;; Interactively, you can toggle jiggling with M-x jiggle-mode.

;; By default, jiggle-mode only jiggles the cursor when you switch
;; buffers.  However, another situation when the cursor can be hard to
;; find is when you're doing searches, either incremental searches with
;; C-s or C-r, or the searches that are part of a query-replace function.
;; To interactively toggle whether jiggle-mode also jiggles the cursor
;; after searches, type M-x jiggle-searches-too.  This option is off by
;; default because it looks rather weird for incremental searches; since
;; they're incremental, the cursor jiggles after *every keystroke*.
;; If you decide you want this permanently enabled, put the line
;;   (jiggle-searches-too 1)
;; after the line
;;   (jiggle-mode 1)
;; in your .emacs file.  You may also want the lines
;;   (setq query-replace-highlight t)
;;   (setq search-highlight t)
;; which set two variables that are standard in GNU Emacs; I use both of
;; those as well as (jiggle-searches-too 1).

;; If you're visually impaired, you may want jiggling to last longer than it
;; does by default, which is 3 jiggles.  Put a line like
;;   (setq jiggle-how-many-times 8)
;; in your .emacs to increase the default from 3 to 8, for example.  That
;; line can go before or after (require 'jiggle).  Also, if it helps,
;;   (setq jiggle-sit-for-how-long .1)
;; slows down the jiggling by a factor of 10 (the default is .01),
;; theoretically, although in real life contemporary computers aren't that
;; fast or precise, so the effect will be smaller.

;; Another consideration is that jiggle-cursor sometimes does nothing if
;; it's in a position where jiggling would "look ugly".  If you can't find
;; the cursor without it then you don't care whether it's ugly, so you may
;; prefer the jiggle-cursor-laterally function; it always jiggles somehow
;; except in an empty buffer.  It can be used instead of jiggle-cursor in
;; the global-set-key example given above.  You can also put the line
;;   (setq jiggle-cursor-function 'jiggle-cursor-laterally)
;; in your .emacs to make that the function used by jiggle-mode.

;; Executing (require 'jiggle) implements jiggle-buffer-switch-hook.  If you
;; neither bind jiggle-cursor nor enable jiggle-mode, then making that hook
;; available is the only effect of (require 'jiggle).  This is a normal hook
;; run every time the selected buffer is switched; it is run in the
;; newly-selected buffer.  The hook is implemented without using
;; p{re,ost}-command-{,idle}-hook, so it's reasonably efficient; on a 486/50
;; under Windows 95 it entails no performance hit I can discern.

;;; Code:

;; The sections of this package are separated by five-leading-semicolon
;; headers, & the best way to quickly move thru them is to use
;; outline-minor-mode, for which control variable definitions are provided at
;; end of file.  If you're not familiar with outline-minor-mode, just
;; remember that you type C-c @ C-t to get a listing of all the headings,
;; type the usual motion keys to move among them, & type C-c @ C-a to see all
;; the text again.  When you realize how convenient this is, see Emacs Info
;; on Outline Mode & remap outline-minor-mode-prefix to something less
;; carpally hostile (I use C-c C-k).  M-x imenu may also help.

(eval-when-compile (require 'cl))

;;;;; ************************* USER OPTIONS ************************** ;;;;;

(defvar jiggle-how-many-times 3
  "How many times (jiggle-cursor) does so.")

(defvar jiggle-sit-for-how-long 0.01
  "How many seconds (jiggle-cursor) pauses between cursor movements.")

(defvar jiggle-cursor-function nil
  "If non-nil, the function called in jiggle-mode to jiggle the cursor.
Provided only for customization; jiggle-mode still works if this is nil.

If you assign a value to this variable, the value should be a function of
one argument indicating the number of times to jiggle the cursor; if the
argument is nil, it should default to jiggle-how-many-times; if non-nil,
it should be preprocessed by the `prefix-numeric-value' function before
being used as a number.")

(defvar jiggle-enabled t
  "Non-nil means (jiggle-cursor) works, nil means it has no effect.
Normally if you don't want jiggling you just wouldn't enable the minor mode,
but this allows for the possibility that you'll use defadvice to jiggle after
actions like (backward-paragraph), & (jiggle-mode nil) won't affect your
(defadvice)s because jiggle.el doesn't know about them.  If you then find
yourself running on a machine where jiggling is inappropriate, you can
disable it with (setq jiggle-enabled nil).")
;; The jiggle-enabled variable is also useful to programmers; if you want
;; to temporarily disable jiggling during a function, wrap
;;   (let ((jiggle-enabled nil))
;;      ... )
;; around the code where jiggling is disabled.

;; Jiggling will also be disabled while executing keyboard macros, since
;; these often use searches for positioning, & jiggling during the searches
;; would slow them down.

;; Probably quite a few more functions should be in these lists:

(defvar jiggle-disabled-during '(apropos
                                 apropos-command
                                 Buffer-menu-execute
                                 desktop-read
                                 )
  "Functions during which jiggling is disabled.

To add your own functions to this list, execute a form like

\(add-hook 'jiggle-setup-hook (lambda () (nconc jiggle-disabled-during
                                               '(my-first-function
                                                 my-second-function))))

before jiggle.el is loaded.")

(defvar jiggle-postponed-during '(calendar
                                  describe-function
                                  describe-key
                                  describe-variable
                                  exit-calendar
                                  Info-find-node
                                  mouse-choose-completion
                                  )
  "Functions during which jiggling is postponed.

To add your own functions to this list, execute a form like

\(add-hook 'jiggle-setup-hook (lambda () (nconc jiggle-postponed-during
                                               '(my-first-function
                                                 my-second-function)))

before jiggle.el is loaded.")


;; We need to run jiggle-setup-hook now, not at the end of the load, so the
;; user can modify any variables before they're used to construct advices.
;; That's why it's not named 'jiggle-load-hook; hooks with such names are
;; normally run at the end of a load.  (This package has such a hook, & a
;; jiggle-mode-hook, just "for completeness", but I'm not aware they'd
;; actually be useful.)

(run-hooks 'jiggle-setup-hook)

;;;;; ************************ JIGGLE "ENGINE" ************************ ;;;;;

(defun jiggle-current-window-column ()
  "What column of the window (not of the buffer line) the cursor is on.
Compare (current-column), which returns the column of the buffer line."
  (let* ((true-window-width (1- (window-width)))
         (column-if-wrapping (% (- (current-column) (window-hscroll))
                                true-window-width)))
    (if (and (zerop column-if-wrapping)
             ;A zero value can be wrong if not wrapping
             (not (bolp))
             (not truncate-lines)
             (or (eolp)
                 (and (boundp 'show-paren-mode)
                      show-paren-mode
                      (= (char-syntax (preceding-char)) 41))))
                      ;41 = ")"; the character syntax "?\)" messes up
                      ;show-paren-mode & even, sometimes, (read).
                      ;If you don't believe me try "?\["; it's even worse.
        true-window-width
      column-if-wrapping)))

;; The reason for having that function is that I need to decide whether to
;; jiggle the cursor to the left, right, up, or down.  I can't jiggle left if
;; (bolp) or right if (eolp), for example.  Furthermore, if (eolp), jiggling
;; vertically will change (current-column) if the target line is shorter than
;; the current line, or is in the middle of an explicit tab at
;; (current-column).  Changing lines with (next-line) can have results that
;; are undesired here if track-eol, so that needs to be allowed for.  I don't
;; want jiggling to move more than one visible position, so I have to be
;; careful when (null truncate-lines), which is the default, about lines that
;; wrap around the right edge of the window.  And I can't jiggle to a point
;; that's not currently visible on the screen, because that would reposition
;; the buffer in the screen.

;; There's one case where it's impossible to jiggle & avoid error:
;; when (and (boundp 'show-paren-mode)
;;           show-paren-mode
;;           (= (jiggle-current-window-column) (1- (window-width)))
;;           (not (eolp))
;;           (= (char-syntax (preceding-char)) ?\)))).
;; In that case, jiggling left is less harmful than jiggling right;
;; left gives flicker against the 0 position of the following line, whereas
;; right does a flicker-free jiggle at that position but then moves the
;; cursor back up to the end of the current line.  Jiggling vertically or
;; diagonally fail analogously to jiggling right.

;;;###autoload
(defun jiggle-cursor (&optional arg-how-many-times)
  "Jiggle cursor to call your attention to where it is.
The direction in which to jiggle is dynamically selected to avoid error (such
as trying to jiggle left at the beginning of the buffer), to avoid
repositioning the buffer in the window, & to constrain the movement to the
smallest possible area.  If this is impossible (such as in an empty buffer)
nothing is done.  The global variable jiggle-enabled, which defaults to t, is
also checked; if it's nil nothing is done."
  (interactive "P")
  (when (and jiggle-enabled
             (not executing-kbd-macro)
             (pos-visible-in-window-p))
    (let ((window-column (jiggle-current-window-column))
          (entry-dot (point))
          other-dot)
      (cond (jiggle-cursor-function                       ;Jiggle customized?
             (funcall jiggle-cursor-function arg-how-many-times))
            ((and (or (> window-column 1)                 ;Jiggle left?
                      (and (= window-column 1)
                           (= (current-column) 1)))
                  (/= (preceding-char) ?\t))
             (jiggle-cursor-left arg-how-many-times))
            ((not (or (eolp)                              ;Jiggle right?
                      (> window-column (1- (window-width)))
                      (= (following-char) ?\t)))
             (jiggle-cursor-right arg-how-many-times))
            ((and (save-excursion                         ;Jiggle up?
                    (zerop (forward-line -1)));so I'm not on line 1
                  (save-excursion
                    (let ((goal-column (current-column)))
                      (previous-line 1)
                      (setq other-dot (point))
                      (and (< (- goal-column (current-column)) 2)
                           (or truncate-lines
                               (< (- entry-dot other-dot)
                                  (1+ (window-width))))
                           (pos-visible-in-window-p)))))
             (jiggle-cursor-to other-dot arg-how-many-times))
            ((save-excursion                              ;Jiggle down?
               (condition-case nil
                   (let ((goal-column (current-column)))
                     (next-line 1)
                     (setq other-dot (point))
                     (and (< (- goal-column (current-column)) 2)
                          (or truncate-lines
                              (< (- other-dot entry-dot)
                                 (1+ (window-width))))
                          (pos-visible-in-window-p)))
                 (end-of-buffer nil)))
             (jiggle-cursor-to other-dot arg-how-many-times))))))

;;;###autoload
(defun jiggle-cursor-right (&optional arg-how-many-times)
  "Jiggle cursor between current char & char to right."
  (interactive "P")
  (and jiggle-enabled
       (not executing-kbd-macro)
       (not (eobp))
       (jiggle-cursor-to (1+ (point)) arg-how-many-times)))

;;;###autoload
(defun jiggle-cursor-left (&optional arg-how-many-times)
  "Jiggle cursor between current char & char to left."
  (interactive "P")
  (and jiggle-enabled
       (not executing-kbd-macro)
       (not (bobp))
       (jiggle-cursor-to (1- (point)) arg-how-many-times)))

;;;###autoload
(defun jiggle-cursor-laterally (&optional arg-how-many-times)
  "Jiggle cursor between current char & char before or after.
Note that on a zero-length line, lateral jiggling looks vertical,
& at the end of a line, it can jiggle to the beginning of the next line.
This function may be more useful than jiggle-cursor if you're visually
impaired, since it *always* jiggles *somehow* except in a zero-length buffer,
whereas jiggle-cursor may do nothing if jiggling would look ugly."
  (interactive "P")
  (funcall (if (eobp) 'jiggle-cursor-left 'jiggle-cursor-right)
                arg-how-many-times))

;; Looking at paren.el I think that if the file has been loaded but the mode
;; disabled, there is no effect on system behavior.  If the mode is enabled,
;; then idle time causes the cursor to be repositioned strangely, but that
;; only happens after idle time.  It's *possible* the (sit-for) matters even
;; if its amount is less than that idle time.  It's possible to get
;; show-paren-function to do nothing even if show-paren-mode by spoofing the
;; window-system variable.  I've had that seem to help even when (not
;; show-paren-mode); this could be being caused by other similar idle timers.
;; ... Under 19.34.6 that spoof, (let ((window-system nil)) ...), caused
;; a text-mode menu bar to be displayed on every buffer switch.  That's
;; understandable & useful in text mode, more useful than the spoof, which
;; only applied to strange circumstances with wraparound lines.  I tried
;; to double-spoof by setting menu-bar-mode to nil inside the same let, but
;; that had no effect, so I killed the spoof to fix the bug.

(defun jiggle-cursor-to (other-position &optional arg-how-many-times)
  "Jiggle cursor between (point) and OTHER-POSITION ARG-HOW-MANY-TIMES.
If (null ARG-HOW-MANY-TIMES) it defaults to jiggle-how-many-times; otherwise
it's construed as a raw prefix arg, so e.g. either 4 or '(4) mean 4."
  (let ((inhibit-point-motion-hooks t))
    (loop repeat (prefix-numeric-value (or arg-how-many-times
                                           jiggle-how-many-times))
          do
          (save-excursion
            (goto-char other-position)
            (sit-for jiggle-sit-for-how-long))
          (sit-for jiggle-sit-for-how-long))))

;;;;; ********************** BUFFER-SWITCH-HOOK *********************** ;;;;;

;;; Definition of the hook:

(defvar jiggle-buffer-switch-hook nil
  ;; Nothing like this is built in to Emacs 19.34, so I need to hack.  This
  ;; seems to work, sort of, but I'd really rather have something that only
  ;; ran when the *user* switched buffers.  Some commands use
  ;; (switch-to-buffer) or (pop-to-buffer) in contexts where the hook
  ;; *should* be run, since the effect is a user-level buffer switch; but
  ;; other commands (see "exceptions to the hook" below) use those functions
  ;; in contexts where the user doesn't see a switch.
  "Hook that runs any time the user switches buffers.
Deliberately ignores minibuffer since that has its own hooks.")

;;; Implementation of the hook:

(require 'advice)

(mapcar (function
         (lambda (f)
           (eval
            `(defadvice ,f (after run-jiggle-buffer-switch-hook act)
               "Implement jiggle-buffer-switch-hook."
               (run-hooks 'jiggle-buffer-switch-hook)))))
        '(bury-buffer
          kill-buffer
          other-window
          pop-to-buffer
          switch-to-buffer
          ))

;; 'set-buffer is deliberately excluded from that function list; I don't want
;; to do a user-level action like jiggling the cursor when *code*, rather
;; than the user, is moving among buffers.  If you just need to access the
;; contents of another buffer from inside a function, you should usually be
;; doing it with 'set-buffer, not one of the advised functions above.
;; Unfortunately, not every use of those advised functions is a user-level
;; use, so sometimes the hook produces surprising effects with jiggle-mode.
;; Exceptions to the hook below avoid some of these effects, but don't
;; prevent the hook from being run, so when you write buffer-switching hook
;; code, be aware that buffers can be switched in lots of circumstances, & do
;; some checking to ensure that the context in which your code is running is
;; what you intended.

;;; Exceptions to the hook:

(mapcar (function
         (lambda (f)
           (eval
            `(defadvice ,f (around jiggle-disabled act)
               "Disable jiggle-cursor during this command."
               (let ((jiggle-enabled nil))
                 ad-do-it)))))
        jiggle-disabled-during)

(mapcar (function
         (lambda (f)
           (eval
            `(defadvice ,f (around jiggle-postponed act)
               "Postpone jiggling, if jiggle-mode, to end of command."
               (let ((jiggle-enabled nil))
                 ad-do-it)
               (when jiggle-mode
                 (jiggle-cursor))))))
        jiggle-postponed-during)

;;;;; ************************** MINOR MODE *************************** ;;;;;

(defvar jiggle-mode nil
  "Non-nil turns on jiggling of cursor every time buffer is switched.")

(or (assq 'jiggle-mode minor-mode-alist)
    (callf2 cons '(jiggle-mode " Jiggle") minor-mode-alist))

;;;###autoload
(defun jiggle-mode (arg)
  "Toggle jiggle mode, which jiggles the cursor when switching buffers.
With argument ARG, turn jiggle mode on iff ARG is positive.
When small fonts are used to pack a maximum amount of text onto the screen,
the steady box cursor used by default with Emacs under windowing systems can
be hard to see (unlike the blinking cursor that is common under terminal
systems).  We discern motion more easily than color or shape, so jiggling the
cursor on a large frame lets it be seen instantly without searching."
  (interactive "P")
  (when (setq jiggle-mode
              (if arg
                  (> (prefix-numeric-value arg) 0)
                (not jiggle-mode)))
    (run-hooks 'jiggle-mode-hook))
  (force-mode-line-update)
  (funcall (if jiggle-mode 'add-hook 'remove-hook)
           'jiggle-buffer-switch-hook 'jiggle-cursor)
  (jiggle-adjust-search-advices))

(defvar jiggle-searches-too nil
  "Non-nil means jiggle-mode causes jiggling on isearch or query-replace.
This variable only has an effect when (jiggle-mode) is invoked,
so users should invoke the jiggle-searches-too *function*, not
toggle this variable themselves.")

;;;###autoload
(defun jiggle-searches-too (arg)
  "Toggle whether jiggle mode also jiggles the cursor during searching.
With argument ARG, turn search jiggling on iff ARG is positive."
  (interactive "P")
  ;; I considered
  ;;   (interactive (list (y-or-n-p
  ;;                       "Jiggle when searching and/or query-replacing? ")))
  ;; but that leaves the user wondering why the meaning of the arg isn't
  ;; symmetrical with that of the arg to jiggle-mode.  Also, interactive
  ;; experimentation is more convenient if the function is just a toggle.
  (setq jiggle-searches-too
        (if arg
            (> (prefix-numeric-value arg) 0)
          (not jiggle-searches-too)))
  (jiggle-adjust-search-advices)
  (when (interactive-p)
    (message
     (if jiggle-searches-too
         (if jiggle-mode
             "Cursor will jiggle on searches."
           "Cursor would jiggle on searches, but jiggle-mode is now off.")
       "Cursor will not jiggle on searches."))))

(defvar jiggle-search-functions-to-advise
  '(isearch-update
    replace-highlight)
  "Internal variable used by jiggle-mode.")

(defun jiggle-adjust-search-advices ()
  (if (and jiggle-mode jiggle-searches-too)
      ;; We want search jiggling enabled
      (mapcar (function
               (lambda (f)
                 (eval
                  `(defadvice ,f (after jiggle-search act)
                     "Jiggle the cursor during isearch or query-replace."
                     (sit-for jiggle-sit-for-how-long)
                     ;; Without that, jiggling doesn't happen when searches
                     ;; reposition the screen.
                     (jiggle-cursor)))))
              jiggle-search-functions-to-advise)
    ;; We want search jiggling disabled
    (condition-case nil ;Protect against possibility it was never enabled
        (mapcar (function
                 (lambda (f)
                   (ad-disable-advice f 'after 'jiggle-search)
                   (ad-activate f)))
                jiggle-search-functions-to-advise)
      (error nil))))

;;;;; ***************************** TO DO ***************************** ;;;;;

;;;; ------------------------ THE LOAD PROCESS ------------------------- ;;;;

;; That jiggle-setup-hook is a little strange.  It works as documented & does
;; what's necessary, but I'm not convinced this is the optimal strategy.  The
;; basic problem I'm trying to solve is how to allow the user to specify
;; *additional* elements of a container structure that doesn't exist until
;; this file is loaded.  The (defvar) concept works for scalars just because
;; it doesn't overwrite nonvoid values, but there's no analogous builtin for
;; defining a list of objects that the user can twiddle before the list is
;; loaded.  Perhaps it's possible to code (defvar x) as an expression
;; involving (if (boundp x)), so the user can just code
;;   (setq jiggle-postponed-during '(my-hello-world my-AI))
;; & everything would Just Work ... no, it wouldn't, because the (defvar)'s
;; initialization wouldn't be evaluated if the variable had a value ... but a
;; (defconst) would work, I think.  The same problem occurs with
;; orthodox-prefix-keys in my orthodox.el, & is solved badly there too, just
;; using a (defvar) that requires the user to duplicate the M-o if it's still
;; wanted; although there less harm is done since the initial value of the
;; list has only one element.  Using the (defconst) approach would still
;; allow the user to completely override the contents of the list as well as
;; just add to it; that'd be done by putting code in jiggle-load-hook, a
;; normal load hook which is run at the end of the file, & so doesn't have
;; the code fragility of jiggle-setup-hook, which only works because it's run
;; at a particular point during the load process.  In jiggle-load-hook, the
;; user can either setq the entire list or use delq to delete individual
;; elements; merely adding additional elements, which is what the user is
;; most likely to want to do, is done just by (setq)ing the list before
;; loading the file.  ... Oh wait, shit, *that* doesn't work either, because
;; the load process uses the contents of these lists to do advices, so they
;; need to have achieved the final value the user wants them to have before
;; those advices are created.  So it seems as if a hook must run *before* the
;; load process is finished to give the user control over the advices.  Maybe
;; none of the advice functionals should be inline; they should all be
;; invoked only the first time the mode is turned on.  In a load of jiggle.el
;; resulting from an autoload caused by an invocation of (jiggle-mode 1), I
;; think that'd result in the hook running before the function.  At least it
;; seems that's how it should work, but there are notes in my dotemacs.el
;; about problems I've had with hooks & autoloads in others' packages.

;; It's interesting that the problem is actually *harder* to solve in my
;; orthodox.el, which is a much simpler package, just because there's no
;; function one calls to use orthodox.el; loading the package does
;; everything.  It doesn't seem to me to be a good idea to artificially
;; require the user to invoke some function that would otherwise be
;; unnecessary just for the purpose of coping with customization.

;; We may actually have a language deficiency here.  It's clear what I
;; *want*: I want a package to be able to assign a single name to a
;; collection of objects that the user can customize either by replacing the
;; entire collection or by selectively adding or deleting elements.  For
;; scalars, (defvar) meets these needs perfectly: a package developer uses it
;; to declare a variable anywhere in a package, and a package user uses
;; (setq) to modify the variable before loading the package, and the
;; customized load process Just Works.  But there's nothing comparable for
;; sequences.  I'm wondering whether it's a language deficiency because a
;; solution to the problem needs to be robust, rather than depending on the
;; precise positioning of a hook like jiggle-setup-hook, and needs to be
;; reasonably easy for package developers to use without needing to be Emacs
;; internals experts.  It should integrate somehow with the new customization
;; facilities (which I haven't seen yet) so users can modify nonscalar
;; package parameters interactively as well as in .emacs.  It's been an
;; annoyance to me that outline-minor-mode can't have its prefix dynamically
;; twiddled, just because the developer couldn't be bothered to make that
;; possible.  Perhaps the language makes such coding too much work.  I'd also
;; like it to be able to have multiple prefixes; at least my orthodox.el did
;; allow that.

;; I need to get a job, I don't have time for this.

;;;; -------------------- THE SWITCH HOOK STRATEGY --------------------- ;;;;

;; It's because (switch-to-buffer) is often used in code where it seems
;; it'd be better to use (set-buffer) that I need to run all those hook
;; exception advice functionals.  But actually (switch-to-buffer) isn't
;; *documented* to be a user-level-only function the way (set-mark-command)
;; is, I'm just construing it that way.  (switch-to-buffer) changes the
;; window/buffer association, which is a perfectly reasonable thing for code
;; to do in the middle of doing other stuff.  The problem arises when
;; jiggle-buffer-switch-hook is run by the invocation of (switch-to-buffer),
;; causing a (sit-for) in the middle of what the code is doing, typically
;; causing unwanted momentary screen displays.

;; There may be a fundamentally superior approach.  There should be a
;; jiggle-buffer-switch-idle-hook complementing
;; jiggle-buffer-switch-hook.  When jiggle-mode is turned on, instead of
;; putting jiggle-cursor in jiggle-buffer-switch-hook, it should put it
;; in jiggle-buffer-switch-idle-hook.  There should be a nonrepeating
;; jiggle-buffer-switch-idle-timer, & a function that loading this file
;; puts into jiggle-buffer-switch-hook [sic] that activates that timer to run
;; after a short idle time (such as .25 s).  That would eliminate both
;; multiple & intermediate jiggles.  The jiggle-disabled-during &
;; jiggle-postponed-during variables might actually become unnecessary
;; using this approach, although I wouldn't eliminate them; now that
;; they've been implemented, they might as well be kept around, just in
;; case some situation arises where they're needed.  But it'd be nice if
;; they were empty by default.

;;;; ----------------------- MINOR ENHANCEMENTS ------------------------ ;;;;

;; A day after I posted the first version of jiggle.el to gnu.emacs.sources,
;; somebody e-mailed back "excellent!", & a week after that, somebody else
;; posted to gnu.emacs.help a complaint about the same problem (giving no
;; indication they'd seen this package), & a fourth person (counting me)
;; posted yet another solution to the problem, that one involving a repeating
;; timer that rotated cursor colors.  So obviously this is a problem a lot of
;; people have faced, one that should have a solution built in to Emacs.

;; I didn't like the color rotator myself because it turns the screen into a
;; damn Nadermas tree, & is distracting when the cursor isn't actually being
;; searched for.  I prefer to use color to indicate minor modes like
;; overwrite-mode.  But this is just a matter of taste, & Emacs should be
;; able to support all variations.

;; Perhaps this package should be generalized to cursor.el, including
;; jigglers, color rotators, blinkers, shape shifters, & anything else
;; anybody wants to contribute.  The jiggle-cursor function would be renamed
;; to cursor-jiggle, and likewise passim.

;; Perhaps the buffer-switch hooks should simultaneously be moved into
;; simple.el, especially if the idle-hook strategy seems good.

;;     "The trouble with doing something right the first time
;;      is that nobody appreciates how difficult it was."
;;           --John L. Wierzbicki <jwierzbicki@melpar.esys.com>

;;;;; ************************* EMACS CONTROL ************************* ;;;;;

;; Local Variables:
;; mode: outline-minor
;; fill-column: 77
;; outline-regexp: ";;;;+"
;; page-delimiter: "^;;;;"
;; End:

(run-hooks 'jiggle-load-hook) ;see documentation of load process conundra

(provide 'jiggle)

;;; jiggle.el ends here