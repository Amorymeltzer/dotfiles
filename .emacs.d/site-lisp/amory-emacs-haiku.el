;;; amory-emacs-haiku.el --- Emacs haiku -*- lexical-binding: t; -*-
;; Emacs haiku from http://www.emacswiki.org/emacs/EmacsHaiku

;; Used for scratch initialization
(defvar amory-emacs-haiku
  '("My dot emacs grows
     one day I look inside it
     singularity"
    "Emacs starts, I wait
     while the lisp libraries load
     never close Emacs"
    "The sound of typing
     it can only mean one thing
     dot emacs expands"
    "Beyond the dreams of
     avarice, gnu emacs is
     a hidden treasure"
    "Oort is so awesome
     deuglifies Outlook crap
     `W k' rocks"
    "Great clouds overhead
     Tiny black birds rise and fall
     Snow covers Emacs
	 -- Alex Schroeder"
    "hacking on Smyrno
     `error in process filter'
     something is b0rken
	 -- EdwardOConnor"
    "Swiftly typing. Oh!
     Where would we be without you,
     `self-insert-command'?"
    "treeless quiet field
     sudden bud: EmacsWiki
     now he{ar,re} the birds sing
	 -- ttn"
    "an emacs user's
     fingers dance on the keyboard;
     a nerd pianist
	 -- Erik Bourget"
    "The file was open.
     flying in a sparrow stole
     a parenthesis
	 -- Oliver Scholz"
    "The day went away.
     The file still puts its weight on
     the tired mode-line.
	 -- Oliver Scholz"
    "On a cloudy day
     you hear the cons cells whisper:
     'We are lost and gone.'
	 -- Oliver Scholz"
    "A message, a string
     remind me of my sweet love.
     Good bye, my buffers.
	 -- Oliver Scholz"
    "Hot night in summer:
     Hush, you quibbling characters!
     Do not wake her up!
	 -- Oliver Scholz"
    "A bright, busy day.
     The windows watch a thousand
     wild cursors dancing.
	 -- Oliver Scholz"
    "Oh, why don't you are
     a lake, a stream, a meadow
     this morning, Emacs?
	 -- Oliver Scholz"
    "The friends chat gaily,
     I stand up to join their talk.
     My `save-excursion'.
	 -- Oliver Scholz")
  "Haiku taken from the Emacs Wiki's EmacsHaiku page.")

;; Function for choosing a random haiku from the above list
(defun amory-random-emacs-haiku (&optional prefix)
  "Select and format a random haiku from `amory-emacs-haiku'.  Optional
PREFIX to determine what, if any, characters to lead each line with;
defaults to lisp `;; `."
  (random t)
  (let* ((prefix (or prefix ";; "))
	 (n (random (length amory-emacs-haiku)))
	 (haiku (nth n amory-emacs-haiku)))
    (with-temp-buffer
      (insert haiku)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(goto-char (line-beginning-position))
	(delete-horizontal-space)
	(insert prefix)
	(when (looking-at "--")
	  (insert "    "))
	(forward-line 1))
      (concat (buffer-substring-no-properties (point-min) (point-max))
	      "\n\n"))))

(provide 'amory-emacs-haiku)

;;; amory-emacs-haiku.el ends here
