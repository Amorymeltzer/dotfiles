
;; ERC, an Emacs IRC client
(when (locate-library "erc")
  (autoload 'erc "erc" nil t)
  (autoload 'erc-select "erc" nil t)
  (autoload 'erc-select-read-args "erc" nil nil) ; needed for XEmacs
  (autoload 'erc-select-ssl "erc" nil t)

  (setq erc-server                         "irc.freenode.net"
        erc-port                           6667
        erc-user-full-name                 "Edward O'Connor"
        erc-email-userid                   "ted"
        erc-nick                           '("hober" "hober2" "janTeto")
        erc-nicklist-use-icons             nil
        erc-password                       nil ; set this in local config
        erc-nickserv-passwords             nil ; set this in local config
        erc-anonymous-login                t
        erc-auto-query                     'bury
        erc-join-buffer                    'bury
        erc-max-buffer-size                30000
        erc-prompt-for-password            nil
        erc-command-indicator              "CMD"
        erc-echo-notices-in-current-buffer t
        erc-send-whitespace-lines          nil
        erc-hide-list                      '("JOIN" "PART" "QUIT")
        erc-ignore-list                    '("jibot"))

  (setq erc-quit-reason-various-alist
        '(("brb"    "I'll be right back.")
          ("lunch"  "Having lunch.")
          ("dinner" "Having dinner.")
          ("food"   "Getting food.")
          ("sleep"  "Sleeping.")
          ("work"   "Getting work done.")
          (".*"     (yow))))

  (setq erc-part-reason-various-alist erc-quit-reason-various-alist
        erc-part-reason               'erc-part-reason-various
        erc-quit-reason               'erc-quit-reason-various)
  (defvar ted-erc-autojoin t
    "Whether or not ERC should autojoin on connect.")

  (defvar ted-erc-identify t
    "Whether or not ERC should identify with NickServ on connect.")

  (setq erc-server-alist
        '(("a11y" a11y "irc.a11y.org" 6667)
          ("apple" apple "irc.apple.com" 6667)
          ("freenode" freenode "irc.freenode.net" 8001)
          ("w3c" w3c "irc.w3.org" 6665))
        erc-networks-alist
        '((a11y "a11y.org")
          (apple "apple.com")
          (freenode "freenode.net")
          (w3c "w3.org")))

  (defun ted-irc (netspec)
    "Interactively select an IRC network to connect to.
  Loosely based on `erc-server-select'."
    (interactive (list (assoc (completing-read "IRC network? "
                                               erc-server-alist nil t)
                              erc-server-alist)))
    (let* ((network (nth 1 netspec))
           (nick (car erc-nick))
           (args (list :server (nth 2 netspec)
                       :port (nth 3 netspec)
                       :nick nick))
           (password (cdr (assoc nick
                                 (cadr (assoc network
                                              erc-nickserv-passwords))))))
      (when password
        (setq args (append (list :password password) args)))
      (setq ted-erc-identify (not (eq network 'freenode)))
      (apply 'erc args))))
