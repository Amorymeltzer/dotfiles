(setq erc-prompt ">"
      erc-fill-column 75
      erc-header-line-format nil
      erc-track-exclude-types '("324" "329" "332" "333" "353" "477" "MODE"
                                "JOIN" "PART" "QUIT" "NICK")
      erc-lurker-threshold-time 3600
      erc-track-priority-faces-only t
      erc-autojoin-timing :ident
      erc-flood-protect nil
      erc-server-send-ping-interval 45
      erc-server-send-ping-timeout 180
      erc-server-reconnect-timeout 60
      erc-server-flood-penalty 1000000
      erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#clojure"
                                     "#leiningen" "#seajure" "#clojuredocs"))
      erc-prompt-for-nickserv-password nil
      erc-accidental-paste-threshold-seconds 0.5
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 14)

(delete 'erc-fool-face 'erc-track-faces-priority-list)
(delete '(erc-nick-default-face erc-fool-face) 'erc-track-faces-priority-list)

(eval-after-load 'erc
  '(progn
     (when (not (package-installed-p 'erc-hl-nicks))
       (package-install 'erc-hl-nicks))
     (require 'erc-spelling)
     (require 'erc-services)
     (require 'erc-truncate)
     (require 'erc-hl-nicks)
     (require 'notifications)
     (erc-services-mode 1)
     (erc-truncate-mode 1)
     (setq erc-complete-functions '(erc-pcomplete erc-button-next))
     (setq-default erc-ignore-list '("Lajla" "wingy" "fasta"))
     (add-to-list 'erc-modules 'hl-nicks)
     (add-to-list 'erc-modules 'spelling)
     (set-face-foreground 'erc-input-face "dim gray")
     (set-face-foreground 'erc-my-nick-face "blue")
     (define-key erc-mode-map (kbd "C-c r") 'pnh-reset-erc-track-mode)
     (define-key erc-mode-map (kbd "C-c C-M-SPC") 'erc-track-clear)
     (define-key erc-mode-map (kbd "C-u RET") 'browse-last-url-in-brower)))

(defun erc-track-clear ()
  (interactive)
  (setq erc-modified-channels-alist nil))

(defun browse-last-url-in-brower ()
  (interactive)
  (require 'ffap)
  (save-excursion
    (let ((ffap-url-regexp "\\(https?://\\)."))
      (ffap-next-url t t))))

(defun pnh-reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update)
  (erc-modified-channels-display))

