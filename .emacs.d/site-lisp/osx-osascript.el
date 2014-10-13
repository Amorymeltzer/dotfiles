;;; osx-osascript.el --- invoke osascript on OSX
;;
;; ~/share/emacs/pkg/osx/osx-osascript.el ---
;;
;; $Id: osx-osascript.el,v 1.16 2007/10/25 21:52:47 harley Exp $
;;

;; Author:    Harley Gorrell <harley@mahalito.net>
;; URL:       http://www.mahalito.net/~harley/elisp/osx-osascript.el
;; License:   GPL v2
;; Keywords:  osx, osascript, applescript

;;; Commentary:
;; * run applescript from Emacs via osascript
;; * This is not an applescript mode.
;; * Tested with 21.2

;;; History:
;;  2003-03-22: jhg : written
;;  2004-04-11: jhg : cleaned up for release.

;;; Vars:
(defvar osascript-program "osascript"
  "The path to the osascript program.
Normally /usr/bin/osascript.")

(defvar osascript-args "-ss"
  "Agruments to pass to osascript.")

(defvar osascript-run-library nil
  "List of applescript functions to prepend.")

(defvar osascript-temp-prefix "osascript-"
  "The prefix for osascript temp files.")

(defvar osascript-keep-output nil
  "Keep the output of osascript?")

(defvar osascript-debug nil
  "Keep temp files and buffers for debugging?")
;; (setq osascript-debug t)

;;; Code:

(defun osascript-temp-file-name ()
  "Generate a temporary file name to use."
  (concat (make-temp-file osascript-temp-prefix) ".applescript"))

;;; Canned functions

(defvar osascript-on-elispify "
on elispify(itm)
  if class of itm is integer then
    return itm as string
  else if class of itm is date then
    return \"\\\"\" & (itm as string) & \"\\\"\"
  else if class of itm is boolean then
    if itm then
      return \"t\"
    else
      return \"nil\"
    end if
  else if class of itm is list then
    set str to \"\"
    repeat with i in itm
      set str to str & elispify(i) & \" \"
    end repeat
    return \"(\" & str & \")\"
  else
    return \"\\\"\" & (itm as string) & \"\\\"\"
  end if
end elispify
")

;;; Ways to invoke osascript (file, string & buffer)

(defun osascript-run-file (filename)
  "Run the osascript named by FILENAME."
  (interactive
   (list (expand-file-name (read-file-name "File: " buffer-file-name))))
  ;;(message "(osascript-run-file \"%s\")" filename)
  (let ((outbuf
         (get-buffer-create (concat "*" osascript-program " " filename "*")))
        (outstr nil)
        rv)
    ;; Maybe use "(shell-command-to-string)"?
    (condition-case err
        (setq rv (call-process osascript-program nil outbuf nil
                               osascript-args filename))
      (error (message "%s" err) nil))
    (if osascript-keep-output
      (save-excursion
        (set-buffer outbuf)
        (setq outstr (buffer-substring-no-properties
                      (point-min) (point-max)))))
    (unless osascript-debug
      (kill-buffer outbuf))
    (if (equal rv 0) outstr nil)))

(defun osascript-insert-strings (itm)
  "Insert the strings found in ITM, a tree of strings."
  (cond
   ((stringp itm)
    (insert itm))
   ((listp itm)
    (mapcar 'osascript-insert-strings itm))
   (t (error "Should be a string or a list of strings"))))

(defun osascript-run-str (str)
  "Run the osascript STR."
  (let ((osa-tmp-file (osascript-temp-file-name)))
    (with-temp-file osa-tmp-file
      ;; put in library functions
      ;;(message "%s" osascript-run-library)
      (mapcar 'insert osascript-run-library)
      ;; our quoted applescript text
      (osascript-insert-strings str))
    (prog1
        (osascript-run-file osa-tmp-file)
      (unless osascript-debug
        (delete-file osa-tmp-file) ))))

(defun osascript-run-str-elispify (str)
  "Run the osascript STR and include the elispify function.
To get the return value elispifed, you should end your
applescript with elispify(retval)."
  (let ((osascript-run-library (list osascript-on-elispify))
        (osascript-args "-sh")
        outstr)
    (setq outstr (osascript-run-str str))
    (condition-case err
        (car (read-from-string outstr))
      (error nil))))
;; (osascript-run-str-elispify "elispify(1+3)")
;; (osascript-run-str-elispify "1+3")

(defun osascript-run-buffer ()
  "Run the current buffer through osascript."
  (interactive)
  (osascript-run-str (buffer-substring-no-properties (point-min) (point-max)))
  nil)

;;;;;;;;;; Finder examples

(defun osx-finder-do (&rest pgm)
  "Tell Finder to run the program PGM."
  (osascript-run-str `("
tell application \"Finder\"
  activate
  " ,@pgm "
end tell")))

;; (osx-finder-do "beep")
;; (osx-finder-do "beep\nset position of finder window 1 to {100, 100}")

(defun osx-safari-do (&rest pgm)
  "Tell Finder to run the program PGM."
  (osascript-run-str `("
tell application \"Safari\"
  activate
  " ,@pgm "
end tell")))

(defun osx-safari-url (url)
  "Tell Safari to open the URL."
  (osx-safari-do
   "if not (exists document 1) then\n"
   "   make new document at the beginning of documents\n"
   "end if\n"
   (format "set the url of the front document to \"%s\"" url)))

;; (osx-safari-url "http://www.mahalito.net/")

(provide 'osx-osascript)

;;; osx-osascript.el ends here
