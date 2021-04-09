# Flycheck

<https://www.flycheck.org/en/latest/user/syntax-checks.html>

Todo list:

- [x] `global-flycheck-mode` or just `prog-mode-hook` and `text-mode-hook`?  Try former?
  - [x] `(add-hook 'after-init-hook #'global-flycheck-mode)`
- [ ] Basic customizations
  - [ ] Keybindings
  - [ ] Learn to use it!
	- [ ] <https://www.flycheck.org/en/latest/user/error-list.html>
	- [ ] <https://www.flycheck.org/en/latest/user/error-reports.html>
	- [ ] <https://www.flycheck.org/en/latest/user/syntax-checkers.html>
	- [ ] Maybe take over flymake commands?
  - [x] Customize-group, etc.
  - [ ] Faces?  Copy flymake, or start over fresh, using whatever the theme picks up?  Split the difference?
- [ ] Language-specific stuff <https://www.flycheck.org/en/latest/languages.html>
  - For each, check:
	- Correct program found and working; Maybe: <https://www.flycheck.org/en/latest/user/syntax-checkers.html>
	- Turns on and runs automatically
	- Modeline works
	- Not too slow
	- Other customizations
  - [ ] Language list:
	- [ ] css: <https://www.flycheck.org/en/latest/languages.html#css>
	- [x] elisp: <https://www.flycheck.org/en/latest/languages.html#emacs-lisp>
	- [ ] html: <https://www.flycheck.org/en/latest/languages.html#html>
	- [ ] js: <https://www.flycheck.org/en/latest/languages.html#javascript>
	  - Possibly annoyingly slow?  That's likely just `js2-mode` though
	- [x] json: <https://www.flycheck.org/en/latest/languages.html#json>
	- [x] markdown: <https://www.flycheck.org/en/latest/languages.html#markdown>
	- [x] perl: <https://www.flycheck.org/en/latest/languages.html#perl>
	  - [x] perlcritic
	  - [ ] perltidy as well? (perltidyrc) Does that even make sense?
	- [ ] php: <https://www.flycheck.org/en/latest/languages.html#php>
	- [ ] python: <https://www.flycheck.org/en/latest/languages.html#python>
	- [ ] r: <https://www.flycheck.org/en/latest/languages.html#r>
	- [ ] ruby: <https://www.flycheck.org/en/latest/languages.html#ruby>
	- [ ] bash: <https://www.flycheck.org/en/latest/languages.html#shell-scripting-languages>
	- [ ] text: <https://www.flycheck.org/en/latest/languages.html#text>
	- [ ] ts: <https://www.flycheck.org/en/latest/languages.html#typescript>
	- [ ] yaml: <https://www.flycheck.org/en/latest/languages.html#yaml>
- [ ] Recommended extensions: <https://www.flycheck.org/en/latest/community/extensions.html>
  - [ ] <https://github.com/flycheck/flycheck-pos-tip>
  - [ ] <https://github.com/flycheck/flycheck-inline>
  - [ ] <https://github.com/cuonglm/flycheck-checkbashisms> (<https://salsa.debian.org/debian/devscripts/-/blob/master/scripts/checkbashisms.pl>)
- [ ] Other extensions/packages:
  - [ ] flycheck-aspell
  - [ ] flycheck-color-mode-line (also above)
  - [ ] flycheck-elsa / elsa
  - [ ] flycheck-grammarly
  - [ ] flycheck-relint
  - [ ] flycheck-title (also above)
  - [ ] magit-diff-flycheck
