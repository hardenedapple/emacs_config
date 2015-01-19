;;;; This file is here so I can have a nice REPL quickly load.
;;;  i.e. this is a file to load to start a lisp REPL for a quick session, when
;;;  I don't need to wait the 10 seconds or so for my entire emacs to load.

(load "~/.emacs.d/without_plugins.el")
(setq load-path (cons "~/.emacs.d/packages/slime" load-path))
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy slime-asdf slime-banner))
(slime)
(delete-other-windows)
