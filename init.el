;;; In order to have ecukes tests for my entire emacs configuration, I have to
;;; follow some file naming conventions that ecukes expects.
;;;
;;; One of these is having a package and the entry file of that package the same
;;; name as the directory it's stored under.
;;; Hence, I need to have the main emacs initialisation code under a file named
;;; .emacs.d.el.
;;; init.el is just a stub that loads that config file so that emacs can find it
;;; upon load.
(load "~/.emacs.d/.emacs.d.el")
