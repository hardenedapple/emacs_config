(require 'f)

(defvar .emacs.d-support-path
  (f-dirname load-file-name))

(defvar .emacs.d-features-path
  (f-parent .emacs.d-support-path))

(defvar .emacs.d-root-path
  (f-parent .emacs.d-features-path))

(add-to-list 'load-path .emacs.d-root-path)

(require '.emacs.d)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
