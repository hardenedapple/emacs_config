;;;; C Settings
;;;;
(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'case-label '+)


;;;; Elisp Settings
;;;;
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map [mouse-3]
              (mouse-function-on-symbol (help-xref-interned (intern current-symbol))
                                        (pop-tag-mark)))))


;;;; Latex Settings
;;;;
(setq latex-run-command "pdflatex")
(add-hook 'latex-mode-hook
          (lambda ()
            (set-fill-column 125)))  ; usually only write latex on large screens


;;;; Lisp Settings
;;;;
(setq inferior-lisp-program "/usr/bin/sbcl")


;;;; Scheme Settings
;; I'm using guile at the moment
(setq scheme-program-name "guile")
