;;; C Settings
;;;
(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'case-label '+)

;;; Latex Settings
;;;
(add-hook 'latex-mode-hook
          (lambda ()
            (set-fill-column 125)))  ; usually only write latex on large screens


;;; Lisp Settings
;;;
(setq inferior-lisp-program "/usr/bin/sbcl")


;;; Script Settings
;;;
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
