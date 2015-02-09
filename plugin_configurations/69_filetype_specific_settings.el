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

;;;; Help mode settings
;;;;
(defun help-follow-source-link ()
  "Follow \"defined in ...\" link at top of *Help* buffer.

This function is really dumb about doing this -- it goes to first
window with the *Help* buffer showing, moves to the first link in
that buffer, and follows it."
  (interactive)
  (when (string-match "^\*Help\.*"  (buffer-name (current-buffer)))
      (let ((first-button (button-at (next-button 1))))
        (when (not (string-match "C source code" (button-label first-button)))
          ;; Act fully as if the user pressed the button themselves
          ;; (here as I look for the value of THIS-COMMAND in my custom
          ;; DISPLAY-BUFFER functions, because of precisely these commands)
          (let ((this-command 'push-button))
            (button-activate first-button))))))

(define-key help-mode-map (kbd "M-g M-f") 'help-follow-source-link)


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
