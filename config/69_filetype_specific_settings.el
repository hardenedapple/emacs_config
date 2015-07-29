;;;; C Settings
;;;;
(setq c-default-style "linux"
      c-basic-offset 4)
(c-set-offset 'case-label '+)


;;;; Comint Mode
;;;;
(add-hook 'comint-mode-hook
          (lambda ()
            (auto-fill-mode -1)))


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

(with-eval-after-load 'tex-mode
    (define-key latex-mode-map "1" (insert-this-char ?!))
    (define-key latex-mode-map "!" (insert-this-char 49))
    (define-key latex-mode-map "2" (insert-this-char ?@))
    (define-key latex-mode-map "@" (insert-this-char 50))
    (define-key latex-mode-map "3" (insert-this-char ?#))
    (define-key latex-mode-map "#" (insert-this-char 51))
    (define-key latex-mode-map "4" (insert-this-char ?$))
    (define-key latex-mode-map "$" (insert-this-char 52))
    (define-key latex-mode-map "5" (insert-this-char ?%))
    (define-key latex-mode-map "%" (insert-this-char 53))
    (define-key latex-mode-map "6" (insert-this-char ?^))
    (define-key latex-mode-map "^" (insert-this-char 54))
    (define-key latex-mode-map "7" (insert-this-char ?&))
    (define-key latex-mode-map "&" (insert-this-char 55))
    (define-key latex-mode-map "8" (insert-this-char ?*))
    (define-key latex-mode-map "*" (insert-this-char 56))
    (define-key latex-mode-map "9" (insert-this-char ?\())
    (define-key latex-mode-map "(" (insert-this-char 57))
    (define-key latex-mode-map "0" (insert-this-char ?\)))
    (define-key latex-mode-map ")" (insert-this-char 48))
    (define-key latex-mode-map "{" (insert-this-char ?\[))
    (define-key latex-mode-map "[" (insert-this-char ?\{))
    (define-key latex-mode-map "}" (insert-this-char ?\]))
    (define-key latex-mode-map "]" (insert-this-char ?\})))


;;;; Lisp Settings
;;;;
(defun upsexp-newline-and-parentheses (&optional arg)
  "Move outside the current sexp, start a newline, leave `point'
inside a new sexp.

Argument determines how many lists to move down before starting
the new line.

Taken from \"speed-of-thought-lisp\"
https://github.com/Malabarba/speed-of-thought-lisp"
  (interactive "^p")
  (up-sexp arg)
  (newline-and-indent)
  (insert "()")
  (forward-char -1))

(define-key lisp-mode-shared-map (kbd "<C-return>") 'upsexp-newline-and-parentheses)
(define-key lisp-mode-shared-map (kbd "M-r") 'kill-backward-up-list)
(define-key lisp-mode-shared-map (kbd "M-s M-s") 'delete-pair)
;; Override the prog-mode remappings between - and _ in lisp mode
(define-key lisp-mode-shared-map "-" 'self-insert-command)
(define-key lisp-mode-shared-map "_" 'self-insert-command)

;; Elisp specifically
(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map [mouse-3]
    (mouse-function-on-symbol (help-xref-interned (intern current-symbol))
                              (pop-tag-mark))))

(setq inferior-lisp-program "/usr/bin/sbcl")


;;;; Man Settings
;;;;
(setq Man-notify-method 'pushy)


;;;; Python Settings
;;;;
(defun python-shell-restart-inferior-python (&optional buffer)
  "Restarts the python shell currently running.

If `buffer' is given, restarts the process in that buffer, if `arg' is non-null
stops the current python process using `delete-process' rather than
`process-send-eof'"
  (interactive)
  ;; Be on the lookout for having to do something different with dedicated and
  ;; non-dedicated processes.
  (let* ((python-buffer (or (python-shell-get-buffer) buffer))
         (python-process (get-buffer-process python-buffer)))
    ;; If there's currently no process, then just start one.
    ;; Otherwise, call `delete-process' on it and start a new python shell with
    ;; the same arguments as the current one.
    (if (not python-process)
        (python-shell-get-or-create-process)
      (delete-process python-process)
      (python-shell-get-or-create-process
       (mapconcat 'identity (process-command python-process) " ")))))

;;; Python doesn't use semicolons very much, so make them all colons
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map ";" (insert-this-char ?:))
            (define-key python-mode-map ":" (insert-this-char ?\;))))

;;;; Scheme Settings
;;;;
;; I'm using guile at the moment
(setq scheme-program-name "guile")
