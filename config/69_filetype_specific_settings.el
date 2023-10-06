;;;; C Settings
;;;;

;; ;; The below is what I prefer, but I'm working on GNU at the moment so
;; ;; use default.
;; (setq c-default-style "linux"
;;       c-basic-offset 4)
;; (c-set-offset 'case-label '+)

(with-eval-after-load 'cc-cmds
  (defadvice c-indent-new-comment-line (around only-backslash-defines activate)
    "Advice that ensures backslash continuations for C preprocessors are not added
to #ifdef or #include lines.

This advice is slightly hacky but it doesn't look like there's a better way."
    ;; We have to remove the current cache, otherwise it would cause the
    ;; backslash to be inserted on a freshly typed #include anyway.
    (let ((c-opt-cpp-start c-opt-cpp-macro-define-start))
      (setq c-macro-cache nil
            c-macro-cache-start-pos nil
            c-macro-cache-syntactic nil)
      ad-do-it)))


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

(with-eval-after-load 'help-mode
    (define-key help-mode-map (kbd "M-g M-f") 'help-follow-source-link))

;;;; Latex Settings
;;;;
(setq latex-run-command "pdflatex")


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


(with-eval-after-load 'lisp-mode
  (define-key emacs-lisp-mode-map mouse-get-help-key
    (mouse-function-on-symbol (help-xref-interned (intern current-symbol))))
  (define-key emacs-lisp-mode-map (kbd "C-M-q") nil)
  (define-key lisp-interaction-mode-map (kbd "C-M-q") nil)
  ;; Really don't know why emacs makes this a local key binding when there's
  ;; already a key-binding for it in the global map.
  (define-key emacs-lisp-mode-map (kbd "M-C-i") nil)
  (define-key lisp-interaction-mode-map (kbd "M-C-i") nil))

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

;; Things to remember and/or fix in the future:
;;
;;    Fancy prompt not handled by emacs
;;       Use
;;       c.TerminalInteractiveShell.simple_prompt = True
;;       in the ipython configuration file
;;
;;    Completion hangs
;;       First off, python.el sends the wrong string to ipython if there's an
;;       open bracket in the command.
;;          e.g.  >>>  get_ipython().Completer.all_
;;          results in  .Completer.all_  getting sent to the underlying
;;          process.
;;
;;          Though sending the entire string to IPython actually works, simply
;;          pressing <TAB> in the terminal IPython prompt doesn't.
;;          I guess that means there may be some reason for it.
;;       python-shell-send-string-no-output hangs
;;          This was because I had ran a non-simple prompt before changing the
;;          ipython config to run a simple_prompt.
;;          This had set the `ansi-color-context' to something that meant I
;;          always got the empty string back.
;;          Killing the buffer and creating a new one worked.
;;

;;;; Scheme Settings
;;;;
;; I'm using guile at the moment
(setq scheme-program-name "guile")
