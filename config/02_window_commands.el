;;;; Window history buffer switch
;;;;
(defvar buffer-choose-default-function 'switch-to-buffer
  "Function to call with unprefixed C-x b")

(defun window-history-buffer-choose (&optional prefix)
  "Select a buffer from the history of `current-window'."
  (interactive "P")
  (if prefix
      (let ((buffer-names
             (mapcar (lambda (list-thing) (buffer-name (car list-thing)))
                     (append (window-prev-buffers) (window-next-buffers)))))
        (if buffer-names
            (switch-to-buffer
             (completing-read "Buffer previous " buffer-names
                              nil t nil nil (car buffer-names) nil))
          (call-interactively buffer-choose-default-function)))
    (call-interactively buffer-choose-default-function)))

(define-key ctl-x-map "b" 'window-history-buffer-choose)


;;;; Window Layout
;;;;
(define-key ctl-x-map "+" 'what-cursor-position)
(define-key ctl-x-map "=" 'balance-windows)

(defun fix-window-horizontal-size (&optional num-columns)
  "Set `current-window' size to 90 NUM-COLUMNS columns wide."
  (interactive "P")
  (enlarge-window (- (or num-columns 90) (window-width)) 'horizontal))

(define-key ctl-x-4-map "w" 'fix-window-horizontal-size)
(define-key ctl-x-4-map "g" 'delete-other-windows-vertically)

;;; DISPLAY-BUFFER settings
(defun display-buffer-some/pop-window (buffer alist)
  "If `one-window-p' display in new window.

Otherwise try `display-buffer-use-some-window'."
  (if (one-window-p)
      (display-buffer-pop-up-window buffer alist)
    ;; Recently switched from LRU to MRU.
    ;; Am liking this at the moment.  Will see whether I like it going forward.
    ;; Am not perfectly happy with the mechanism by which I'm doing this, but
    ;; it does look like this is somewhat the "standard" way to handle
    ;; association lists.
    (let ((alist (cons '(some-window . mru) alist)))
      (display-buffer-use-some-window buffer alist))))

(defvar display-buffer-here-commands
  (list 'previous-error 'next-error 'first-error 'push-button
  'occur-mode-goto-occurrence 'goto-error-on-linenum)
  "Commands to use same window when calling `pop-to-buffer'")

(defun display-buffer-same-window-from-command (buffer alist)
  "Open BUFFER in `current-window' if `this-command' is in
`display-buffer-here-commands'"
  (when (memq this-command display-buffer-here-commands)
    (display-buffer-same-window buffer alist)))

(defun display-buffer-previous-other-window (buffer alist)
  "Call `display-buffer-in-previous-window' with
`inhibit-same-window' set so never open in `current-window'."
  (let ((alist (cons '(inhibit-same-window . t) alist)))
    (display-buffer-in-previous-window buffer alist)))

(setq window-combination-resize t
      display-buffer-base-action (list (list
                                        'display-buffer-reuse-window
                                        'display-buffer-same-window-from-command
                                        'display-buffer-previous-other-window
                                        'display-buffer-some/pop-window)))

;;; Make it more likely that split-window-sensibly will split vertically
(setq fit-window-to-buffer-horizontally t
      split-height-threshold 27
      split-width-threshold 175      ; 2 * 80 columns of text + line numbers etc
      compilation-window-height 10)


;;; Run command in other window
;; For a similar function see my configuration for smart-window
(defun run-function-other-window (function &optional window &rest args)
  "Call FUNCTION with arguments ARGS in a different window.

Takes a function to run, and executes it with WINDOW selected.

If WINDOW is `nil', and `one-window-p' split the current window
and select the result, else select WINDOW or `other-window'.

Then `apply' ARGS to FUNCTION."
  (if (window-live-p window)
      (select-window window)
    (if (one-window-p)
        (select-window (split-window-sensibly))
      (other-window 1)))
  (apply function args))

(defun run-command-other-window (command &optional window)
  (interactive "C")
  (run-function-other-window #'call-interactively window command))

(define-key ctl-x-4-map (kbd "M-x") 'run-command-other-window)

;;;; Swap Windows with window number X
;;;;
(defun swap-window-with-number (number)
  "Swap window with window number NUMBER.

Window number assigned in the same way as the \"C-x p %d\" command."
  (interactive "P")
  (if (integerp number)
      (let ((window (nth (1- number) (window-number-list))))
        (if (and window (or (not (window-minibuffer-p window))
                            (minibuffer-window-active-p window)))
            (swap-windows-properly window)
          (error "No such window.")))))

;;;; Tab bar handling
;;;;
;;; Only show tab bar while we have more than one tab.
(setq tab-bar-show 1)
;;; Map `<prefix-num> Ctrl-x t g' to specific tab choice.  Maps somewhat similar
;;; to the tab choice mapping in vim which is `<count>gt' so I hope that will
;;; make it easier to remember.
(define-key tab-prefix-map (kbd "g") 'tab-select)
