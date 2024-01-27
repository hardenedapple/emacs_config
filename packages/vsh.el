;;; vsh.el --- specialized and modified comint.el for shell -*- lexical-binding: t -*-

;; Copyright (C) 2024 Matthew Malcomson

;; Author: Matthew Malcomson <hardenedapple@gmail.com>
;; Keywords: processes

;;; Commentary:

;;; Customization and Buffer Variables

;; Variables

(defgroup vsh nil
  "Running vsh shell from within Emacs buffers."
  :group 'processes
  :group 'unix)

(defcustom vsh-default-prompt "vshcmd: > "
  "Pattern that indicates a prompt in a VSH buffer."
  :type 'string
  :group 'vsh)

(defcustom vsh-mode-hook
  '(vsh--initialise-settings vsh--setup-colors vsh-start-process)
  "Hook for customizing vsh mode."
  :type 'hook
  :group 'vsh)

(defcustom vsh-ignore-ansi-colors nil
  "Define whether to ignore ansi color sequences coming from underlying terminal
or to use them to specify text colors.

`nil' implies that SGR control sequences are filtered, anything else means that
SGR control sequences are interpreted as determining color and *not* filtered."
  :type 'boolean
  :group 'vsh)

;; Emacs has an in-built server that can be started with `server-start' or starting
;; emacs as a daemon (see `daemonp').  This is sufficient for most interaction
;; that underlying helpers in VSH would want to perform.
;;
;; This may or may not have been started at the time that a VSH buffer is opened,
;; or indeed at the later time that an underlying process wants to talk to the
;; emacs process that is running the buffer.
;;
;; In order to account for that we *could* create our own process that does much
;; the same thing.  We could then turn that on/off independently of the main
;; server.  The problem with that is that it feels like this would be less
;; visible to the user.  This isn't much of a problem.
;; It means a bunch of duplicated code though -- and it just feels a little
;; silly for the sole purpose of having a separate emacs server process.
;;
;;
;;
;; There are two approaches that we implement:
;; 1) When starting a VSH process for the first time, start a special VSH server
;;    for all vsh helpers to communicate with.  This server is started when needed,
;;    and is less likely to be stopped without knowing that it disables VSH
;;    functionality.
;; 2) Attempt to use any existing server.  This means there is less likely to be
;;    server processes that the user is unaware of.  This server may or may not be
;;    running when VSH starts.  It seems slightly more likely that this would get
;;    stopped in between starting a VSH underlying process and when some helper
;;    wants to communicate with the parent emacs.

;; Things to think about:
;; 1) Multiple frames, many showing the relevant vsh buffer.
;;    - Perform action in whichever is the active frame, otherwise the first
;;      frame found that is showing this VSH buffer.
;; 2) How to know which buffer we're talking from?  Does it matter?
;; 3) Need to tell subprocesses what the server name is (in case of multiple
;;    emacs servers running).
;; 4) Looks like file-permissions are what ensures we're only allowing the
;;    current user to control this emacs session.
;; 5) If using the standard server, then `emacsclient' has features like getting
;;    informed that the frame I'm working on has been suspected/deleted etc.
;;    There's also a the features of tracking which files were opened in this
;;    emacs process becase of an `emacsclient' call, and `emacsclient'
;;    automatically waits on the emacs server to respond (rather than the hack I
;;    have of having the VSH user say "successful edit" on the command line).
;; 6) The communication features I have are:
;;    - GDB:
;;      - Move point to a given position in a given file
;;        `emacsclient --no-wait +line:column filename'
;;      - Mark stack.
;;        `emacsclient --eval '(some-vsh-command-for-marking-stack (list positions ...))'
;;      - Show here.
;;        `emacsclient --no-wait ????'
;;      - Mark this.
;;        `emacsclient --eval '(some-other-vsh-command-for-marking-position position)'
;;    - EDITOR
;;      - `emacsclient filename ...'
;;    - readline bindings
;;      - `emacsclient --eval '(some-vsh-command-for-recording-bindings
;;                              buffer-indicator bindings)''

(defcustom vsh-may-start-server t
  "Hook to determine whether VSH attempts to ensure `server-start' has been run.

Some features depend on emacs `server-start' having been run in the emacs buffer
that VSH runs in.  This variable determines whether VSH attempts to start such a
server if one is not running.

If the server is not running and this variable is set to `nil`
those features are automatically disabled."
  :type 'boolean
  :group 'vsh)

(defvar vsh-new-output nil
  "Boolean indicating whether we are in the middle of an output or not.

This is not supposed to be a marker between logical subcommands of e.g. a bash
process, rather it is whether we recently manually moved the marker position
(e.g. because a command was just executed).")
(defvar vsh-completions-keys '((possible-completions . "?")
                               (glob-list-expansions . "g")
                               (unix-line-discard . ""))
  "Association list of control characters defining the key sequences
to send to readline processes in underlying terminal for
`possible-completions', `glob-list-expansions', and
`unix-line-discard' respectively.")
(defvar vsh-last-command-position nil
  "Marker giving line of last text sent to the underlying pseudo-terminal.")
(defvar vsh-buffer-initialised nil
  "Has this current buffers process been initialised.")

;; TODO
;;   - Add functionality to send to this buffer from some other buffer.
;;     - Including the python "send having de-dented this text" stuff.
;;   - Change server-name if there is already a server running?
;;     Unlikely to be a commonly hit thing, but certainly possible.
;;   - Add tests
;;   - Add something "joining" text together so that a single `undo' removes an
;;     entire output.
;;     - Something with `buffer-modified-tick'?
;;   - Error handling
;;     - Alert when attempting to interact with an underlying process and the
;;       underlying process has been terminated.
;;   - File search using directory of underlying process.
;;     - Use the `/proc' filesystem searching trick I used in vim.
;;     - Maybe introduce some sort of `hippie-complete-<special>' function that
;;       does this, as I don't think I want to override C-x C-f for vsh buffers.
;;   - Think about what buffer-local symbols need to be marked as
;;     `permanent-local' so they are not removed on changing major mode (see
;;     "(elisp) Creating Buffer-Local").
;;   - Fix `vsh-next-command' to move to the start of current command line if at
;;     a prompt.
;;   - Add more colours
;;     - Highlight strings on special lines only (not in output).
;;     - I wonder whether it's also useful to make the hook for our filter
;;       function hook so we can add actions to it as and when needed (see
;;       `comint-output-filter-functions').
;;   - Documentation
;;     - Document the functions and variables in this file.
;;     - Write adjustments for emacs in the demo VSH files in the VSH repo.
;;   - Determine repository layout (i.e. is this emacs file going in the
;;     originally vim repository).
;;   - Understand autoload things.
;;   - Allow buffer-local or user-specified prompt.
;;   - Maybe do something about the `repeat-mode' stuff.
;;   - Handle `case-fold-search' (ensure it doesn't change things)

;; N.b. have noticed that the below (i.e. a command line with a hash but that
;; does not have a space between the hash and the text on it) is not counted as
;; a comment proper and is not counted as a command proper either.
;; In vim there is very little problem with that -- seems very little uses the
;; comment marker for any behaviour.
;; Will keep an eye out for how I implement things in emacs to ensure nothing
;; too problematic comes out from it.  If there is something problematic then I
;; expect I'll define what should happen in both the vim and emacs versions.
;;
;; vshcmd: > #test

(defun vsh-prompt (&optional buffer)
  "Will eventually return the vsh prompt for the relevant buffer.
As yet I've not implemented different prompts for different buffers, so this is
essentially a literal."
  vsh-default-prompt)

;;; Defining the different line types:
(defun vsh--comment-marker (&optional buffer)
  "Regexp defining comment prefix."
  (string-join (list (vsh-prompt buffer) "# ")))

(defun vsh--command-marker (&optional buffer)
  "Regexp defining command prefix."
  (rx (group-n 1 (literal (vsh-prompt buffer))
               (zero-or-more blank))
      ;; N.b. Using `blank' rather than `whitespace' because the syntax classes
      ;; are user controlled while the `blank' character class is dependent on
      ;; unicode properties.  Now `blank' does not match newline, so we ensure
      ;; that the newline is also directly mentioned in our regexp.
      (group-n 2 (or eol (not (any ?# blank ?\n))))))

(defun vsh-split-regexp (&optional buffer)
  "Regexp defining lines which are not classed as output (and are hence a
\"split\" of output).

These are all lines which start with the `vsh-prompt' with any trailing
whitespace stripped."
  (string-join (list "^" (replace-regexp-in-string "\\s-+$" "" (vsh-prompt buffer))))
  ;; The reason for defining things with whitespcae stripped is that in *vim*
  ;; (where this file format was defined) there was an unfortunate interaction
  ;; between automatic removal of trailing whitespace and that turning some
  ;; "empty command lines" into "output" lines.
  ;; This could happen in emacs too -- but even if we didn't have the same
  ;; problem, we'd want to keep the format of a file meaning the same thing
  ;; between the two text editors.
  ;; (string-join (list "^" (vsh-prompt buffer)))
  )

(defun vsh-command-regexp (&optional buffer)
  "Rx regexp defining lines which are commands."
  (rx bol (regexp (vsh--command-marker buffer))))

(defun vsh-comment-regexp (&optional buffer)
  "Regexp defining lines which are comments"
  (rx bol (group-n 1 (regexp (vsh--comment-marker buffer)))))

(defun vsh-motion-marker (&optional buffer)
  "Regexp defining what we move to with up/down motions.

This is slightly different to split marker and the comment markers.  The reason
for this is just observed use.

All lines starting with the base prompt text are \"split\" markers.
Lines starting with the base prompt followed by a hash are \"comments\".
Lines starting with the base prompt and *not* followed by a hash are
\"commands\".
Comment lines followed directly by a line that would be a command if it began
at the start of a line are \"saved commands\".

This function returns a regexp that matches either a \"command\" or a \"saved
command\"."
  (rx bol
      (zero-or-one (regexp (vsh--comment-marker buffer)))
      (regexp (vsh--command-marker buffer))))

(defun vsh--current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun vsh-bol ()
  "Move to beginning of command line or comment if this line is not output."
  (interactive)
  (if (cl-find-if (lambda (fn)
                    (string-match (funcall fn) (vsh--current-line)))
                  (list #'vsh-motion-marker #'vsh-comment-regexp))
      (goto-char (+ (line-beginning-position) (match-end 1)))
    (beginning-of-line)))

(defun vsh-next-command (&optional count)
  "Move to the next vsh prompt."
  ;; TODO ??? Would be nice to handle that "next command" with cursor at the
  ;; very start of a command (e.g. on the `s' of `vshcmd') takes us to the start
  ;; of *this* command.  On the other hand, it's not too important, and seems a
  ;; little tricky to implement.  I don't think I chose that in vim on purpose.
  (interactive "p")
  ;; Move to the next vsh prompt (as defined by `vsh-motion-marker').
  ;; If there is no next prompt then move to the end/start of the buffer
  ;; (depending on direction).
  ;; If the first/last line of the buffer is a prompt and we have moved there,
  ;; then get to the start of a prompt.
  (if (re-search-forward (vsh-motion-marker) nil 'to-end-on-error count)
      ;; We have moved our point, but because there is no lookahead regexp in elisp
      ;; we may have moved it one character further than we wanted to (the character
      ;; we checked to ensure it was not a hash indicating a comment).
      ;; Hence just go directly to the relevant character we need.
      (goto-char (match-end 1))
    (if (string-match (vsh-motion-marker) (vsh--current-line))
        (goto-char (+ (line-beginning-position) (match-end 1))))))

(defun vsh-prev-command (&optional count)
  "Move to the previous vsh prompt."
  (interactive "p")
  (vsh-next-command (- count)))

(defun vsh--segment-bound (&optional forwards inc-marker)
  "Find start or end of the current segment.

Segment is defined as a command plus the entire output directly under it."
  (save-excursion
    (forward-line (if forwards 1 0))
    (let ((have-seen-match (looking-at-p (vsh-split-regexp))))
      (unless have-seen-match
        (setq have-seen-match
              (re-search-forward (vsh-split-regexp) nil 'eof-if-not-found
                                 (if forwards 1 -1)))
        (when (and have-seen-match forwards)
          (beginning-of-line)))
      (when (and (not forwards) have-seen-match (not inc-marker))
        (forward-line 1)))
    (point)))

(defun vsh-mark-segment (inc-marker)
  "Mark the current segment."
  (interactive "P")
  (set-mark (vsh--segment-bound nil inc-marker))
  (goto-char (vsh--segment-bound t inc-marker))
  (activate-mark))

(defun vsh--move-to-end-of-block (regex forwards)
  (beginning-of-line)
  (let ((not-moved 1))
    (while (and (looking-at-p regex)
                (= 0 (setq not-moved (forward-line (if forwards 1 -1))))))
    ;; If failed to move backwards, that means the first line of this file
    ;; is a command and we are at the start of the buffer.
    ;; We will not fail to move forwards, because `forward-line' will leave
    ;; us at the end of the buffer before this happens.  When the last line
    ;; in a buffer is a command we will end up at the very end of the buffer
    ;; at the end of the current line.  Check for this in order to include
    ;; the entire last line.
    (when (and (= not-moved 0) (/= (point) (point-max)))
      (forward-line (if forwards 0 1)))
    (point)))

;; Go upwards until find a split marker.
;;   - If no split marker found, return current point.
;; Once found split marker.
;;   - If looking for end of command block, go down until find *not* a split
;;     marker, return that point.
;;   - If looking for start of command block, go up until find *not* a split
;;     marker, return that point.
(defun vsh--command-block-bounds (inc-comments &optional forwards)
  "Find start or end of current command block.

Command block is defined as either a sequence of lines all starting with
`vsh-split-regexp', or a sequence of lines all starting with
`vsh-command-regexp' depending on the value of the provided `inc-comments'
argument."
  (save-excursion
    (beginning-of-line)
    (let ((regex (if inc-comments (vsh-split-regexp) (vsh-command-regexp))))
      (if (and (not (looking-at-p regex))
               (not (re-search-backward regex nil t)))
          (point)
        (vsh--move-to-end-of-block regex forwards)))))

(defun vsh-mark-command-block (inc-comments)
  "Mark the entire command block around the current position."
  (interactive "P")
  (push-mark (vsh--command-block-bounds inc-comments nil))
  (goto-char (vsh--command-block-bounds inc-comments t))
  (when (/= (point) (mark t)) (activate-mark)))

(defun vsh--beginning-of-block-fn (&optional count)
  (let ((count (or count 1)))
    (if (> count 0)
        (progn
          (beginning-of-line)
          (dotimes (loop-counter count)
            (re-search-backward (vsh-split-regexp) nil 'move-to-end)
            (vsh--move-to-end-of-block (vsh-split-regexp) nil)))
      (dotimes (loop-counter (abs count))
        (vsh--move-to-end-of-block (vsh-split-regexp) t)
        (re-search-forward (vsh-split-regexp) nil 'move-to-end))))
  (vsh-bol))
(defun vsh-beginning-of-block (count)
  (interactive "p")
  (vsh--beginning-of-block-fn count))

(defun vsh--end-of-block-fn (&optional count interactive)
  (let ((count (or count 1)))
    (if (> count 0)
        (progn
          (dotimes (loop-counter count)
            (re-search-forward (vsh-split-regexp) nil 'move-to-end)
            (vsh--move-to-end-of-block (vsh-split-regexp) t))
          (backward-char))
      (dotimes (loop-counter (abs count))
        (vsh--move-to-end-of-block (vsh-split-regexp) nil)
        (re-search-backward (vsh-split-regexp) nil 'move-to-end)))))
(defun vsh-end-of-block (count)
  (interactive "p")
  (vsh--end-of-block-fn count)
  (vsh-bol))

(defun vsh-make-cmd (rbeg rend)
  (interactive "r")
  (let ((rbeg (save-excursion (goto-char rbeg) (line-beginning-position))))
    (replace-regexp-in-region "^" (vsh-prompt) rbeg rend)))

(defun vsh-save-or-activate-command (save)
  (save-excursion
    (end-of-line)
    (re-search-backward (vsh-split-regexp) nil 'eof-on-missing)
    (beginning-of-line)
    (cond
     (save (if (looking-at-p (vsh-command-regexp))
               (insert (vsh--comment-marker))
             (message "Output is not Active")))
     (t (if (looking-at (rx (regexp (vsh-comment-regexp)) (literal (vsh-prompt))))
            (delete-region (match-beginning 1) (match-end 1))
          (message "Output is not currently Saved"))))))
(defun vsh-save-command ()
  (interactive)
  (vsh-save-or-activate-command t))
(defun vsh-activate-command ()
  (interactive)
  (vsh-save-or-activate-command nil))

(defun vsh-new-prompt ()
  (interactive)
  (goto-char (vsh--segment-bound t nil))
  (insert (vsh-prompt) "\n")
  (backward-char))

;; Implementing start of block *backwards*:
;; 1) Get to any block using re-search-backward.
;;    - If we are outside of a block this gets us to some block.
;;    - If we are in the middle of a large block this stays inside the block.
;;    - If we are at the start of a block this moves us to the block above.
;;      (which is what we want in this case).
;; 2) Move to the very start of this block with `vsh--move-to-end-of-block'.

;; Implementation of end of block *forwards*.
;; 1) Get to any block using re-search-forward.
;;    - If we are outside of a block this gets us to some block.
;;    - If we are in the middle of a large block this stays inside the block.
;;    - If we are at the end of a block this moves us to the block below.
;; 2) Move to the end of the current block with `vsh--command-block-bounds'.

;; Implementation start of block *forwards*.
;; 1) Get to end of current block.
;;    - If we are outside of a block do not move.
;;    - If in the middle of a block this gets us to the end of the block above.
;;    - If at the end of a block keeps us where we are.
;; 2) Move to the start of the next block with `re-search-forward'.

;; Similar for end of block *backwards*.
;; 1) Get to start of current block.
;;    - If outside of a block do not move.
;;    - If in the middle of a block gets us to the start of the current block.
;;    - If at the very start of a bolck keep us where we are.
;; 2) Move to the end of the previous block with `re-search-backward'.

(defun vsh-insert-point (&optional buffer)
  "Marker giving insert point for any text that comes from the pseudo-terminal."
  (process-mark (get-buffer-process (or buffer (current-buffer)))))
;; It seems the emacs marker is a little different to the vim version.  For this
;; purpose the particular behaviour difference I'm interested in is that when a
;; marker is deleted (i.e. because the surrounding text is deleted) vim removes
;; that marker while emacs simply leaves the marker in between the start and end
;; of the deletion event (essentially where that marker was).  This essentially
;; means that `process-mark' is not going to get lost, which means the fallback
;; mechanisms seem unnecessary.

(defun vsh--process-filter (proc output)
  (with-current-buffer (process-buffer proc)
    ;; Horrible hack to avoid the very first prompt given by the underlying
    ;; process on startup.
    (when (or vsh-buffer-initialised (seq-find (lambda (x) (= x ?\n)) output))
      (save-excursion
        (let* ((insert-point (or (vsh-insert-point) (point-max)))
               (start-point (copy-marker insert-point nil)))
          (goto-char insert-point)
          ;; When the underlying process outputs only part of a line I want to
          ;; append the next bit of output to the end of that line.  However, when
          ;; I have just ran `vsh-execute-command' I want the output to be added
          ;; just below the current prompt.
          ;; Rather than always add a newline into the buffer when I run
          ;; `vsh-execute-command' I append a newline to the start of the first
          ;; output ran after such a command.  This allows running multiple lines
          ;; with `vsh-execute-command' (or similar) before any output comes and
          ;; not having blank lines between the lines that were run.
          (when vsh-new-output (insert-char ?\n) (setq-local vsh-new-output nil))
          (insert output)
          (if vsh-ignore-ansi-colors
              (ansi-color-filter-region start-point insert-point)
            (ansi-color-apply-on-region start-point insert-point t))
          ;; Leave temporary marker for GC.
          (set-marker start-point nil))))
    (unless vsh-buffer-initialised (setq-local vsh-buffer-initialised t))))

;; To think about:
;; I don't think I need to call `accept-process-output'?
;; My expectation is that it would be better to wait until emacs is waiting on
;; the user to be handling output.

;; I don't think I need to call `accept-process-output' since I usually do very
;; little and return to the user-input loop.  In fact *not* calling it is useful
;; to ensure that when I run `vsh-execute-region' I send the command from every
;; line before reading anything back (hence leaving all commands in the region
;; next to each other and all output from them underneath).

(defun vsh--server-advertise-env ()
  (string-join (list (if server-use-tcp "EMACS_SERVER_FILE" "EMACS_SOCKET_NAME")
                     "=" (server--file-name))))

;; Decided to make this interactive so can run with `M-x'.  Mainly for when
;; I've done something silly in the underlying process and want to fix it.
(defun vsh-start-process ()
  (interactive)
  (let ((proc (vsh--get-process)))
    (when proc (delete-process proc)))
  ;; `default-directory' should automatically be determined by the buffer
  ;; directory (which is what we want).
  ;; `process-environment' just needs to include that the terminal is dumb and
  ;; some mechanism via which to communicate back to emacs.
  (let* ((process-environment
          (list "TERM=dumb" (vsh--server-advertise-env)
                (string-join (list "VSH_EMACS_BUFFER="
                                   (prin1-to-string (sxhash-eq (current-buffer)))))))
         (proc (make-process
                :name "vsh-proc"
                :buffer (current-buffer)
                ;; TODO Going to have to ship a copy of this shell script.
                ;; Maybe a shared subrepo in order to ensure consistency between
                ;; the vim and emacs versions.
                ;; Alternatively I wonder whether I could just include the
                ;; vsh.el file in the same repo as the vim plugin.  That way the
                ;; shell scripts etc are all the same.
                :command (list "/home/matmal01/.vim/bundle/vsh/autoload/vsh/vsh_shell_start"
                               "/home/matmal01/.vim/bundle/vsh/autoload/vsh" "bash")
                :connection-type 'pty
                :noquery t
                :filter 'vsh--process-filter
                ;;  Default sentinel seems good enough (because I'm using
                ;;  `process-mark' of the underlying process as my mark for
                ;;  where to add text).  It doesn't have the check for adding a
                ;;  newline to the start of output if we are inserting a
                ;;  new-output, but since this is a rare event and is likely to
                ;;  happen after there has been some output anyway.
                ;; :sentinel vsh--process-sentinel
                )))
    ;; When insert *at* the process mark, marker will advance.
    (set-marker-insertion-type (process-mark proc) t)
    (set-marker (process-mark proc)
                (save-excursion
                  (goto-char (point-min))
                  (when (looking-at-p (vsh-prompt)) (end-of-line))
                  (point)))
    (setq-local vsh-new-output t)
    proc))

(defun vsh-goto-last-prompt ()
  (interactive)
  (when vsh-last-command-position
    (goto-char vsh-last-command-position)))
(defun vsh-goto-insert-point ()
  (interactive)
  (when (vsh-insert-point)
    (goto-char (vsh-insert-point))))

(defun vsh--set-markers (proc &optional where)
  (let ((where (or where (line-end-position))))
    (set-marker (process-mark proc) where)
    (if (markerp vsh-last-command-position)
        (set-marker vsh-last-command-position (process-mark proc))
      (setq-local vsh-last-command-position (copy-marker (process-mark proc))))))

(defun vsh--get-process (&optional buffer)
  ;; TODO This should give some alert if there is no process with this buffer.
  (get-buffer-process (or buffer (current-buffer))))

(defun vsh--delete-and-send (text-to-send proc)
  (kill-region (vsh--segment-bound nil) (vsh--segment-bound t))
  (vsh--set-markers proc)
  (setq-local vsh-new-output t)
  (process-send-string proc text-to-send))

(defun vsh-execute-command ()
  "If on a command line, execute it.
If on an output line execute the command line relevant for this output.
If on a comment line do nothing.

Returns `true' when we saw a command for programming convenience."
  (interactive)
  (let ((segment-start (vsh--segment-bound nil t))
        (proc (vsh--get-process)))
    (unless (not (save-excursion
                   (goto-char segment-start)
                   (looking-at-p (vsh-command-regexp))))
      (unless (= segment-start (line-beginning-position))
        (goto-char segment-start)
        (vsh-bol))
      (vsh--delete-and-send
       (string-join
        (list
         (buffer-substring-no-properties
          (+ (line-beginning-position) (length (vsh-prompt))) (line-end-position))
         "\n"))
       proc))))

(defun vsh-execute-region ()
  "Run each command in the given region.

Treats things line-wise, so that if the region starts at a point somewhere
part-way through a prompt we execute that prompt."
  (interactive)
  (let ((rend (copy-marker (region-end)))
        (ending-position nil))
    (goto-char (region-beginning))
    (beginning-of-line)
    (while (< (point) rend)
      (when (looking-at-p (vsh-command-regexp))
        (vsh-execute-command)
        (setq ending-position (line-end-position)))
      (forward-line 1))
    (setq rend nil)
    (goto-char ending-position)))

(defun vsh-execute-block (inc-comments)
  "Execute each command line in the given block of commands."
  (interactive "P")
  (vsh-mark-command-block inc-comments)
  (vsh-execute-region))

(defun vsh-execute-and-new-prompt ()
  (interactive)
  (vsh-execute-command)
  (vsh-new-prompt))

(defun vsh-send-control-char (char)
  (interactive "cChar to send as control char:")
  (process-send-string (vsh--get-process) (string (- (upcase char) ?@))))

(defun vsh-send-password (&optional prompt)
  ;; Mostly taken from `comint-send-invisible'.
  ;; As it happens, `comint-send-invisible' mostly works, the only problem is
  ;; that there are a bunch of customisable variables that function uses which a
  ;; user would probably only expect to take affect with `comint' buffers.
  ;;
  ;; Hence writing our own function for this and avoiding those specific
  ;; functions.  Will see some time later on whether I want user-configurable
  ;; things at the same places.
  (interactive "P") ;; Defeat C-x ESC ESC (as per `comint-send-invisible').
  (let ((proc (vsh--get-process)))
    (if proc
        (let ((prefix-prompt (or prompt "Non-echoed text: "))
              str)
          ;; (when comint-password-function
          ;;   (setq str (funcall comint-password-function prefix-prompt)))
          (setq str (read-passwd prefix-prompt))
          (when (stringp str)
            (process-send-string proc (string-join (list str "\n")))))
      (error "Buffer %s has no process" (current-buffer)))))

(defun vsh-send-unterminated (string &optional buffer)
  "Send string without a terminating newline.

This is helpful for times where the underlying process is listening out for
single-key commands and ignoring null bytes."
  (interactive "MString to send: ")
  (let ((proc (vsh--get-process buffer)))
    (process-send-string proc string)))

(defun vsh--receive-readline-bindings (buffer-hash pos-compl glob-expan line-discard)
  (let ((p (base64-decode-string pos-compl))
        (g (base64-decode-string glob-expan))
        (l (base64-decode-string line-discard)))
    (dolist (buffer (buffer-list))
      ;; Using `buffer-base-buffer' for indirect buffers.
      (when (= buffer-hash (sxhash-eq (or (buffer-base-buffer buffer) buffer)))
        (with-current-buffer buffer
          (when (eq 'vsh-mode major-mode)
            (setq-local vsh-completions-keys
                        `((possible-completions . ,p)
                          (glob-list-expansions . ,g)
                          (unix-line-discard    . ,l)))))))))

(defun vsh-request-completions (use-glob)
  "Ask proc in underlying terminal for possible-completions of command so far."
  (interactive "P")
  (when (save-excursion
          (beginning-of-line) (looking-at-p (vsh-command-regexp)))
    (let ((text-to-send
           (string-join
            (list
             (buffer-substring-no-properties
              (+ (line-beginning-position) (length (vsh-prompt))) (point))
             (alist-get (if use-glob 'glob-list-expansions 'possible-completions)
                        vsh-completions-keys)
             (alist-get 'unix-line-discard vsh-completions-keys)))))
      (vsh--delete-and-send text-to-send (vsh--get-process)))))

;; TODO
;; (defun vsh-send-region-other-buffer (rbeg rend &optional buffer)
;;   (interactive "r")
;;   (unless buffer (setq buffer (completing-read <...>)))
;;   (let (....)))

(defvar vsh-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'comment-indent-new-line)
    (define-key map (kbd "C-c C-n") 'vsh-next-command)
    (define-key map (kbd "C-c C-p") 'vsh-prev-command)
    (define-key map (kbd "C-c C-a") 'vsh-bol)
    (define-key map (kbd "C-c n") 'vsh-new-prompt)
    ;; Negative argument puts you at start of next block.
    ;; This is just `beginning-of-defun' function, but we can't use
    ;; `beginning-of-defun-function' because `beginning-of-defun' has a call to
    ;; `beginning-of-line' just after that hook is used.  Hence define our own
    ;; function which leaves us at the start of a prompt.
    (define-key map (kbd "C-M-a") 'vsh-beginning-of-block)
    ;; Similar for `end-of-defun'. In order to make things like `mark-defun'
    ;; work properly we want to go right to the end of the command block, but
    ;; for moving around we want to end up at the start of the last command in a
    ;; block.
    (define-key map (kbd "C-M-e") 'vsh-end-of-block)
    ;; Universal argument marks the "outer" block as per vim nomenclature
    ;; (i.e. counting comment lines same as output).
    ;; Similar to `end-of-defun' and `beginning-of-defun', `mark-defun' mostly
    ;; works with the standard mappings once we've defined
    ;; `*-of-defun-function'.  In this case the reason we define our own
    ;; function is to allow `universal-argument' to choose between including
    ;; comments and not including comments.
    (define-key map (kbd "C-M-h") 'vsh-mark-command-block)
    (define-key map (kbd "C-c C-o") 'vsh-mark-segment)
    ;; Decided against putting this on a keybinding.
    ;; (define-key map TO-CHOOSE 'vsh-make-cmd)

    (define-key map (kbd "C-c C-s") 'vsh-save-command)
    (define-key map (kbd "C-c C-x") 'vsh-activate-command)

    (define-key map (kbd "C-M-x") 'vsh-execute-block)
    (define-key map (kbd "C-c RET") 'vsh-execute-command)
    (define-key map (kbd "C-c C-M-x") 'vsh-execute-region)
    (define-key map (kbd "C-c C-<return>") 'vsh-execute-and-new-prompt)

    (define-key map (kbd "C-c C-d") 'vsh-goto-insert-point)
    (define-key map (kbd "C-c C-z") 'vsh-goto-last-prompt)
    ;; Decided against putting this on a keybinding.
    ;; (define-key map TO-CHOOSE 'vsh-send-region)

    (define-key map (kbd "C-c C-c") 'vsh-send-control-char)
    (define-key map (kbd "C-c TAB") 'vsh-request-completions)

    ;; Decided against putting the below on a keybinding
    ;; (define-key map TO-CHOOSE 'vsh-send-password)
    map))

;; (defvar-keymap vsh-repeat-map
;;   :doc "Keymap to repeat vsh key sequences.  Used in `repeat-mode'."
;;   :repeat t
;;   "C-f" #'vsh-next-command
;;   "C-b" #'vsh-prev-command)

;; Only ever want to use these variables as buffer-local, but don't want them
;; associated with any buffers *except* vsh buffers.
;; Going to define a default (that hopefully all other modes would see if they
;; ever start looking at them) and whenever turning vsh-mode on make the
;; variable buffer-local in the buffer that is getting this mode.

(defun vsh--setup-colors ()
  (setq-local
   font-lock-defaults
   `((;; Ensure commands and comments are given a nice purple face.
      ;; (To match vim should be comment face, but because I'm the writer
      ;; of these plugins and my emacs colorscheme doesn't look good with
      ;; comment face I'm doing something different).
      (,(rx (regexp (vsh-split-regexp))
            (group (one-or-more not-newline)))
       . (1 font-lock-constant-face))
      ;; Ensure a split marker is given the pre-processor face.
      (,(vsh-split-regexp) . font-lock-preprocessor-face))
     ;; keywords-only, if this is nil then strings and comments are
     ;; automatically highlighted.
     ;; I have very little against highlighting them, but when I allow this
     ;; it makes the above highlighting problematic.
     ;; In the vim implementation I've avoided highlighting strings as such
     ;; in outputs, and rather attempted to reproduce some of the control
     ;; character colors that terminals use.
     ;;
     ;; Ideally I would like to do that for emacs too, but for the moment
     ;; I'll be using this much simpler setup that at least gets the two
     ;; main parts colored as I want.
     t))
  (unless vsh-ignore-ansi-colors
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at-p (vsh-split-regexp))
            (forward-line)
          (let ((end-of-segment (vsh--segment-bound t)))
            (ansi-color-apply-on-region (point) end-of-segment t)
            (goto-char end-of-segment)))))))

(defun vsh--initialise-settings ()
  "Default settings for behaviour in this major mode."
  ;; Settings for customisation of this particular major-mode.
  (auto-fill-mode nil)
  (setq-local comment-start (vsh-prompt))
  (setq-local comment-end "")
  (setq-local indent-line-function (lambda () (indent-region nil t)))
  (setq-local beginning-of-defun-function 'vsh--beginning-of-block-fn)
  (setq-local end-of-defun-function 'vsh--end-of-block-fn))

;;; TODO
;; I notice that vimrc-mode puts the derived mode and the `auto-mode-alist'
;; setting under `autoload' comments.  I guess this is probably what I would
;; want to do.  I naively copied that in this file, but it didn't seem to work
;; as expected.
;; For the moment ignoring this thing, working with the base functionality.
;; Will have a look into what it is and whether I want it later.
(define-derived-mode vsh-mode fundamental-mode "Vsh"
  "Major mode for interacting with VSH files.

These files describe a terminal session, they are persistent and allow replaying
what was done with ease.

\\{vsh-mode-map}

Entry to this mode runs the hooks on `vsh-mode-hook'."
  :interactive t
  (make-local-variable 'vsh-last-command-position)
  (make-local-variable 'vsh-buffer-initialised)
  (make-local-variable 'vsh-new-output)
  (make-local-variable 'vsh-completions-keys)
  (make-local-variable 'vsh-ignore-ansi-colors)
  (unless (or (not vsh-may-start-server)
              ;; Need to check this is `fboundp' because `server-start'
              ;; autoloads the server package.
              (and (fboundp 'server-running-p) (server-running-p)))
    (server-start)))

(add-to-list 'auto-mode-alist '("\\.vsh\\'" . vsh-mode))


(provide 'vsh)

;;; vsh.el ends here