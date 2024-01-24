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

(defcustom vsh-mode-hook '()
  "Hook for customizing vsh mode."
  :type 'hook
  :group 'vsh)

(defvar vsh-new-output nil
  "Boolean indicating whether we are in the middle of an output or not.

This is not supposed to be a marker between logical subcommands of e.g. a bash
process, rather it is whether we recently manually moved the marker position
(e.g. because a command was just executed).")

(defvar vsh-last-command-position nil
  "Marker giving line of last text sent to the underlying pseudo-terminal.")
(defvar vsh-buffer-initialised nil
  "Has this current buffers process been initialised.")

;; TODO
;;   - Ensure that pressing enter on a prompt gives us a newline with a prompt
;;     already inserted while pressing enter on a line without a prompt gives us
;;     a newline without that prompt.
;;   - Add the "send-control-character" functionality.
;;   - Maybe do something about the `repeat-mode' stuff.
;;   - Allow buffer-local prompt.
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
  ; The reason for defining things with whitespcae stripped is that in *vim*
  ; (where this file format was defined) there was an unfortunate interaction
  ; between automatic removal of trailing whitespace and that turning some
  ; "empty command lines" into "output" lines.
  ; This could happen in emacs too -- but even if we didn't have the same
  ; problem, we'd want to keep the format of a file meaning the same thing
  ; between the two text editors.
  ; (string-join (list "^" (vsh-prompt buffer)))
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
  ;; TODO Would be nice to handle that "next command" with cursor at the very
  ;; start of a command (e.g. on the `s' of `vshcmd') takes us to the start of
  ;; *this* command.  On the other hand, it's not too important, and seems a
  ;; little tricky to implement.
  ;; I don't think I chose that in vim on purpose.
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

(defun vsh-beginning-of-block (count)
  (interactive "p")
  (if (> count 0)
      (progn
        (beginning-of-line)
        (dotimes (loop-counter count)
          (re-search-backward (vsh-split-regexp) nil 'move-to-end)
          (vsh--move-to-end-of-block (vsh-split-regexp) nil)))
    (dotimes (loop-counter (abs count))
      (vsh--move-to-end-of-block (vsh-split-regexp) t)
      (re-search-forward (vsh-split-regexp) nil 'move-to-end)))
  (vsh-bol))

(defun vsh-end-of-block (count)
  (interactive "p")
  (if (> count 0)
      (progn
        (dotimes (loop-counter count)
          (re-search-forward (vsh-split-regexp) nil 'move-to-end)
          (vsh--move-to-end-of-block (vsh-split-regexp) t))        
        (backward-char))
    (dotimes (loop-counter (abs count))
      (vsh--move-to-end-of-block (vsh-split-regexp) nil)
      (re-search-backward (vsh-split-regexp) nil 'move-to-end)))
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

(defun vsh--process-filter (proc output)
  (with-current-buffer (process-buffer proc)
    ;; Horrible hack to avoid the very first prompt given by the underlying
    ;; process on startup.
    (when (or vsh-buffer-initialised (/= 1 (seq-count (lambda (x) (= x ?\n)) output)))
      (save-excursion
        (goto-char (or (vsh-insert-point) (point-max)))
        ;; When the underlying process outputs only part of a line I want to
        ;; append the next bit of output to the end of that line.  However, when
        ;; I have just ran `vsh-execute-command' I want the output to be added
        ;; just below the current prompt.
        ;; Rather than always add a newline into the buffer when I run
        ;; `vsh-execute-command' I append a newline to the start of the first
        ;; output ran after such a command.  This allows running multiple lines
        ;; with `vsh-execute-command' (or similar) before any output comes and
        ;; not having blank lines between the lines that were run.
        (when vsh-new-output (insert-char ?\n) (setq vsh-new-output nil))
        (insert output)))
    (unless vsh-buffer-initialised (setq vsh-buffer-initialised t))))

;; To think about:
;; Do I need to call `accept-process-output'?
;; - I think not -- I'm guessing I want to wait until emacs is waiting on the
;;   user to be handling output.

(defun vsh--start-process ()
  ;; TODO Will need to see if there is any way that we could end up running this
  ;; command when in a different buffer.
  ;; Assuming not for now.
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc (delete-process proc)))
  ;; `default-directory' should be determined by the buffer directory.
  ;; `process-environment' 
  (let* ((process-environment
          (list "TERM=dumb" "VSH_EMACS_LISTEN_ADDRESS=not-yet-set"))
         (proc (make-process
                :name "vsh-proc"
                :buffer (current-buffer)
                ;; TODO Going to have to ship a copy of this shell script.
                ;; Maybe a shared subrepo in order to ensure consistency between
                ;; the vim and emacs versions.
                :command (list "/home/matmal01/.vim/bundle/vsh/autoload/vsh/vsh_shell_start"
                               "/home/matmal01/.vim/bundle/vsh/autoload/vsh" "bash")
                :connection-type 'pty
                :noquery t
                :filter 'vsh--process-filter
                ;;  Currently not defining this as expect the default will be
                ;;  good enough.
                ;;  :sentinel vsh--process-sentinel
                )))
    ;; When insert *at* the process mark, marker will advance.
    (set-marker-insertion-type (process-mark proc) t)
    (set-marker (process-mark proc)
                (save-excursion
                  (goto-char (point-min))
                  (when (looking-at-p (vsh-prompt)) (end-of-line))
                  (point)))
    (setq vsh-new-output t)
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
      (setq vsh-last-command-position (copy-marker (process-mark proc))))))

(defun vsh--get-process (&optional buffer)
  ;; TODO This should give some alert if there is no process with this buffer.
  (get-buffer-process (or buffer (current-buffer))))

(defun vsh-execute-command ()
  "If on a command line, execute it.
If on an output line execute the command line relevant for this output.
If on a comment line do nothing.

Returns `true' when we saw a command for programming convenience."
  ;; TODO -- almost everything here.
  (interactive)
  (let ((segment-start (vsh--segment-bound nil t))
        (proc (vsh--get-process)))
    (unless (not (save-excursion
                   (goto-char segment-start)
                   (looking-at-p (vsh-command-regexp))))
      (unless (= segment-start (line-beginning-position))
        (goto-char segment-start)
        (vsh-bol))
      (kill-region (vsh--segment-bound nil) (vsh--segment-bound t))
      (vsh--set-markers proc)
      (setq vsh-new-output t)
      (process-send-string
       proc
       (string-join
        (list
         (buffer-substring (+ (line-beginning-position) (length (vsh-prompt)))
                           (line-end-position))
         "\n"))))))

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

(defvar vsh-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'vsh-next-command)
    (define-key map (kbd "C-c C-p") 'vsh-prev-command)
    (define-key map (kbd "C-c C-a") 'vsh-bol)
    ;;; TODO
    ;; Negative argument puts you at start of next block.
    (define-key map (kbd "C-M-a") 'vsh-beginning-of-block)
    ;; Negative argument puts you at end of previous block.
    (define-key map (kbd "C-M-e") 'vsh-end-of-block)
    ;; Universal argument marks the "outer" block as per vim nomenclature
    ;; (i.e. counting comment lines same as output).
    (define-key map (kbd "C-M-h") 'vsh-mark-command-block)
    (define-key map (kbd "C-c C-o") 'vsh-mark-segment)
    ;; Decided against putting this on a keybinding.
                                        ; (define-key map TO-CHOOSE 'vsh-make-cmd)
    
    (define-key map (kbd "C-c C-s") 'vsh-save-command)
    (define-key map (kbd "C-c C-x") 'vsh-activate-command)
    
    (define-key map (kbd "C-M-x") 'vsh-execute-block)
    (define-key map (kbd "C-c RET") 'vsh-execute-command)
    (define-key map (kbd "C-c C-RET") 'vsh-execute-region)

    (define-key map (kbd "C-c C-d") 'vsh-goto-insert-point)
    (define-key map (kbd "C-c C-z") 'vsh-goto-last-prompt)
    ;; Decided against putting this on a keybinding.
                                        ; (define-key map TO-CHOOSE 'vsh-send-region)

    ;; (define-key map TO-CHOOSE 'vsh-send-control-char)
    ;; (define-key map TO-CHOOSE 'vsh-request-completions)
    ;; (define-key map TO-CHOOSE 'vsh-request-globs)
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

(defun vsh-setup-colors ()
  (setq font-lock-defaults
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
          t)))


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
  (vsh-setup-colors)
  (vsh--start-process))

(add-to-list 'auto-mode-alist '("\\.vsh\\'" . vsh-mode))


(provide 'vsh)

;;; vsh.el ends here
