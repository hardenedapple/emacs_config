;;; Splice window functions
(defun splice-window--get-split-type (&optional window forwards)
  "Return split direction for WINDOW.
Returns nil if WINDOW is either the root window or the minibuffer window."
  (cond
   ((window-combined-p window) (if forwards 'below 'above))
   ((window-combined-p window t) (if forwards 'right 'left))))

(defun splice-window--get-all-window-siblings
    (&optional direction window)
  "Return list of WINDOW's siblings in given DIRECTION.

Default direction is next."
  (let* ((window-iterator-function (if (eql direction 'prev)
                                       'window-prev-sibling
                                     'window-next-sibling))
         ;; Here selected window takes over when there are no siblings
         (current-sibling (or window (selected-window)))
         return-list)
    (while (setq current-sibling
                 (funcall window-iterator-function current-sibling))
      (push
       (window-state-get current-sibling t)
       return-list))
    return-list))

(defun splice-window--add-back-windows (base-window to-add forwards
                                                    &optional direction)
  "Add window specification TO-ADD into BASE-WINDOW's config."
  (let ((direction
         (or direction
             (splice-window--get-split-type base-window forwards)
             (if (>= (/ (window-body-width base-window) split-width-threshold)
                     (/ (window-body-height base-window) split-height-threshold))
                 (if forwards 'right 'left)
               (if forwards 'below 'above)))))
    ;; Recursion needed
    ;; Split current window, work on new one
    ;; set window-combination-limit to t
    ;; Do splice-window--add-back-windows on first child
    (dolist (conf to-add)
      (window-state-put
       conf (split-window base-window nil direction) 'safe))))

(defun splice-window-upwards (&optional window)
  "Splice current level of WINDOW ancestry up one.

If WINDOW is not eq to or a child of `frame-root-window', then it
  must be at least one level of nesting down.

This function remove one level of nesting e.g., given this frame

    +------------+------------+
    |            |     B      |
    |     A      +------------+
    |            |     C      |
    +------------+------------+
    |            D            |
    +-------------------------+

when called on window B leaves the frame as

    +--------+--------+-------+
    |        |        |       |
    |    A   |    B   |   C   |
    |        |        |       |
    +--------+--------+-------+
    |            D            |
    +-------------------------+

when called on window A, leave the frame as
not scaled correctly

    +-------------------------+
    |                         |
    |            A            +
    |                         |
    +-------------------------+
    |            C            |
    +-------------------------+
    |            B            |
    +-------------------------+
    |                         |
    |            D            |
    |                         |
    +-------------------------+

This function leaves any preexisting subnests like the B/C window
in the example."
  (interactive)
  (let ((cur-win (or window (selected-window)))
        (original-win (selected-window)))
    (unless (or (one-window-p)
                (frame-root-window-p (window-parent cur-win)))
      (let ((forward-siblings
             (splice-window--get-all-window-siblings 'next cur-win))
            (backward-siblings
             (splice-window--get-all-window-siblings 'prev cur-win)))
        ;; Remove current siblings
        ;; once all siblings are closed, emacs automatically splices the
        ;; remaining window into the above level.
        (delete-other-windows-internal cur-win (window-parent cur-win))
        ;; If selected window is in the windows to put back, `window-state-put'
        ;; deals with selecting the window. If it isn't
        ;; `delete-other-windows-internal' has just selected the wrong window,
        ;; and won't change it back, so we deal with that here
        (when (window-live-p original-win) (select-window original-win))
        (splice-window--add-back-windows cur-win forward-siblings t)
        (splice-window--add-back-windows cur-win backward-siblings nil)))))

(defun merge-windows (window1 &rest windows)
  "Merge all WINDOWS into WINDOW1.

This is the opposite of `splice-window-upwards'."
  (let ((confs (mapcar #'window-state-get windows))
        (direction (if (window-combined-p window1) 'right 'below)))
    (dolist (add-window windows)
      (delete-window add-window))
    (dolist (add-conf confs)
      (window-state-put add-conf (split-window window1 nil direction)))))

;;; Swap windows
(defun swap-windows-properly (window1 &optional window2)
  "Swap WINDOW1 and WINDOW2 respecting any splits

Takes two windows, and swaps them, works if one or both of the
windows are internal ones, i.e. if `window-live-p' returns `nil'
on them."
  (unless (windowp window1)
    (error "Must supply valid WINDOW1 to SWAP-WINDOWS-PROPERLY"))
  (let ((window2 (or window2 (selected-window))))
    (let ((state1 (window-state-get window1 t))
          (state2 (window-state-get window2 t)))
      (window-state-put state1 window2 'safe)
      (window-state-put state2 window1 'safe))))

(provide 'splice-windows)
