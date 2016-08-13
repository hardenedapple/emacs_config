;;;; Plugins and everything not enabled by default
;;;;

;; The usual definition for this variable relies on the hostname
;; "host.does.not.exist" not getting found via DNS.
;; Unfortunately, on some networks (those that are stupid), any host that does
;; not exist gets given a known IP that doesn't respond.
;; Hence the command tramp uses to decide the defaults of this option hangs,
;; causing emacs to hang on startup.
;; Here we give tramp a default value so the initialisation code is not run
;; if 'nslookup' returns an IP for "host.does.not.exist".
;; Otherwise we are on a network set up so that the check will not hang, and the
;; initialisation code may as well be run.
;; This is a problem somewhere before tramp 2.2.13 and after tramp <insert
;; version from solaris>.
;; Infortunately, we can't check for `tramp-version' before loading tramp, so we
;; check for the `emacs-version' that ship by default with these versions.
(unless (and (string-prefix-p "24.5" emacs-version)
             (with-temp-buffer
               (call-process "nslookup" nil t nil "host.does.not.exist")
               (goto-char (point-min))
               (or (search-forward-regexp "can't find" nil t)
                   (search-forward-regexp "No answer" nil t))))
  (defvar tramp-ssh-controlmaster-options " -o ControlPath='tramp.%%r@%%h:%%p'"))

;;; I keep single file packages in this directory
(add-to-list 'load-path "~/.emacs.d/packages/")

(require 'cask "~/.emacs.d/cask/cask.el")
(cask-initialize)

(dolist (p '(
             cl
             eldoc
             paredit
             desktop
             nameses
             splice-window
             transpose-frame
             window-number
             ))
  (require p))

;;; Load all files in the directory config
;;; Name of file denotes order it's loaded in.
;;; Note order matters in way here:
;;;    wrap-region after paredit to not overwrite '('
(dolist (conf-file (directory-files "~/.emacs.d/config/" t "^[^.].+\\.elc?$"))
  (load conf-file))

;;; Remove some settings from MINOR-MODE-ALIST
;;;
(defvar minor-mode-show-list
  (list
   'compilation-in-progress
   'compilation-minor-mode
   'compilation-shell-minor-mode
   'diff-minor-mode
   'overwrite-mode
   'projectile-mode
   'undo-tree-visualizer-selection-mode
   'window-number-mode
   'keyswap-mode
   'swift-motion-mode
   'lisp-motion-mode
   'paredit-motion-mode
   )
  "List of minor-modes to show in modeline")
(setq minor-mode-alist
      (cl-remove-if-not
       (lambda (val)
         (member (car val) minor-mode-show-list))
       minor-mode-alist))
