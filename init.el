;;;; Plugins and everything not enabled by default
;;;;

;;; Set up packages and load configurations.
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

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
(unless (with-temp-buffer
          (call-process "nslookup" nil t nil "host.does.not.exist")
          (goto-char (point-min))
          (search-forward-regexp "No answer" nil t))
  (defvar tramp-ssh-controlmaster-options " -o ControlPath='tramp.%%r@%%h:%%p'"))

;;; I keep single file packages in this directory
(add-to-list 'load-path "~/.emacs.d/packages/")

(when (not package-archive-contents)
  (package-refresh-contents))

(let ((download-only '(monokai-theme tangotango-theme))
      (elpa-packages '(avy arduino-mode buffer-move c-eldoc elisp-slime-nav
                                     expand-region goto-chg ido-ubiquitous
                                     ido-at-point ido-vertical-mode jump-char
                                     list-register magit monky multiple-cursors
                                     paredit projectile python-pylint quack
                                     smart-tab smart-window smex undo-tree
                                     vimrc-mode window-number wrap-region
                                     xcscope yasnippet transpose-mark
                                     markdown-mode
                                     ;; Occasionally use the below to show
                                     ;; currently unbound keys, which is useful
                                     ;; for deciding on a keybinding.
                                     ;; unbound
                                     ))
      (require-only '(desktop dired-x eldoc em-smart eshell
                              le-eval-and-insert-results nameses transpose-frame
                              splice-windows
                              uniquify epa-file)))
  ;; Install packages, require packages
  (dolist (p (append elpa-packages download-only))
    (unless (package-installed-p p)
      (package-install p)))

  (dolist (p (append require-only elpa-packages))
    (require p)))

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
   )
  "List of minor-modes to show in modeline")
(setq minor-mode-alist
      (cl-remove-if-not
       (lambda (val)
         (member (car val) minor-mode-show-list))
       minor-mode-alist))
