;; Configuration for magit
(setq magit-repo-dirs '("~/.emacs.d" "~/share/repos/useful-files"))
(defun magit-stage-this-file ()
  "Stage the file the current buffer is visiting."
  (interactive)
  (magit-stage-item (buffer-file-name)))

;;; Set some keys for evil
(evil-leader/set-key
  "gg" 'magit-log
  "gw" 'magit-stage-this-file
  "gd" 'magit-diff-unstaged
  "gp" 'magit-push
  "gf" 'magit-pull
  "gs" 'magit-status)
