;; Filesets
(filesets-init)
(setq filesets-data `(("Emacs Config"
                       (:files "~/.emacs.d/TODO.txt"
                               "~/.emacs.d/init.el"
                               ,@(directory-files
                                  "~/.emacs.d/plugin_configurations" t
                                  "^.+\\.elc?$")))))

;;; Note can run any command on all files in a set once the command is in the
;;; variable filesets_commands http://stackoverflow.com/questions/7071915/emacs-filesets-how-to-run-other-elisp-not-shell-commands
