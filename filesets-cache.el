(setq-default filesets-be-docile-flag 'nil)

(setq filesets-submenus '("Emacs Config" ("0 Emacs Config" ["Files: Emacs Config" (filesets-open (quote :files) (quote "Emacs Config"))] "---" ["0 89_non_evil_plugin_settings.el" (filesets-file-open nil (quote "/home/apple/.emacs.d/plugin_configurations/89_non_evil_plugin_settings.el") (quote "Emacs Config"))] ["1 99_evil_plugin_configurations.el" (filesets-file-open nil (quote "/home/apple/.emacs.d/plugin_configurations/99_evil_plugin_configurations.el") (quote "Emacs Config"))] ["2 TODO.txt" (filesets-file-open nil (quote "~/.emacs.d/TODO.txt") (quote "Emacs Config"))] ["3 init.el" (filesets-file-open nil (quote "~/.emacs.d/init.el") (quote "Emacs Config"))] "---" ["Close all files" (filesets-close (quote :files) (quote "Emacs Config"))] ["Run Command" (filesets-run-cmd nil (quote "Emacs Config") (quote :files))] ["Add current buffer" (filesets-add-buffer (quote "Emacs Config") (current-buffer))] ["Remove current buffer" (filesets-remove-buffer (quote "Emacs Config") (current-buffer))] ["Rebuild this submenu" (filesets-rebuild-this-submenu (quote "Emacs Config"))])))

(setq filesets-menu-cache '(("0 Emacs Config" ["Files: Emacs Config" (filesets-open (quote :files) (quote "Emacs Config"))] "---" ["0 89_non_evil_plugin_settings.el" (filesets-file-open nil (quote "/home/apple/.emacs.d/plugin_configurations/89_non_evil_plugin_settings.el") (quote "Emacs Config"))] ["1 99_evil_plugin_configurations.el" (filesets-file-open nil (quote "/home/apple/.emacs.d/plugin_configurations/99_evil_plugin_configurations.el") (quote "Emacs Config"))] ["2 TODO.txt" (filesets-file-open nil (quote "~/.emacs.d/TODO.txt") (quote "Emacs Config"))] ["3 init.el" (filesets-file-open nil (quote "~/.emacs.d/init.el") (quote "Emacs Config"))] "---" ["Close all files" (filesets-close (quote :files) (quote "Emacs Config"))] ["Run Command" (filesets-run-cmd nil (quote "Emacs Config") (quote :files))] ["Add current buffer" (filesets-add-buffer (quote "Emacs Config") (current-buffer))] ["Remove current buffer" (filesets-remove-buffer (quote "Emacs Config") (current-buffer))] ["Rebuild this submenu" (filesets-rebuild-this-submenu (quote "Emacs Config"))])))

(setq filesets-ingroup-cache 'nil)

(setq filesets-cache-version "1.8.4")
