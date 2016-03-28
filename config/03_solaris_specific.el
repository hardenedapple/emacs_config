;; Use ggrep for grep-command when using Solaris
(when (string-equal system-type "usg-unix-x")
  (grep-apply-setting 'grep-command "ggrep -Hn ")
  (grep-apply-setting 'grep-template "ggrep <X> <C> -Hn <R> <F>")
  (grep-apply-setting 'grep-find-command
                      '("/usr/gnu/bin/find . -type f -exec ggrep -n  /dev/null {} +" . 43))
  (grep-apply-setting 'grep-find-template
                      "/usr/gnu/bin/find . <X> -type f <F> -exec ggrep <C> -n <R> /dev/null {} +"))
