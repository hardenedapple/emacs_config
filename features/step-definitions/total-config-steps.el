;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I am in \"\\([^\"]+\\)\" mode$"
       (lambda (mode-name)
         (Given (format "I am in buffer \"*test-%s-mode*\"" mode-name))
         (Given "I start an action chain")
         (And "I press \"M-x\"")
         (And (format "I type \"%s-mode\"" mode-name))
         (And "I execute the action chain")))

(Then "^the cursor should be at the end of the buffer$"
      (lambda ()
        (cl-assert (= (buffer-size) (point)))))
