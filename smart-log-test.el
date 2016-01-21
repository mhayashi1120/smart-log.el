(require 'smart-log)

(require 'ert)
(require 'cl-lib)

(cl-loop with test-body = "a\nbb\nccc\ndddd\n"
         with file = (make-temp-file (concat temporary-file-directory "smart-log-"))
         for (eol . cases)
         in '((unix (0 0) (1 0) (2 2) (3 2) (4 2))
              (dos (0 0) (1 0) (2 0) (3 3) (4 3) (5 3) (6 3) (7 7))
              (mac (0 0) (1 0) (2 2) (3 2) (4 2))
              )
         do (progn
              (let ((coding-system-for-write eol))
                (write-region test-body nil file nil 'no-msg))
              (cl-loop for (arg res) in cases
                       do
                       (should (equal (smart-log--find-bol file arg) res)))))
