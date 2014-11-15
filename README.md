smart-log.el
============

Major mode with intepretting miscellaneous time format in log file.

Usage
=====

* Put the following to your .emacs

    (autoload 'smart-log-mode "smart-log"
              "Intepretting miscellaneous time format in log file" t)

* You may want to open log file automatically `smart-log-mode', put the followings.

    ;; System log directory (on Debian)
    (add-to-list 'auto-mode-alist `("/var/log/" . smart-log-mode))
    ;; djb tool
    (add-to-list 'auto-mode-alist
                 `("/@[a-f0-9]\\{24\\}\\.[su]\\'" . smart-log-mode))
    ;; General log filename and logrotated files.
    (add-to-list 'auto-mode-alist
                 '("\\.log\\(?:\\.[0-9]+\\)?\\(?:\\.\\(?:gz\\|bz2\\|xz\\)\\)?\\'"
                   . smart-log-mode))

* You may want to revert tail of log automatically, put the following to your .emacs

    (setq smart-log-auto-revert-tail t)

* You can find huge log file:

    M-x smart-log-find-file
