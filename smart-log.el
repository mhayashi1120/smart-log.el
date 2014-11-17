;;; smart-log.el --- View log file mode with intepretting miscellaneous time format.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: applications, development
;; URL: https://github.com/mhayashi1120/smart-log.el/raw/master/smart-log.el
;; Emacs: GNU Emacs 23 or later
;; Version: 0.1.0
;; Package-Requires: ()

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * Put the following to your .emacs:
;;
;;   (autoload 'smart-log-mode "smart-log"
;;             "View log file mode." t)
;;   (autoload 'smart-log-find-file "smart-log"
;;             "Open huge log FILE with chunked.")

;; * You may want to open log file automatically `smart-log-mode', put the followings:
;;
;;   ;; System log directory (on Debian)
;;   (add-to-list 'auto-mode-alist `("\\`/var/log/" . smart-log-mode))
;;   ;; djb tool
;;   (add-to-list 'auto-mode-alist
;;                `("/@[a-f0-9]\\{24\\}\\.[su]\\'" . smart-log-mode))
;;   ;; General log filename and logrotated files.
;;   (add-to-list 'auto-mode-alist
;;                '("\\.log\\(?:\\.[0-9]+\\)?\\(?:\\.\\(?:gz\\|bz2\\|xz\\)\\)?\\'"
;;                  . smart-log-mode))

;; * You may want to revert tail of log automatically, put the following to your .emacs:
;;
;;   (setq smart-log-auto-revert-tail t)

;; * You can find huge log file by:
;;
;;   M-x smart-log-find-file

;;; TODO:
;; * grep with regexp by date and date-time range
;; * define-key of toggle auto tail revert
;; * disable auto-tail-revert if :paging prop is not indicate max byte
;; * performance (e.g. a lot of jka call)
;; * log file should not exceed 1024 bytes per line.
;; * activate region sequential logging date, improve guessed format.

;;; Code:

(eval-when-compile
  ;;TODO remove it
  (require 'cl)
  (require 'parse-time))

(defgroup smart-log ()
  "Smart log viewer."
  :group 'applications)

(require 'parse-time)
(require 'view)

;;;
;;; Basic utility
;;;

(defun smart-log--filter-physical-file (file)
  (let* ((jka-info (jka-compr-get-compression-info file))
         (rawfile (if jka-info
                      (file-local-copy file)
                    file)))
    rawfile))

(defun smart-log--detect-coding-system (file)
  ;;TODO auto-coding-alist
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert-file-contents file nil 0 4096)
    last-coding-system-used))

(defun smart-log--extract-year (time)
  (nth 5 (decode-time time)))

(defvar smart-log--plist nil)
(make-variable-buffer-local 'smart-log--plist)

;;;
;;; Datetime deformat
;;;

(defvar smart-log--unformatters
  '(
    ;; djb tools (@400000004f90304a3abfd92c => 2012-04-20 00:33:20)
    (:name tai64n :sample-fn smart-log-tai64n
           :convert-fn smart-log-tai64n->date)
    ;; (May 20 00:05:02 => 2012-05-20 00:05:02)
    ;;   when log file have entries that over a year.
    (:name rfctime :sample-fn smart-log-rfc822-time
           :convert-fn smart-log-rfc822-time->date)
    ;; (01/02/03 04:05:06 => 2001-02-03 04:05:06)
    (:name general-jp :sample-fn smart-log-general-locale-date
           :convert-fn smart-log-general-locale-jp->date)
    ;; (01/02/03 04:05:06 => 2003-02-01 04:05:06)
    (:name general-en :sample-fn smart-log-general-locale-date
           :convert-fn smart-log-general-locale-en->date)
    ;; (01/02/03 04:05:06 => 2003-01-02 04:05:06)
    (:name general-us :sample-fn smart-log-general-locale-date
           :convert-fn smart-log-general-locale-us->date)
    (:name general :sample-fn smart-log-general-date
           :convert-fn smart-log-general-date->date)
    ;; apache (20/May/2012:07:35:25 +0900 => 2012-05-20 07:35:25)
    (:name CLF :sample-fn smart-log-clf-date
           :convert-fn smart-log-clf-date->date)
    ;; apache error log (Sun May 20 07:35:25 2012 => 2012-05-20 07:35:25)
    (:name apache-error-log :sample-fn smart-log-apache-error-log
           :convert-fn smart-log-apache-error-log->date)
    ;; squid or else... (1337993229 => 2012-05-26 09:47:09)
    (:name epoch :sample-fn smart-log-epoch
           :convert-fn smart-log-epoch->date)
    )
  "Considerable log format alist each item has (NAME REGION-FN TO-DATE-FN)
NAME
:sample-fn is called at beginning of line and return region of
   logged time in buffer.
:convert-fn accept one string arg and return `current-date' format.
")

;;
;; rfc822
;;

(eval-and-compile
  (defconst smart-log-rfc822-time-part-regexp
    (let* ((ms (loop repeat 12 for m in parse-time-months collect (car m)))
           (ws (loop repeat 7 for m in parse-time-weekdays collect (car m)))
           (mr (regexp-opt ms))
           (wr (regexp-opt ws))
           (2d "[0-9]\\{1,2\\}")
           (regexp
            ;; ignore some line header characters.
            ;; e.g. Nov  9 01:31:08
            (concat
             ;; day of the week
             "\\(\\b" wr ", +\\)?"
             ;; month name
             "\\(\\b" mr "\\b\\)"
             " +"
             ;; days of month
             2d
             " "
             ;; time part
             2d ":" 2d ":" 2d)))
      regexp)))

(defconst smart-log-rfc822-time-regexp
  (eval-when-compile
    (let* ((regexp
            ;; ignore some line header characters.
            ;; e.g. Nov  9 01:31:08
            (concat
             "^"
             ;; skip max 16 chars
             ".\\{,16\\}?"
             smart-log-rfc822-time-part-regexp)))
      regexp)))

(defun smart-log-rfc822-time ()
  (and (looking-at smart-log-rfc822-time-regexp)
       (cons (or (match-beginning 1) (match-beginning 2))
             (match-end 0))))

(defun smart-log-rfc822-time->date (str)
  (destructuring-bind (sec min hour day month year . rest)
      (parse-time-string str)
    (encode-time sec min hour day month
                 (or year
                     (plist-get smart-log--plist :base-year)))))

;;
;; epoch
;;

(defun smart-log-epoch ()
  (and (looking-at "[0-9.]+")
       (cons (match-beginning 0) (match-end 0))))

(defun smart-log-epoch->date (str)
  (seconds-to-time (string-to-number str)))

;;
;; tai64n (djb)
;;

;;http://cr.yp.to/libtai/tai64.html#tai64n

(defvar smart-log-tai64n-regexp
  "^@[0-9a-f]\\{24\\}")

(defun smart-log-tai64n ()
  ;; djb tool
  (and (looking-at smart-log-tai64n-regexp)
       (cons (match-beginning 0) (match-end 0))))

(defun smart-log-tai64n->date (str)
  (let ((i1 (substring str 1 5))
        (i2 (substring str 5 9))
        (i3 (substring str 9 13))
        (i4 (substring str 13 17))
        (i5 (substring str 17 21))
        (i6 (substring str 21 25)))
    (let ((high (string-to-number i3 16))
          (low (string-to-number i4 16))
          ;; Emacs `format-time-string' cannot handle milli seconds.
          ;; Only ignore the 3rd and 4th element.
          (mhigh (string-to-number i5 16))
          (mlow (string-to-number i6 16)))
      ;; 1. ignore first and second short value (i1 and i2)
      ;;
      ;; 2. tai64n is start from 1970/1/1 00:00:10
      ;;    emacs time is start from 1970/1/1 00:00:00
      (cond
       ((< low 10)
        (setq high (1- high))
        (setq low (- (+ ?\xffff low) 10)))
       (t
        (setq low (- low 10))))
      (list high low mhigh mlow))))

;;
;; General date
;;

(eval-and-compile
  (defconst smart-log-general-date-part-regexp
    (let ((2d "[0-9]\\{1,2\\}"))
      (concat
       ;; date part
       "\\b\\([0-9]\\{4\\}\\)[-/.]\\(" 2d "\\)[-/.]\\(" 2d "\\)\\b"
       ;; skip general separator
       "[ \t,:;]+"
       ;; time part
       "\\(" 2d "\\):\\(" 2d "\\)\\(?::\\(" 2d "\\)\\)?"
       "\\(?:[:.]\\([0-9]+\\)\\)?"))))

(defconst smart-log-general-date-regexp
  (eval-when-compile
    (concat
     ;; skip 16 chars
     ".\\{,16\\}?"
     smart-log-general-date-part-regexp)))

(defun smart-log-general-date ()
  (and (looking-at smart-log-general-date-regexp)
       (cons (match-beginning 1) (match-end 0))))

(defun smart-log-general-date->date (str)
  (and (string-match smart-log-general-date-regexp str)
       (let* ((getter (lambda (x)
                        (let ((s (match-string x str)))
                          (or (and s (string-to-number s)) 0))))
              (year (funcall getter 1))
              (month (funcall getter 2))
              (day (funcall getter 3))
              (hour (funcall getter 4))
              (min (funcall getter 5))
              (sec (funcall getter 6)))
         (encode-time sec min hour day month year))))

(eval-and-compile
  (defconst smart-log-general-locale-date-part-regexp
    (let ((2d "[0-9]\\{1,2\\}"))
      (concat
       ;; date part
       "\\b\\([0-9]\\{2,4\\}\\)[-/.]\\(" 2d "\\)[-/.]\\([0-9]\\{2,4\\}\\)\\b"
       ;; skip general separator
       "[ \t,:;]+"
       ;; time part
       "\\(" 2d "\\):\\(" 2d "\\)\\(?::\\(" 2d "\\)\\)?"
       "\\(?:[:.]\\([0-9]+\\)\\)?"))))

;; http://www.kanzaki.com/docs/html/dtf.html
(defconst smart-log-general-locale-date-regexp
  (eval-when-compile
    (concat
     ;; skip 32 chars
     ".\\{,32\\}?"
     smart-log-general-locale-date-part-regexp)))

(defun smart-log-general-locale-date ()
  (and (looking-at smart-log-general-locale-date-regexp)
       (cons (match-beginning 1) (match-end 0))))

(defun smart-log-general-locale-date->date (str yi mi di)
  (and (string-match smart-log-general-locale-date-regexp str)
       (let* ((getter (lambda (x)
                        (let ((s (match-string x str)))
                          (or (and s (string-to-number s)) 0))))
              (year (let ((tmp (funcall getter (1+ yi))))
                      (cond
                       ((> tmp 100) tmp)
                       ((< (plist-get smart-log--plist :base-year) 2000)
                        (+ 1900 tmp))
                       (t
                        (+ 2000 tmp)))))
              (month (funcall getter (1+ mi)))
              (day (funcall getter (1+ di)))
              (hour (funcall getter 4))
              (min (funcall getter 5))
              (sec (funcall getter 6)))
         (encode-time sec min hour day month year))))

(defun smart-log-general-locale-en->date (str)
  (smart-log-general-locale-date->date str 2 1 0))

(defun smart-log-general-locale-us->date (str)
  (smart-log-general-locale-date->date str 2 0 1))

(defun smart-log-general-locale-jp->date (str)
  (smart-log-general-locale-date->date str 0 1 2))

;;
;; Apache Error Log
;;

(defconst smart-log--apache-error-log-re
  (eval-when-compile
    (concat
     "\\["
     "\\("
     "[a-zA-Z]\\{3\\}"
     " "
     "[a-zA-Z]\\{3\\}"
     " "
     "[0-9]\\{1,2\\}"
     " "
     "[0-9:]+"
     " "
     "[0-9]\\{4\\}"
     "\\)"
     "\\]"
     )))

(defun smart-log-apache-error-log ()
  ;;FIXME regexp
  (and (looking-at smart-log--apache-error-log-re)
       (cons (match-beginning 1) (match-end 1))))

(defun smart-log-apache-error-log->date (str)
  (apply 'encode-time (parse-time-string str)))

;;
;; Common Log Format (CLF)
;;

(defconst smart-log--clf-log-re
  (eval-when-compile
    ;;FIXME regexp is not correct...
    (concat
     ;; skip
     ".\\{,64\\}"
     "\\["
     "\\([0-9]\\{1,2\\}/.../[0-9]\\{4\\}\\(?::[0-9]\\{2\\}\\)\\{3\\}.*?\\)"
     "\\]")))

(defun smart-log-clf-date ()
  (and (looking-at smart-log--clf-log-re)
       (cons (match-beginning 1) (match-end 1))))

(defun smart-log-clf-date->date (str)
  (let* ((data (split-string str "[]\\[/: ]" t))
         (get (lambda (x) (nth x data)))
         (getnum (lambda (x)
                   (let ((s (funcall get x)))
                     (or (and s (string-to-number s)) 0))))
         (year (funcall getnum 2))
         (month-nm (funcall get 1))
         (month (cdr (assoc-string month-nm parse-time-months t)))
         (day (funcall getnum 0))
         (hour (funcall getnum 3))
         (min (funcall getnum 4))
         (sec (funcall getnum 5))
         (zone (funcall get 6))
         (tz (smart-log--parse-timezone zone)))
    (encode-time sec min hour day month year tz)))

(defun smart-log--parse-timezone (zone)
  (cond
   ((string-match "^\\([+-]\\)?\\([0-9]\\{1,2\\}\\):?\\([0-9]\\{1,2\\}\\)$" zone)
    (let* ((mark (match-string 1 zone))
           (hh (match-string 2 zone))
           (mm (match-string 3 zone))
           (sign (if (string= mark "+") 1 -1))
           (hour (string-to-number hh))
           (min (string-to-number mm)))
      (* sign (* (+ min (* hour 60)) 60))))))

;;;
;;; Compute formatter
;;;

(defun smart-log--line-date (sample-fn convert-fn)
  (let ((region (funcall sample-fn)))
    (when region
      (condition-case nil
          (let* ((start (car region))
                 (end (cdr region))
                 (str (buffer-substring start end))
                 (date (funcall convert-fn str)))
            date)
        (error nil)))))

(defun smart-log--valid-formatters ()
  ;; 1. log file MUST start with LOG line.
  (goto-char (point-min))
  (remq
   nil
   (mapcar
    (lambda (fmtr)
      (and (funcall (plist-get fmtr :sample-fn))
           fmtr))
    smart-log--unformatters)))

(defun smart-log--score-formats (formatters)
  (mapcar
   (lambda (fmtr)
     (let ((score (smart-log--score-formatter fmtr)))
       (cons score fmtr)))
   formatters))

(defun smart-log--score-formatter (formatter)
  (let ((scores '(1))
        (sample-fn (plist-get formatter :sample-fn))
        (convert-fn (plist-get formatter :convert-fn))
        (mtime (plist-get smart-log--plist :mtime))
        (valid 0)
        (all 0)
        last dates)
    (goto-char (point-min))
    (while (not (eobp))
      (setq all (1+ all))
      (ignore-errors
        (let ((date (smart-log--line-date sample-fn convert-fn)))
          (setq dates (cons date dates))
          (setq valid (1+ valid))))
      (forward-line 1))
    (setq last (car dates))
    ;; 4. last log line have valid time (nearly equal file mtime)
    ;; DIFF-SEC    := ABS(FILETIME - LOG-LAST-ENTRY)
    ;; PROBABILITY := 1 - (DIFF-SEC / 120)^2
    ;; SCORE       := MAX(PROBABILITY, 0.5)
    (let* ((diff (abs (- (float-time last) mtime)))
           (probability (- 1 (expt (/ diff 120.0) 2)))
           (score (max probability 0.5)))
      (push score scores))
    ;;TODO 3. check order of log entry times.
    ;; 2. valid log line approprivate count.
    (push (/ valid (ftruncate all)) scores)
    (apply '* scores)))

;; `smart-log' guess time format by following assumption:
;; 1. log file MUST start with LOG line.
;;    that line MUST have time string in first 1024 bytes.
;; 2. lines should have valid date appropriate count.
;; 3. lines have been nearly sorted by date.
;;    log time format may be deficient year part.
;; 4. log file highly probably end with log entry that
;;    have time nearly equal file mtime.
(defun smart-log--compute-formatter ()
  (save-excursion
    (loop with formatters = (smart-log--valid-formatters)
          with max = 0
          with fmtr = nil
          for (score . formatter) in (smart-log--score-formats formatters)
          do (when (> score max)
               (setq max score
                     fmtr formatter))
          finally return fmtr)))

(defun smart-log--compute-from-file (file)
  (with-temp-buffer
    (let* ((rawfile (smart-log--filter-physical-file file))
           (attr (file-attributes rawfile))
           (size (nth 7 attr))
           (mtime (nth 5 attr))
           (head-end 2048)
           (tail-beg (max (- size 4096) head-end)))
      (insert-file-contents rawfile nil 0 head-end)
      (goto-char (point-max))
      (delete-region (point-at-bol) (point-max))
      (when (> size tail-beg)
        (insert-file-contents rawfile nil tail-beg size))
      (setq smart-log--plist (list :mtime (float-time mtime)))
      (smart-log--compute-formatter))))

(defun smart-log--compute-from-buffer (buffer)
  (with-temp-buffer
    (let ((compute-buf (current-buffer))
          mtime)
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            ;; first some lines
            (goto-char (point-min))
            (let ((head-end (point-at-bol 3)))
              (append-to-buffer compute-buf (point-min) head-end)
              ;; last some lines
              (goto-char (point-max))
              (let ((tail-beg (max (point-at-bol -100) head-end)))
                (append-to-buffer compute-buf tail-beg (point-max))))
            (setq mtime (visited-file-modtime)))))
      (setq smart-log--plist
            (list :mtime
                  (float-time mtime)
                  :base-year
                  (smart-log--extract-year mtime)))
      (smart-log--compute-formatter))))

;;;
;;; UI
;;;

;;
;; font lock
;;

(defface smart-log-time-face
  '((t :inherit font-lock-constant-face))
  "Face for logged time which is interpreted by smart-log algorithm.")

(defface smart-log-paging-face
  '((t :inherit font-lock-variable-name-face))
  "Face for region of showing the buffer.")

(defface smart-log-file-size-face
  '((t :inherit font-lock-keyword-face))
  "Face for region of showing the buffer.")

(defface smart-log-error-face
  '((t :inherit font-lock-warning-face))
  "Face for guessed error log.")

(defface smart-log-debug-face
  '((t :inherit shadow))
  "Face for guessed debug log.")

(defvar smart-log-error-face 'smart-log-error-face)
(defvar smart-log-debug-face 'smart-log-debug-face)

(defvar smart-log-font-lock-keywords
  (eval-when-compile
    `((
       ,(concat "\\b"
                (regexp-opt '("error" "warning" "warn" "caution" "fatal") t)
                "\\b")
       0 smart-log-error-face)
      ("\\bdebug\\b" 0 smart-log-debug-face))))

;;
;; Customize
;;

(defcustom smart-log-display-format "%Y-%m-%d %H:%M:%S"
  "Log displaying format.
This option is passed to `format-time-string'."
  :group 'smart-log
  :type 'string)

(defcustom smart-log-display-milliseconds nil
  "Display milliseconds(or nanoseconds) if log format support this."
  :group 'smart-log
  :type 'boolean)

(defcustom smart-log-mode-hook nil
  "Run end of `smart-log-mode'."
  :group 'smart-log
  :type 'boolean)

(defcustom smart-log-auto-revert-tail nil
  "Run `turn-on-auto-revert-tail-mode' when `smart-log-mode' is on."
  :group 'smart-log
  :type 'boolean)

(defcustom smart-log-paging-chunk-size 5000000
  "Chunk size bytes when `smart-log-chunk-mode' is on."
  :group 'smart-log
  :type 'integer)

;;
;; Format
;;

(if (fboundp 'file-size-human-readable)
    (defun smart-log--format-filesize (file-size)
      (file-size-human-readable file-size))

  ;; From Emacs 24
  (defun smart-log--format-filesize (file-size)
    (let ((power 1024.0)
          (post-fixes
           ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
           (list "" "k" "M" "G" "T" "P" "E" "Z" "Y")))
      (while (and (>= file-size power) (cdr post-fixes))
        (setq file-size (/ file-size power)
              post-fixes (cdr post-fixes)))
      (format (if (> (mod file-size 1.0) 0.05)
                  "%.1f"
                "%.0f")
              file-size))))

;;
;; Mode specific
;;

(defvar smart-log--mode-line
  '("Log"
    (:eval
     (or (let ((type (plist-get smart-log--plist :name)))
           (and type
                (propertize
                 (format " [%s]" type)
                 'face 'smart-log-time-face)))
         ""))
    (:eval
     (or (let ((range (plist-get smart-log--plist :paging)))
           (and range
                (propertize
                 (format " %s - %s"
                         (or (and (car range)
                                  (smart-log--format-filesize (car range)))
                             "")
                         (or (and (cdr range)
                                  (smart-log--format-filesize (cdr range)))
                             ""))
                 'face 'smart-log-paging-face)))
         ""))
    (:eval
     (let (info)
       (cond
        ((not buffer-file-name))
        ((setq info (jka-compr-get-compression-info buffer-file-name))
         (let ((jka-prog (jka-compr-info-compress-program info)))
           (propertize (format ":%s" jka-prog)
                       'face 'smart-log-file-size-face)))
        (t
         (let* ((attr (file-attributes buffer-file-name))
                (size (nth 7 attr)))
           (propertize
            (format ":%s"
                    (smart-log--format-filesize size))
            'face 'smart-log-file-size-face))))))))

(defvar smart-log-mode-map nil
  "Keymap for smart-log mode.")

(unless smart-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map view-mode-map)

    (define-key map "\C-c\C-t" 'smart-log-toggle-time-format)

    (setq smart-log-mode-map map)))

(defvar smart-log--formatted-region nil)
(make-variable-buffer-local 'smart-log--formatted-region)

;;
;; internal
;;

(defvar smart-log--show-format nil)
(make-variable-buffer-local 'smart-log--show-format)

(defun smart-log--overlay-at (point)
  (catch 'return
    (dolist (o (overlays-at point))
      (when (overlay-get o 'smart-log-time)
        (throw 'return o)))))

(defun smart-log--format-current-line (sample-fn convert-fn)
  (let ((region (funcall sample-fn)))
    (when region
      (unless (smart-log--overlay-at (car region))
        (condition-case nil
            (let* ((start (car region))
                   (end (cdr region))
                   (str (buffer-substring start end))
                   (date (funcall convert-fn str))
                   (ov (make-overlay start end))
                   (text (smart-log--format-time date)))
              (overlay-put ov 'smart-log-time t)
              (if smart-log--show-format
                  (overlay-put ov 'display text)
                (overlay-put ov 'smart-log-hiden-display text))
              (overlay-put ov 'face 'smart-log-time-face))
          (error nil)))))
  (forward-line 1))

(defun smart-log--find-bol (file maybe-start)
  (catch 'done
    (when (= maybe-start 0)
      (throw 'done 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary))
        (let ((start (- maybe-start 1))
              ;; FIXME: may misunderstand eol-style if
              ;; there is too long line.
              (end (+ maybe-start 1024))
              goto-bol)
          (insert-file-contents file nil start end)
          (forward-char)
          (cond
           ;; FIXME: mac eol-style
           ((not (string-match "\n" (buffer-string)))
            ;; guessed as mac eol style
            (when (eq (char-after) '?\r)
              (throw 'done maybe-start))
            (setq goto-bol (lambda ()
                             (and (re-search-backward "\r" nil t)
                                  (progn (forward-char) t)))))
           ((eolp)
            (throw 'done maybe-start))
           (t
            (setq goto-bol (lambda ()
                             (and (re-search-backward "\n" nil t)
                                  (progn (forward-char) t))))))
          (setq end start)
          (setq start (max (- end 256) 0))
          (while (> end 0)
            (insert-file-contents file nil start end)
            ;; goto end of insert. buffer `point' is start from 1
            (goto-char (1+ (- end start)))
            (when (funcall goto-bol)
              (throw 'done (+ start (1- (point)))))
            (goto-char (point-min))
            (setq start (- start 256))
            (setq end (- end 256))))
        0))))

;; Emacs lowlevel api `insert-file-contents' return char
;; count of inserted. not bytes. To handle as byte count
;; define such pretty complex procedure
;; START allow negative value like `substring' from EOF
;;   positive value must be a bol of file
(defun smart-log--load-log (start &optional maybe-end)
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)
         (tramp-cache-inhibit-cache t)
         (rawfile (smart-log--filter-physical-file buffer-file-name))
         (attr (file-attributes rawfile))
         (modtime (nth 5 attr))
         (log-size (nth 7 attr))
         (buf (current-buffer))
         (multibytep enable-multibyte-characters)
         (end
          (cond
           ((not maybe-end)
            log-size)
           (t
            (min maybe-end log-size))))
         ;; suppress visiting file warnings
         (buffer-file-name nil)
         (coding-system buffer-file-coding-system))
    (when (< start 0)
      (let* ((maybe-start (max (min (+ log-size start) end) 0))
             (bol-start (smart-log--find-bol rawfile maybe-start)))
        (setq start bol-start)))
    (cond
     ((> start end)
      (signal 'args-out-of-range (list start end)))
     (t
      (let ((inhibit-read-only t))
        (erase-buffer))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'binary))
          ;; `insert-file-contents' BEG start from 0 to END by byte.
          ;; return value is inserted char, not byte.
          (insert-file-contents rawfile nil start end))
        (goto-char (point-max))
        ;; FIXME detecting eol stype ugly way
        (cond
         ;; first, search LF in enough size of bytes
         ((or (re-search-backward "\n" nil t)
              ;; search CR, this case should be mac EOL style
              (re-search-backward "\r" nil t))
          (let ((decrease (- (point-max) (1+ (point)))))
            (delete-region (1+ (point)) (point-max))
            (setq end (- end decrease)))))
        (when multibytep
          (set-buffer-multibyte t)
          (decode-coding-region (point-min) (point-max) coding-system))
        (append-to-buffer buf (point-min) (point-max)))))
    (set-visited-file-modtime modtime)
    (set-buffer-modified-p nil)
    (plist-put smart-log--plist :mtime (float-time modtime))
    (let ((range (plist-get smart-log--plist :paging)))
      (setcar range start)
      (setcdr range end))))

(defun smart-log--clear-display ()
  (remove-overlays nil nil 'smart-log-time t)
  (setq smart-log--formatted-region nil))

(defun smart-log--cleanup ()
  (loop for b in (buffer-list)
        if (and (not (eq b (current-buffer)))
                (eq (buffer-local-value 'major-mode b) 'smart-log-mode))
        return nil
        finally (progn
                  ;; kill timer if smart-log-mode buffer is not exists.
                  (cancel-function-timers 'smart-log--delayed-format))))

(defun smart-log--buffer-byte-size ()
  (if buffer-file-coding-system
      (length (encode-coding-string
               (buffer-string)
               buffer-file-coding-system))
    (buffer-size)))

(defun smart-log--prepare-file-plist (file)
  (let* ((attr (file-attributes file))
         (mtime (nth 5 attr))
         (year (smart-log--extract-year (nth 5 attr)))
         (size (smart-log--buffer-byte-size)))
    (setq smart-log--plist
          (append
           (list :mtime (float-time mtime)
                 :base-year year
                 :paging (cons 0 size))
           (smart-log--compute-from-buffer (current-buffer))))))

(defun smart-log-merge-properties (prop1 prop2)
  (loop with prop = (copy-sequence prop1)
        for ps on prop2 by (lambda (x) (cddr x))
        do (let ((name (car ps))
                 (value (cadr ps)))
             (plist-put prop name value))
        finally return prop))

;; control display format all of `smart-log' buffer
(defun smart-log--delayed-format ()
  (with-local-quit
    (when (eq major-mode 'smart-log-mode)
      (let ((region (cons (window-start) (window-end))))
        (unless (equal smart-log--formatted-region region)
          (let* ((plist smart-log--plist)
                 (sample-fn (plist-get plist :sample-fn))
                 (convert-fn (plist-get plist :convert-fn)))
            (when (and sample-fn convert-fn)
              (save-excursion
                (goto-char (car region))
                (while (and (<= (point) (cdr region))
                            (not (eobp)))
                  (smart-log--format-current-line sample-fn convert-fn))))
            (setq smart-log--formatted-region region)))))))

(defun smart-log--format-time (date)
  (concat
   (format-time-string smart-log-display-format date)
   (and smart-log-display-milliseconds
        (let ((mhigh (caddr date))
              (mlow (cadddr date)))
          (cond
           ((or mhigh mlow)
            (format ".%09.0f" (+ (* (ftruncate (or mhigh 0)) ?\x10000)
                                 (or mlow 0)))))))))

(defadvice auto-revert-tail-handler
    (after smart-log-auto-revert-tail () activate)
  (ignore-errors
    (when (derived-mode-p 'smart-log-mode)
      (plist-put smart-log--plist :mtime (float-time (visited-file-modtime)))
      (let ((range (plist-get smart-log--plist :paging)))
        (setcdr range auto-revert-tail-pos)))))

;; for revert-buffer--default
(defun smart-log--after-revert-function ()
  (smart-log--clear-display)
  (plist-put smart-log--plist :mtime (float-time (visited-file-modtime)))
  (let ((range (plist-get smart-log--plist :paging)))
    (setcdr range (smart-log--buffer-byte-size))))

;;
;; Command
;;

(defvar smart-log-chunk-mode-map nil)
(unless smart-log-chunk-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\C-c\C-b" 'smart-log-backward-page)
    (define-key map "\C-c\C-f" 'smart-log-forward-page)

    (setq smart-log-chunk-mode-map map)))

(defun smart-log-chunk-mode-inhibit-write ()
  (error "This buffer is chunked"))

(define-minor-mode smart-log-chunk-mode
  "Chunked log file to see huge log file. Do not call manually this mode."
  nil "[Chunked]" smart-log-chunk-mode-map
  (cond
   ((not (derived-mode-p 'smart-log-mode))
    (message "Not a valid major-mode.")
    (smart-log-chunk-mode -1))
   ((not smart-log-chunk-mode)
    (remove-hook 'write-contents-functions
                 'smart-log-chunk-mode-inhibit-write t)
    ;; show all file contents
    (let ((file buffer-file-name))
      (kill-buffer (current-buffer))
      (find-file file)))
   (t
    (add-hook 'write-contents-functions
              'smart-log-chunk-mode-inhibit-write nil t))))

(defun smart-log-forward-page ()
  "Forward chunked page follow `smart-log-paging-chunk-size'."
  (interactive)
  (let* ((range (plist-get smart-log--plist :paging))
         (start (cdr range))
         (end (+ (cdr range) smart-log-paging-chunk-size)))
    (smart-log--load-log start end)))

(defun smart-log-backward-page ()
  "Backward chunked page follow `smart-log-paging-chunk-size'."
  (interactive)
  (let* ((range (plist-get smart-log--plist :paging))
         (end (car range))
         (may-begin (- (car range) smart-log-paging-chunk-size))
         (file buffer-file-name)
         (start (smart-log--find-bol file (max may-begin 0))))
    (if (<= end start)
        (message "No more previous page")
      (smart-log--load-log start end))))

;;;###autoload
(defun smart-log-find-file (file &optional from-front)
  "Open huge log FILE with chunked. See `smart-log-chunk-mode'
If optional arg FROM-FRONT non-nil means open log from beginning of file."
  (interactive "fLog File: \nP")
  (unless (file-exists-p file)
    (error "Not a log file"))
  (let ((buffer (get-file-buffer file)))
    (unless buffer
      (setq buffer (create-file-buffer file))
      (let* ((coding-system (smart-log--detect-coding-system file))
             (attr (file-attributes file))
             (modtime (nth 5 attr)))
        (with-current-buffer buffer
          (let ((create-lockfiles nil))
            (set-visited-file-name file))
          (set-visited-file-modtime modtime)
          (set-buffer-file-coding-system coding-system)
          (smart-log-mode)
          (cond
           (from-front
            (smart-log--load-log 0 smart-log-paging-chunk-size))
           (t
            (smart-log--load-log (- smart-log-paging-chunk-size))))
          (setq smart-log--plist
                (append
                 smart-log--plist
                 (smart-log--compute-from-buffer (current-buffer))))
          (smart-log-chunk-mode 1))))
    (switch-to-buffer buffer)))

;;;###autoload
(define-derived-mode smart-log-mode nil
  nil
  "View log file mode with intepretting miscellaneous time format."
  (if (not (and buffer-file-name
                (file-exists-p buffer-file-name)))
      ;; when opening 0 byte file temporarily
      ;; fallback to basic mode
      (fundamental-mode)
    (view-mode 1)
    (setq mode-name smart-log--mode-line)
    (set (make-local-variable 'font-lock-defaults)
         '(smart-log-font-lock-keywords t t nil beginning-of-line))
    (set (make-local-variable 'revert-buffer-function)
         'smart-log-revert-buffer)
    (setq smart-log--show-format t)
    (add-hook 'kill-buffer-hook
              'smart-log--cleanup nil t)
    ;; control default revert-buffer-function
    (add-hook 'after-revert-hook
              'smart-log--after-revert-function nil t)
    (setq buffer-read-only t)
    (smart-log--prepare-file-plist buffer-file-name)
    (when smart-log-auto-revert-tail
      (auto-revert-tail-mode 1))
    (cancel-function-timers 'smart-log--delayed-format)
    ;; delay after 0.5 second
    ;; Just after `find-file', `window-start' and `window-end' point to
    ;; wrong region.
    (run-with-timer 0.5 0.5 'smart-log--delayed-format)))

(defun smart-log-revert-buffer (_ignore _noconfirm)
  "Revert all of log entries."
  (interactive)
  (let ((line (line-number-at-pos (point))))
    (smart-log--load-log 0)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun smart-log-switch-format ()
  "Switch logging time format manually."
  (interactive)
  (let* ((source (mapcar
                  (lambda (x)
                    (prin1-to-string (plist-get x :name)))
                  smart-log--unformatters))
         (key (let ((completion-ignore-case t))
                (completing-read "Format: " source nil t)))
         (fmtr (loop with key = (intern key)
                     for fmtr in smart-log--unformatters
                     if (eq (plist-get fmtr :name) key)
                     return fmtr)))
    (setq smart-log--plist
          (smart-log-merge-properties smart-log--plist fmtr))
    (smart-log--clear-display)))

(defun smart-log-toggle-time-format ()
  "Toggle between displaying time format or raw text."
  (interactive)
  (setq smart-log--show-format (not smart-log--show-format))
  (destructuring-bind (new . old)
      (if smart-log--show-format
          `(display . smart-log-hiden-display)
        `(smart-log-hiden-display . display))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((disp (overlay-get ov old)))
        (and disp
             (overlay-put ov new disp)))
      (overlay-put ov old nil))))

;;;
;;; Unload
;;;

(defun smart-log-unload-function ()
  (cancel-function-timers 'smart-log--delayed-format)
  (loop for (func class name) in
        '((auto-revert-tail-handler after smart-log-auto-revert-tail))
        do (progn
             (ad-disable-advice func class name)
             (ad-update func))))

;;;; Add general log filenames for `package'
;;;###autoload(add-to-list 'auto-mode-alist `("/@[a-f0-9]\\{24\\}\\.[su]\\'" . smart-log-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.log\\(?:\\.[0-9]+\\)?\\(?:\\.\\(?:gz\\|bz2\\|xz\\)\\)?\\'" . smart-log-mode))

(provide 'smart-log)

;;; smart-log.el ends here
