;;; smart-log.el --- Major mode with intepretting miscellaneous time format in log file.

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: applications, development
;; URL: https://github.com/mhayashi1120/smart-log.el/raw/master/smart-log.el
;; Emacs: todo GNU Emacs 22 or later
;; Version: 0.0.0
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

;; * Put the following to your .emacs
;;
;;   (autoload 'smart-log-mode "smart-log"
;;             "Major mode with intepretting miscellaneous time format in log file" t)

;; * If you want to revert automatically, put the following to your .emacs
;;
;;   (add-hook 'smart-log-mode-hook 'turn-on-auto-revert-tail-mode)

;; * If you want open log file automatically `smart-log-mode', put the followings.
;;
;;   ;; System log directory (on Debian)
;;   (add-to-list 'auto-mode-alist `("/var/log/" . smart-log-mode))
;;   ;; djb tool
;;   (add-to-list 'auto-mode-alist
;;                `("/@[a-f0-9]\\{24\\}\\.[su]\\'" . smart-log-mode))
;;   ;; General log filename and logrotated files.
;;   (add-to-list 'auto-mode-alist
;;                '("\\.log\\(?:\\.[0-9]+\\)?\\(?:\\.\\(?:gz\\|bz2\\|xz\\)\\)?\\'"
;;                  . smart-log-mode))



;;; TODO:
;; * grep with regexp or time range
;; * too many log entry is appended slow down emacs, or crash..
;; * (visited-file-modtime)
;; * key of toggle auto read
;; * smart-log-find-file
;;   handling huge log file
;; * inhibit save-buffer when restricted mode (huge file narrowing)
;; * disable auto-tail-revert if :paging prop is not indicate max byte

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup smart-log ()
  "Smart log viewer."
  :group 'applications)

(require 'parse-time)
(require 'view)

;;;
;;;
;;;

(defun smart-log--format-filesize (size)
  (let ((s size)
        (f (ftruncate size))
        (units '(B KB MB GB TB EB)))
    (while (> s 1024)
      (setq f (/ (ftruncate s) 1024))
      (setq s (lsh s -10))
      (setq units (cdr units)))
    (if (eq (car units) 'B)
        (format "%dB" s)
      (format "%0.1f%s" f (car units)))))

;;;
;;; Datetime deformat
;;;

(defvar smart-log--unformatters
  '(
    ;; djb tools (@400000004f90304a3abfd92c => 2012-04-20 00:33:20)
    (:name tai64n :sample-fn smart-log-tai64n
           :convert-fn smart-log-tai64n->date)
    ;; (May 20 00:05:02 => 2012-05-20 00:05:02)
    ;; TODO this format may indicate wrong year.
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
:sample-fn is called at beginning of line and return region of logged time in buffer.
:convert-fn accept one string arg and return `current-date' format.
")

;;
;; rfc822
;;

(defconst smart-log-rfc822-time-regexp
  (eval-when-compile
    (let* ((ms (loop repeat 12 for m in parse-time-months collect (car m)))
           (ws (loop repeat 7 for m in parse-time-weekdays collect (car m)))
           (mr (regexp-opt ms))
           (wr (regexp-opt ws))
           (2d "[0-9]\\{1,2\\}")
           (regexp
            ;; ignore some line header characters.
            ;; e.g. Nov  9 01:31:08
            (concat
             "^"
             ;; skip max 16 chars
             ".\\{,16\\}"
             ;; day of the week
             "\\(" wr ", +\\)?"
             ;; month name
             mr
             " +"
             ;; days of month
             2d
             " "
             ;; time part
             2d ":" 2d ":" 2d)))
      regexp)))

(defun smart-log-rfc822-time ()
  (and (looking-at smart-log-rfc822-time-regexp)
       (cons (match-beginning 0) (match-end 0))))

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

(defconst smart-log-general-date-regexp
  (eval-when-compile
    (let ((2d "[0-9]\\{1,2\\}"))
      (concat
       ;; skip 16 chars
       ".\\{,16\\}"
       ;; date part
       "\\([0-9]\\{4\\}\\)[-/.]\\(" 2d "\\)[-/.]\\(" 2d "\\)"
       "[ \t]+"
       ;; time part
       "\\(" 2d "\\):\\(" 2d "\\)\\(?::\\(" 2d "\\)\\)?"
       "\\(?:[:.]\\([0-9]+\\)\\)?"))))

(defun smart-log-general-date ()
  (and (looking-at smart-log-general-date-regexp)
       (cons (match-beginning 0) (match-end 0))))

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

;; http://www.kanzaki.com/docs/html/dtf.html
(defconst smart-log-general-locale-date-regexp
  (eval-when-compile
    (let ((2d "[0-9]\\{1,2\\}"))
      (concat
       ;; skip 16 chars
       ".\\{,16\\}"
       ;; date part
       "\\([0-9]\\{2,4\\}\\)[-/.]\\(" 2d "\\)[-/.]\\([0-9]\\{2,4\\}\\)"
       "[ \t]+"
       ;; time part
       "\\(" 2d "\\):\\(" 2d "\\)\\(?::\\(" 2d "\\)\\)?"
       "\\(?:[:.]\\([0-9]+\\)\\)?"))))

(defun smart-log-general-locale-date ()
  (and (looking-at smart-log-general-locale-date-regexp)
       (cons (match-beginning 0) (match-end 0))))

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
    (concat "\\["
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

(defun smart-log-clf-date ()
  ;;FIXME regexp is not correct...
  (and (looking-at ".\\{,64\\}\\[\\([0-9]\\{1,2\\}/.../[0-9]\\{4\\}\\(?::[0-9]\\{2\\}\\)\\{3\\}.*?\\)\\]")
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
  (loop for fmtr in smart-log--unformatters
        if (funcall (plist-get fmtr :sample-fn))
        collect fmtr))

(defun smart-log--score-formats (formatters)
  (loop for fmtr in formatters
        collect
        (let ((score (smart-log--score-formatter fmtr)))
          (cons score fmtr))))

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
    (let ((diff (abs (- (float-time last) mtime))))
      (push (max (- 1 (expt (/ diff 120.0) 2)) 0.5) scores))
    ;;TODO 3. check order of log entry times.
    ;; 2. valid log line approprivate count.
    (push (/ valid (ftruncate all)) scores)
    (apply '* scores)))

;; `smart-log' guess time format by following assumption:
;; 1. log file MUST start with LOG line.
;;    that line MUST have time string in first 1024 bytes.
;; 2. lines should have valid date appropriate count.
;; 3. lines have been nearly sorted by date.
;;    log time format may be deficit year 
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
    (let* ((attr (file-attributes file))
           (size (nth 7 attr))
           (mtime (nth 5 attr))
           (head-end 2048)
           (tail-beg (max (- size 4096) head-end)))
      (insert-file-contents file nil 0 head-end)
      (goto-char (point-max))
      (delete-region (point-at-bol) (point-max))
      (when (> size tail-beg)
        (insert-file-contents file nil tail-beg size))
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
      (setq smart-log--plist (list :mtime (float-time mtime)))
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
  "Run end of `smart-log-mode'.

\(add-hook 'smart-log-mode-hook 'turn-on-auto-revert-tail-mode)
 TODO or create some option? only last page is valid..
"
  :group 'smart-log
  :type 'boolean)

;;TODO default value
(defcustom smart-log-paging-chunk-size 500
  "Chunk size bytes when `smart-log-chunk-mode' is on."
  :group 'smart-log
  :type 'integer)

;;
;; Mode specific
;;

(defvar smart-log--mode-line
  '("Log"
    (:eval (or (let ((type (plist-get smart-log--plist :name)))
                 (and type
                      (propertize
                       (format " [%s]" type)
                       'face 'smart-log-time-face)))
               ""))
    (:eval (or (let ((range (plist-get smart-log--plist :paging)))
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
    (:eval (and buffer-file-name
                (let* ((attr (file-attributes buffer-file-name))
                       (size (nth 7 attr)))
                  (propertize
                   (format ":%s"
                           (smart-log--format-filesize size))
                   'face 'smart-log-file-size-face))))))

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
  (loop for o in (overlays-at point)
        if (overlay-get o 'smart-log-time)
        return o))

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

;;TODO dos/unix/mac
;;TODO add test
(defun smart-log--find-bol (file maybe-start)
  (catch 'done
    (when (= maybe-start 0)
      (throw 'done 0))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary))
        (let ((start (- maybe-start 1))
              (end (+ maybe-start 16)))
          (insert-file-contents file nil start end)
          (when (eolp)
            (throw 'done maybe-start))
          (setq end start)
          (setq start (max (- end 256) 0))
          (while (> end 0)
            (insert-file-contents file nil start end)
            (goto-char (- end start))
            (when (/= (point-at-bol) (point-min))
              (throw 'done (+ start (1- (point-at-bol)))))
            (goto-char (point-min))
            (setq start (- start 256))
            (setq end (- end 256))))
        0))))

;;start must be a bol
(defun smart-log--read-file (start &optional maybe-end replace)
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)
         (tramp-cache-inhibit-cache t)
         (file buffer-file-name)
         (attr (file-attributes file))
         (modtime (nth 5 attr))
         (buf (current-buffer))
         (end
          (if maybe-end
              (min maybe-end (nth 7 attr))
            (nth 7 attr)))
         ;; suppress visiting file warnings
         (buffer-file-name nil)
         (coding-system buffer-file-coding-system))
    (cond
     ((< start 0)
      (signal 'args-out-of-range (list start)))
     ((> start end)
      (signal 'args-out-of-range (list start end)))
     ((= start end))                    ; simply ignore
     (t
      (when replace
        (let ((inhibit-read-only t))
          (erase-buffer)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'binary))
          ;; `insert-file-contents' BEG start from 0 to END by byte.
          ;; return value is inserted char, not byte.
          (insert-file-contents file nil start end))
        (goto-char (point-max))
        (unless (bolp)
          (let ((decrease (- (point-max) (point-at-bol))))
            (delete-region (point-at-bol) (point-max))
            (setq end (- end decrease))))
        (decode-coding-region (point-min) (point-max) coding-system)
        (append-to-buffer buf (point-min) (point-max)))))
    (set-visited-file-modtime modtime)
    (plist-put smart-log--plist :mtime (float-time modtime))
    (let ((range (plist-get smart-log--plist :paging)))
      (setcar range start)
      (setcdr range end))))

(defun smart-log-redisplay-time ()
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

(defvar smart-log--plist nil)
(make-variable-buffer-local 'smart-log--plist)

(defun smart-log--prepare-file-plist (file)
  (let* ((attr (file-attributes file))
         (mtime (nth 5 attr))
         (year (nth 5 (decode-time (nth 5 attr)))))
    (setq smart-log--plist
          (append
           (list :mtime (float-time mtime)
                 :base-year year
                 :paging (cons 0 nil))
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

;;TODO unadvice when unload-feature
(defadvice auto-revert-tail-handler
    (after smart-log-auto-revert-tail () activate)
  (ignore-errors
    (when (derived-mode-p 'smart-log-mode)
      (plist-put smart-log--plist :mtime (float-time (visited-file-modtime)))
      (let ((range (plist-get smart-log--plist :paging)))
        (setcdr range auto-revert-tail-pos)))))

;;
;; Command
;;

(defvar smart-log-chunk-mode-map nil)
(unless smart-log-chunk-mode-map
  (let ((map (make-sparse-keymap)))
    
    (define-key map "\C-c\C-b" 'smart-log-backward-page)
    (define-key map "\C-c\C-f" 'smart-log-forward-page)
    
    (setq smart-log-chunk-mode-map map)))

(define-minor-mode smart-log-chunk-mode
  "" nil "[Chunked]" smart-log-chunk-mode-map
  (cond
   ((not (derived-mode-p 'smart-log-mode))
    (smart-log-chunk-mode -1))
   (smart-log-chunk-mode
    ;;TODO setcdr range
    )
   (t
    )))

;;TODO open file  -> smart-log-mode auto-mode -> on chunk-mode range cdr is null
(defun smart-log-forward-page ()
  "Forward chunked page follow `smart-log-paging-chunk-size'."
  (interactive)
  (let* ((range (plist-get smart-log--plist :paging))
         (start (cdr range))
         (end (+ (cdr range) smart-log-paging-chunk-size)))
    (smart-log--read-file start end t)))

(defun smart-log-backward-page ()
  "Backward chunked page follow `smart-log-paging-chunk-size'."
  (interactive)
  (let* ((range (plist-get smart-log--plist :paging))
         (end (1- (car range)))
         (may-begin (- (car range) smart-log-paging-chunk-size))
         (file buffer-file-name)
         (start (smart-log--find-bol file (max may-begin 0))))
    (if (<= end start)
        (message "No more previous page")
      (smart-log--read-file start end t))))

;;;###autoload
(defun smart-log-find-file (file)
  (interactive "fLog File: ")
  (unless (file-exists-p file)
    (error "Not a log file"))
  (let ((buffer (get-file-buffer file)))
    (unless buffer
      (setq buffer (create-file-buffer file))
      ;;TODO
      (let* ((coding-system 'utf-8)
             (attr (file-attributes file))
             (modtime (nth 5 attr)))
        (with-current-buffer buffer
          (set-visited-file-name file)
          (set-visited-file-modtime modtime)
          (smart-log-mode)
          (set-buffer-file-coding-system coding-system)
          (smart-log--read-file 0 smart-log-paging-chunk-size t)
          (smart-log-chunk-mode 1)
          (set-buffer-modified-p nil))))
    (switch-to-buffer buffer)))

;;;###autoload
(define-derived-mode smart-log-mode nil
  nil
  "Major mode to view log file"
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
    (setq buffer-read-only t)
    (smart-log--prepare-file-plist buffer-file-name)
    (cancel-function-timers 'smart-log--delayed-format)
    ;; delay after 0.5 second
    ;; Just after `find-file', `window-start' and `window-end' point to
    ;; wrong region.
    (run-with-timer 0.5 0.5 'smart-log--delayed-format)))

(defun smart-log-revert-buffer (_ignore _noconfirm)
  "Revert all of log entries."
  (interactive)
  (let ((line (line-number-at-pos (point))))
    (smart-log--read-file 0 nil t)
    (goto-char (point-min))
    (forward-line (1- line))
    (set-buffer-modified-p nil)))

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
    (smart-log-redisplay-time)))

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

(provide 'smart-log)

;;; smart-log.el ends here
