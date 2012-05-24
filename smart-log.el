;;; smart-log.el --- smart log mode with intepretting miscellaneous format

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: log
;; URL: todo http://github.com/mhayashi1120/smart-log.el/raw/master/smart-log.el
;; Emacs: todo GNU Emacs 22 or later
;; Version: 0.0.0

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

;;; TODO:
;; * grep with regexp or time range
;; * auto sync mode?
;; * too many log entry is appended slow down emacs, or crash..
;; * (visited-file-modtime)
;; * key of toggle auto read
;; * smart-log-open-file
;;   handling huge log file
;; * autoload

;; sample of settings `auto-mode-alist'

(eval-when-compile
  (require 'cl))

(require 'parse-time)

(defgroup smart-log ()
  "Smart log viewer."
  :group 'applications)

(defface smart-log-time-face
  '((t :inherit font-lock-constant-face))
  "Face for logged time which is interpreted by smart-log algorithm.")

(defface smart-log-error-face
  '((t :inherit font-lock-warning-face))
  "Face for guessed error log.")

(defface smart-log-debug-face
  '((t :inherit shadow))
  "Face for guessed debug log.")

(defcustom smart-log-display-format "%Y-%m-%d %H:%M:%S"
  "Log displaying format.
This option is passed to `format-time-string'."
  :group 'smart-log
  :type 'string)

(defcustom smart-log-display-milliseconds nil
  "Display milliseconds(or nanoseconds) if log format support this."
  :group 'smart-log
  :type 'boolean)

(defvar smart-log--plist nil)
(make-variable-buffer-local 'smart-log--plist)

(defvar smart-log--show-format nil)
(make-variable-buffer-local 'smart-log--show-format)

(defvar smart-log--buffer-bytes nil)
(make-variable-buffer-local 'smart-log--buffer-bytes)

(defvar smart-log-mode-map nil)

(unless smart-log-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "g" 'revert-buffer)
    (define-key map "\C-c\C-t" 'smart-log-toggle-time-format)

    (setq smart-log-mode-map map)))

;;;###autoload
(defun smart-log-mode ()
  "Major mode to view log file"
  (interactive)
  (unless (and buffer-file-name
               (file-exists-p buffer-file-name))
    (error "Invalid log file"))
  (kill-all-local-variables)
  (setq major-mode 'smart-log-mode)
  (use-local-map smart-log-mode-map)
  (setq mode-name smart-log--mode-line)
  ;; font lock
  (font-lock-add-keywords nil smart-log-font-lock-keywords t)
  (setq font-lock-keywords-case-fold-search t)
  (set (make-local-variable 'font-lock-keywords-only) t)
  (setq revert-buffer-function 'smart-log-revert-buffer)
  (set (make-local-variable 'isearch-search-fun-function)
       'smart-log--isearch-function)
  (setq smart-log--show-format t)
  (add-hook 'kill-buffer-hook
            'smart-log--cleanup nil t)
  (view-mode 1)
  (setq buffer-read-only t)
  (smart-log--auto-read-start)
  (cancel-function-timers 'smart-log--delayed-format)
  ;; TODO delay after 0.5 second
  ;;  first time `find-file', `window-start' and `window-end' point
  ;;  to wrong region.
  (run-with-timer 0.5 0.5 'smart-log--delayed-format))

(defvar smart-log--mode-line
  '("Smart Log"
    (:eval (or (let ((type (plist-get smart-log--plist :name)))
                 (and type (format " [%s]" type)))
               ""))))

(defun smart-log-revert-buffer (&optional ignore-auto noconfirm)
  (let ((max (save-excursion
               (save-restriction
                 (widen)
                 (point-max)))))
    (let (revert-buffer-function)
      (revert-buffer nil t t))
    (setq smart-log--buffer-bytes nil)
    (view-mode 1)
    (setq buffer-read-only t)))

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

(defvar smart-log--read-timer nil)

(defun smart-log--cleanup ()
  (loop for b in (buffer-list)
        if (and (not (eq b (current-buffer)))
                (eq (buffer-local-value 'major-mode b) 'smart-log-mode))
        return nil
        finally (progn
                  ;; kill timers if smart-log-mode buffer is not exists.
                  (when smart-log--read-timer
                    (cancel-timer smart-log--read-timer)
                    (setq smart-log--read-timer nil))
                  (cancel-function-timers 'smart-log--delayed-format))))

(defun smart-log--auto-read-start ()
  (unless smart-log--read-timer
    (setq smart-log--read-timer
          (run-with-timer
           1 1 'smart-log--auto-read-timer)))
  (timer-activate smart-log--read-timer))

(defun smart-log--auto-read-timer ()
  (with-local-quit
    (when (eq major-mode 'smart-log-mode)
      (unless (file-remote-p buffer-file-name)
        (smart-log-read-new-entries)))))

(defun smart-log-read-new-entries ()
  (let* ((cs buffer-file-coding-system)
         (next (or smart-log--buffer-bytes
                   (length (encode-coding-region
                            (point-min) (point-max) cs t)))))
    (setq smart-log--buffer-bytes next)
    (let* ((attr (file-attributes buffer-file-name))
           (size (nth 7 attr))
           (eobp (eobp)))
      (when (> size next)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-max))
            (let* ((coding-system-for-read cs)
                   (inhibit-read-only t)
                   (point (point-max))
                   (flag (buffer-modified-p)))
              (insert-file-contents buffer-file-name nil next size)
              (setq smart-log--buffer-bytes size)
              (set-buffer-modified-p flag))))
        (when eobp
          (goto-char (point-max)))))))

(defvar smart-log--unformatters
  '(
    ;; djb tools
    (:name tai64n :sample-fn smart-log-tai64n
           :convert-fn smart-log-tai64n->date)
    ;;TODO
    (:name rfctime :sample-fn smart-log-rfc822-time
           :convert-fn smart-log-rfc822-time->date)
    (:name general-jp :sample-fn smart-log-general-locale-date
           :convert-fn smart-log-general-locale-jp->date)
    (:name general-en :sample-fn smart-log-general-locale-date
           :convert-fn smart-log-general-locale-en->date)
    (:name general-us :sample-fn smart-log-general-locale-date
           :convert-fn smart-log-general-locale-us->date)
    (:name general :sample-fn smart-log-general-date
           :convert-fn smart-log-general-date->date)
    ;; apache ...
    (:name CLF :sample-fn smart-log-clf-date
           :convert-fn smart-log-clf-date->date)
    (:name apache-error-log :sample-fn smart-log-apache-error-log
           :convert-fn smart-log-apache-error-log->date)
    ;; squid ...
    (:name epoch :sample-fn smart-log-epoch
           :convert-fn smart-log-epoch->date)
    )
  "Considerable log format alist each item has (NAME REGION-FN TO-DATE-FN)
NAME
:sample-fn is called at beginning of line and return region of logged time in buffer.
:convert-fn accept one string arg and return `current-date' format.
")

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
              ;;TODO no meaning?
              (overlay-put ov 'isearch-open-invisible t)
              (if smart-log--show-format
                  (overlay-put ov 'display text)
                (overlay-put ov 'smart-log-hiden-display text))
              (overlay-put ov 'face 'smart-log-time-face))
          (error nil)))))
  (forward-line 1))

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

(defun smart-log--prepare-file-plist (file)
  (let* ((attr (file-attributes file))
         (mtime (float-time (nth 5 attr)))
         (year (format-time-string "%Y" (nth 5 attr))))
    (setq smart-log--plist
          ;;TODO update mtime after revert or read new-entries
          (list :mtime mtime
                :year (string-to-number year)))))

;;;
;;; font lock
;;;

(defvar smart-log-error-face 'smart-log-error-face)
(defvar smart-log-debug-face 'smart-log-debug-face)

(defvar smart-log-font-lock-keywords
  '(("\\berror\\b" 0 smart-log-error-face)
    ("\\bdebug\\b" 0 smart-log-debug-face)))

;;;
;;; rfc822
;;;

(defconst smart-log-rfc822-time-regexp
  (let* ((ms (loop repeat 12 for m in parse-time-months collect (car m)))
         (ws (loop repeat 7 for m in parse-time-weekdays collect (car m)))
         (mr (regexp-opt ms))
         (wr (regexp-opt ws))
         (2d "[0-9]\\{1,2\\}")
         (regexp (format
                  ".*?\\(%s, +\\)?%s +%s %s:%s:%s"
                  wr mr 2d 2d 2d 2d)))
    regexp))

(defun smart-log-rfc822-time ()
  (and (looking-at smart-log-rfc822-time-regexp)
       (cons (match-beginning 0) (match-end 0))))

(defun smart-log-rfc822-time->date (str)
  (destructuring-bind (sec min hour day month year . rest)
      (parse-time-string str)
    (encode-time sec min hour day month
                 (or year
                     ;;TODO over the year log file
                     (plist-get smart-log--plist :year)))))

;;;
;;; epoch
;;;

(defun smart-log-epoch ()
  (and (looking-at "[0-9.]+")
       (cons (match-beginning 0) (match-end 0))))

(defun smart-log-epoch->date (str)
  (seconds-to-time (string-to-number str)))

;;;
;;; tai64n (djb)
;;;

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

;;;
;;; General date
;;;

(defconst smart-log-general-date-regexp
  (let ((2d "[0-9]\\{1,2\\}"))
    (format
     (concat ".*?\\([0-9]\\{4\\}\\)[-/.]\\(%s\\)[-/.]\\(%s\\)"
             "[ \t]+"
             "\\(%s\\):\\(%s\\)\\(?::\\(%s\\)\\)?"
             "\\(?:[:.]\\([0-9]+\\)\\)?")
     2d 2d 2d 2d 2d)))

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
  (let ((2d "[0-9]\\{1,2\\}"))
    (format
     (concat ".*?\\([0-9]\\{2,4\\}\\)[-/.]\\(%s\\)[-/.]\\([0-9]\\{2,4\\}\\)"
             "[ \t]+"
             "\\(%s\\):\\(%s\\)\\(?::\\(%s\\)\\)?"
             "\\(?:[:.]\\([0-9]+\\)\\)?")
     2d 2d 2d 2d 2d)))

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
                       ((< (plist-get smart-log--plist :year) 2000)
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

;;;
;;; Apache Error Log
;;;

(defun smart-log-apache-error-log ()
  ;;FIXME regexp
  (and (looking-at "\\[\\([a-zA-Z]\\{3\\} [a-zA-Z]\\{3\\} [0-9]\\{1,2\\} [0-9:]+ [0-9]\\{4\\}\\)\\]")
       (cons (match-beginning 1) (match-end 1))))

(defun smart-log-apache-error-log->date (str)
  (apply 'encode-time (parse-time-string str)))

;;;
;;; Common Log Format (CLF)
;;;

(defun smart-log-clf-date ()
  ;;FIXME regexp is not correct...
  (and (looking-at ".*?\\[\\([0-9]\\{1,2\\}/.../[0-9]\\{4\\}\\(?::[0-9]\\{2\\}\\)\\{3\\}.*?\\)\\]")
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

(defun smart-log--prepare-scoring-buffer (file)
  (let* ((attr (file-attributes file))
         (tail-start (max (- (nth 7 attr) 4096) 0)))
    (smart-log--prepare-file-plist file)
    (insert-file-contents file nil 0 1024)
    (goto-char (point-min))
    (cond
     ;; first line exceed 1024 bytes append new line to end of buffer.
     ((looking-at "^.*\\'")
      (goto-char (point-max))
      (insert "\n"))
     (t
      (goto-char (point-max))
      (delete-region (line-beginning-position) (point-max))))
    (insert-file-contents file nil tail-start)
    (delete-region (point) (line-beginning-position 2))))

(defun smart-log--valid-formatters ()
  ;; 1. todo
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
    ;; 4. last log line have valid time (same as file mtime)
    ;; TODO precision of format..
    (let ((diff (abs (- (float-time last) mtime))))
      (push (max (- 1 (expt (/ diff 120.0) 2)) 0.5) scores))
    ;;TODO 3.
    ;; 2. valid log line approprivate count.
    (push (/ valid (ftruncate all)) scores)
    (apply '* scores)))

(defun smart-log--guessed-formatter (file)
  (with-temp-buffer
    (smart-log--prepare-scoring-buffer file)
    (smart-log--compute-formatter)))

;; 1. log file MUST start with LOG line.
;;    that line MUST have time string in first 1024 bytes.
;; 2. lines should have valid date appropriate count.
;; 3. lines have been nearly sorted by date.
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

(defvar smart-log--formatted-region nil)
(make-variable-buffer-local 'smart-log--formatted-region)

(defun smart-log--delayed-format ()
  (with-local-quit
    (when (eq major-mode 'smart-log-mode)
      (let ((region (cons (window-start) (window-end))))
        (unless (equal smart-log--formatted-region region)
          (unless smart-log--plist
            (smart-log--set-plist))
          (when smart-log--plist
            (let ((plist smart-log--plist))
              (save-excursion
                (goto-char (car region))
                (while (and (<= (point) (cdr region))
                            (not (eobp)))
                  (smart-log--format-current-line
                   (plist-get plist :sample-fn) (plist-get plist :convert-fn))))
              (setq smart-log--formatted-region region))))))))

(defun smart-log--set-plist ()
  (smart-log--prepare-file-plist buffer-file-name)
  ;;TODO buffer is too huge
  ;; TODO when unable compute valid formatter
  (let ((fmtr 
         ;; TODO refactor
         (let ((buf1 (current-buffer)))
           (with-temp-buffer
             (let ((buf2 (current-buffer))
                   mtime)
               (with-current-buffer buf1
                 (save-excursion
                   (goto-char (point-min))
                   (append-to-buffer buf2 (point-min) (line-beginning-position 3))
                   (goto-char (point-max))
                   (append-to-buffer buf2 (line-beginning-position -5) (point-max))
                   (setq mtime (visited-file-modtime))))
               (setq smart-log--plist (list :mtime (float-time mtime))))
             (smart-log--compute-formatter)))))
    (setq smart-log--plist
          (smart-log-merge-properties smart-log--plist fmtr))))

(defun smart-log-merge-properties (prop1 prop2)
  (loop with prop = (copy-sequence prop1)
        for ps on prop2 by (lambda (x) (cddr x))
        do (let ((name (car ps))
                 (value (cadr ps)))
             (plist-put prop name value))
        finally return prop))

;;;
;;; Handling isearch
;;;

(defun smart-log--iwsearch-forward (word &optional bound noerror _)
  (smart-log--iresearch-forward (concat "\\b" word "\\b") bound noerror _))

(defun smart-log--iwsearch-backward (word &optional bound noerror _)
  (smart-log--iresearch-backward (concat "\\b" word "\\b") bound noerror _))

(defun smart-log--iwlsearch-forward (word &optional bound noerror _)
  (smart-log--iresearch-forward (concat "\\b" word) bound noerror _))

(defun smart-log--iwlsearch-backward (word &optional bound noerror _)
  (smart-log--iresearch-backward (concat "\\b" word) bound noerror _))

(defun smart-log--isearch-function ()
  (cond
   (isearch-word
    (if (or isearch-nonincremental
            (eq (length isearch-string)
                (length (isearch-string-state (car isearch-cmds)))))
        (if isearch-forward 'smart-log--iwsearch-forward 'smart-log--iwsearch-backward)
      (if isearch-forward 'smart-log--iwlsearch-forward 'smart-log--iwlsearch-backward)))
   (isearch-regexp
    (if isearch-forward 'smart-log--iresearch-forward 'smart-log--iresearch-backward))
   (t
    (if isearch-forward 'smart-log--isearch-forward 'smart-log--isearch-backward))))

(defun smart-log--isearch-forward (string &optional bound noerror _)
  (smart-log--iresearch-forward (regexp-quote string) bound noerror _))

;;TODO cannot search text over overlay and text
;;  e.g. 12:34:45 hoge.htm
;;     "12:34:45 hoge" regexp fails
(defun smart-log--iresearch-forward (regexp &optional bound noerror _)
  (let* ((start (point))
         (text-pos (re-search-forward regexp bound t))
         (mdata (match-data))
         (point start)
         (ovs (overlays-in start (or text-pos (point-max))))
         (sorted (sort ovs (lambda (x y) (< (overlay-start x) (overlay-start y)))))
         display-ov)
    (catch 'done
      (dolist (ov sorted)
        (let ((string (overlay-get ov 'display)))
          (when (and (not (eq (overlay-start ov) (overlay-end ov)))
                     string (string-match regexp string))
            (setq mdata (list (overlay-start ov) (overlay-end ov))
                  display-ov ov)
              (throw 'done t)))))
    (cond
     (display-ov
      (goto-char (overlay-end display-ov))
      (set-match-data mdata)
      (overlay-end display-ov))
     (text-pos
      (set-match-data mdata)
      text-pos)
     ((not noerror) (error "Search failed \"%s\"" regexp))
     (t
      (set-match-data nil)))))

(defun smart-log--isearch-backward (string &optional bound noerror _)
  (smart-log--iresearch-backward (regexp-quote string) bound noerror _))

(defun smart-log--iresearch-backward (regexp &optional bound noerror _)
  (let* ((end (point))
         (text-pos (re-search-backward regexp bound t))
         (mdata (match-data))
         (point end)
         (ovs (overlays-in (or text-pos (point-min)) end))
         (sorted (sort ovs (lambda (x y) (> (overlay-start x) (overlay-start y)))))
         display-ov)
    (catch 'done
      (dolist (ov sorted)
        (let ((string (overlay-get ov 'display)))
          (when (and (not (eq (overlay-start ov) (overlay-end ov)))
                     string (string-match regexp string))
            (setq mdata (list (overlay-start ov) (overlay-end ov))
                  display-ov ov)
              (throw 'done t)))))
    (cond
     (display-ov
      (goto-char (overlay-start display-ov))
      (set-match-data mdata)
      (overlay-start display-ov))
     (text-pos
      (set-match-data mdata)
      text-pos)
     ((not noerror) (error "Search failed \"%s\"" regexp))
     (t
      (set-match-data nil)))))

(provide 'smart-log)

;;; smart-log.el ends here
