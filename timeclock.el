;;; timeclock.el --- timeclock reporting -*- lexical-binding: t -*-
;; Author: Bunny Lushington <bunny@bapi.us>
;; Version: 1.0
;; Package-Requires: (dash s)

;;; Commentary:
;;
;; timeclock.el provides basic time keeping and reporting functionality.


(require 'dash)
(require 's)

(defface timeclock-header-1-face
  '((t :inherit font-lock-function-name-face
       :height 1.5))
  "Timeclock header 1 face"
  :group 'timeclock)

(defface timeclock-header-2-face
  '((t :inherit font-lock-warning-face
       :height 1.3))
  "Timeclock header 2 face"
  :group 'timeclock)

(defface timeclock-time-detail-face
  '((t :inherit font-lock-string-face))
  "Timeclock time face"
  :group 'timeclock)

(defface timeclock-time-elapsed-face
  '((t :inherit font-lock-comment-face))
  "Timeclock time face"
  :group 'timeclock)

(defface timeclock-description-face
  '((t :inherit font-lock-comment-face))
  "Timeclock range description face"
  :group 'timeclock)

(defface timeclock-note-face
  '((t :inherit font-lock-type-face))
  "Timeclock face to display notes"
  :group 'timeclock)

(defvar timeclock/db nil
  "The sqlite object for timeclock operations.")

(defvar timeclock/db-file
  (expand-file-name "timeclock.db" user-emacs-directory)
  "The filename of the timeclock sqlite DB.")

(defvar timeclock/feature-text "Is Feature"
  "The prompt for the is-feature flag.")

(defvar timeclock/feature-indicator "•"
  "Character to denote set is-feature flag in reports.")

(defvar timeclock/start-of-week 1
  "Day a week starts on, 0-6, Sunday is 0.")

(defvar timeclock/pre-punch-in-hook nil
  "Normal hook run before punching in.")

(defvar timeclock/post-punch-in-hook nil
  "Normal hook run after punching in.")

(defvar timeclock/pre-punch-out-hook nil
  "Normal hook run before punching out.")

(defvar timeclock/post-punch-out-hook nil
  "Normal hook run after punching out.")

(defun timeclock/database (&optional file)
  "Create the database schema in FILE; return sqlite object."
  (if (sqlitep timeclock/db)
      (timeclock//create-schema timeclock/db)
    (let ((db-file (or file timeclock/db-file)))
      (setq timeclock/db (sqlite-open db-file))
      (timeclock//create-schema timeclock/db))))

(defun timeclock/punch-in (&optional task is-feature notes)
  "Punch in, first punching out of an active task."
  (interactive)
  (let*
      ((task
        (or task
            (completing-read "Task: " (timeclock//tasks))))
       (is-feature
        (or is-feature (timeclock//bool-to-int
                        (y-or-n-p timeclock/feature-text))))
       (notes (or notes (read-string "Notes: "))))
    (when (timeclock/active) (timeclock/punch-out))
    (run-hooks 'timeclock/pre-punch-in-hook)
    (timeclock//punch-in task is-feature notes)
    (run-hooks 'timeclock/post-punch-in-hook)))

(defun timeclock/punch-out ()
  "Punch of active task."
  (interactive)
  (when (timeclock/active)
    (run-hooks 'timeclock/pre-punch-out-hook)
    (timeclock//punch-out)
    (run-hooks 'timeclock/post-punch-out-hook)))

(defun timeclock/maybe-punch-out ()
  "Propmt user to maybe punch out of active task."
  (interactive)
  (when (and (timeclock/active)
             (y-or-n-p (format "Punch out of current task (%s)?"
                               (timeclock/active-task-name))))
    (timeclock//punch-out)))

(defun timeclock/active ()
  "Return the active task, or nil if none."
  (interactive)
  (let ((task (timeclock//active-task)))
    (if task
        (progn
          (if (called-interactively-p)
              (message (format "%s (%s)"
                               (car task) (nth 2 task))))
          task)
      (progn
        (if (called-interactively-p)
            (message "Timeclock inactive."))
        nil))))

(defun timeclock/active-task-name ()
  "Return the title of the active task or nil."
  (let ((task (timeclock/active)))
    (if task (car task) nil)))

(defun timeclock/report (&optional feature-flag span-text)
  "Produce a timeclock report."
  (interactive "p")
  (let* ((span (timeclock//report-capture-span span-text))
         (span-description (car span))
         (range (cadr span))
         (tasks (timeclock//report range feature-flag))
         (buf (when tasks (get-buffer-create "*timeclock report*"))))
    (if buf
        (progn
          (set-buffer buf)
          (fundamental-mode)
          (erase-buffer)
          (add-to-invisibility-spec 'timeclock-note)
          (timeclock//report-summary-section-header span-description)
          (timeclock//report-summary-section-table tasks)
          (timeclock//report-detail-section-header)
          (timeclock//report-detail-section-table range feature-flag)
          (view-mode)
          (goto-char (point-min))
          (display-buffer (current-buffer) t)
          (message "Generated timeclock report for %s"
                   (s-downcase span-description)))
      (message "No timeclock data for date range."))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reporting Functions

(defun timeclock//feature-flag-clause-hash ()
  (let ((clauses (make-hash-table)))
    (puthash 4  "AND is_feature = 1" clauses)
    (puthash 16 "AND is_feature = 0" clauses)
    clauses))

(defun timeclock//report-capture-span (&optional span-text)
  (let* ((span-text (or span-text
                        (completing-read
                         "Report Span: "
                         timeclock/report-span-hash nil t)))
         (span-clause (gethash span-text timeclock/report-span-hash)))
    (list span-text span-clause)))

(defun timeclock//report-summary-section-header (span-description)
  (insert (concat (propertize "Timeclock Report" 'face 'timeclock-header-1-face)
                  "\n"
                  (propertize span-description 'face 'timeclock-description-face)
                  "   "
                  (buttonize "[toggle notes]" #'timeclock//toggle-notes)
                  "\n\n"
                  (propertize "Summary\n"
                              'face 'timeclock-header-2-face
                              'line-spacing 0.3))))

(defun timeclock//report-summary-section-table (tasks)
  (let (table
        (total (-reduce '+ (--map (nth 1 it) tasks))))
    (setq table
          (make-vtable
           :use-header-line nil
           :insert nil
           :face 'default
           :objects tasks
           :actions '(;; "unbind" the default vtable bidings
                      "S" ignore
                      "{" ignore
                      "}" ignore
                      "g" ignore
                      "M-<left>" ignore
                      "M-<right>" ignore)

           :displayer (lambda (value index max-width table)
                        value)
           :formatter (lambda (value index table)
                        (cond
                         ((= index 0) (timeclock//seconds-to-display-time value))
                         ((= index 1) (if (timeclock//int-to-bool value)
                                          timeclock/feature-indicator " "))
                         (t value)))
           :getter (lambda (object index table)
                     (-let (((task duration is-feature) object))
                       (cond
                        ((= index 0) duration)
                        ((= index 1) is-feature)
                        ((= index 2) task))))))
    (vtable-insert table)
    (goto-char (point-max))
    (insert (concat (make-string 20 ?﹏) "\n"))
    (insert (format "%9s\n" ;; "xxxxh  xxm"
                    (timeclock//seconds-to-display-time total)))))

(defun timeclock//report-detail-section-header ()
  (insert (concat "\n\n"
                  (propertize "Detail\n"
                              'face 'timeclock-header-2-face
                              'line-spacing 0.3))))

(defun timeclock//report-detail-section-day-total (day-total)
  (insert (format "%s%s\n"
                  (make-string 20 ?\ )
                  (timeclock//seconds-to-display-time day-total))))

(cl-defstruct timeclock/task
  "A timeclock task structure."
  title
  is-feature
  punch-in
  punch-out
  notes
  duration
  entry-id)

(defun timeclock//daily-details (range feature-flag)
  (let (task-list)
    (dolist (task (timeclock//detail range feature-flag))
      (-let (((title is-feature punch-in punch-out notes duration entry-id) task))
        (let* ((task-day (format-time-string "%A %B %e" (seconds-to-time punch-in)))
               (this-day-tasks (alist-get task-day task-list nil nil 'equal))
               (task-obj (make-timeclock/task
                          :title title
                          :is-feature is-feature
                          :punch-in punch-in
                          :punch-out punch-out
                          :notes notes
                          :duration duration
                          :entry-id entry-id)))
          (setf (alist-get task-day task-list nil nil 'equal)
                (append this-day-tasks (list task-obj))))))
    (reverse task-list)))

(defun timeclock//calculate-daily-totals (daily-details)
  (let (total-times)
    (dolist (day daily-details)
      (let ((today (car day))
            (total (-reduce '+ (--map (timeclock/task-duration it) (cdr day)))))
        (setq total-times (append total-times (list (list today total))))))
    total-times))

(defun timeclock//obj-to-daily-summary (tasks)
  "Convert a list of task objects into something vtable can display.

This amounts to a list of lists containing only the formatted
attributes present in the daily detail section of the report."
  (let (objects)
    (dolist (task tasks)
      (let* ((in-str (format-time-string "%R" (seconds-to-time (timeclock/task-punch-in task))))
             (out-str (if (timeclock/task-punch-out task)
                          (format-time-string "%R" (seconds-to-time (timeclock/task-punch-out task)))
                        (make-string 5 ?\ )))
             (time-range (format "%s - %s" in-str out-str))
             (duration (timeclock//seconds-to-display-time (timeclock/task-duration task))))
        (setq objects (append objects
                              (list (list
                                     (timeclock/task-is-feature task)
                                     time-range
                                     duration
                                     (timeclock/task-title task)
                                     (timeclock/task-notes task)))))))
    objects))


(defun timeclock//report-detail-section-table (range feature-flag)
  (let* (day-table day-tasks
                   (daily-details (timeclock//daily-details range feature-flag))
                   (totals (timeclock//calculate-daily-totals daily-details)))
    (dolist (today daily-details)
      (insert (propertize (format "%s\n" (car today)) 'line-spacing 0.1))
      (setq day-tasks (timeclock//obj-to-daily-summary (cdr today)))
      (setq day-table
            (make-vtable
             :use-header-line nil
             :insert nil
             :face 'default
             :objects day-tasks
             :actions '("RET" (lambda (obj) (prin1 obj))
                        ;; "unbind" the default vtable bidings
                        "S" ignore
                        "{" ignore
                        "}" ignore
                        "g" ignore
                        "M-<left>" ignore
                        "M-<right>" ignore)
             :displayer (lambda (value index max-width table)
                          (cond
                           ((= index 0)
                            (format "  %s"
                                    (if (timeclock//int-to-bool value)
                                        timeclock/feature-indicator " ")))
                           ((= index 1)
                            (propertize value 'face 'timeclock-time-detail-face))
                           ((= index 2)
                            (propertize value 'face 'timeclock-time-elapsed-face))
                           ((= index 4)
                            (if (not (equal "" value))
                                 (propertize (concat "\n" (string-fill value 50) "\n")
                                             'face 'timeclock-note-face
                                             'line-prefix (make-string 6 ?\ )
                                             'invisible 'timeclock-note)
                              ""))
                           (t value)))
             :getter (lambda (object index table)
                       (-let (((is-feature time-range duration task notes) object))
                         (cond
                          ((= index 0) is-feature)
                          ((= index 1) time-range)
                          ((= index 2) duration)
                          ((= index 3) task)
                          ((= index 4) notes)
                          (t ""))))))

      (vtable-insert day-table)
      (goto-char (point-max))
      (let* ((duration (car (alist-get (car today) totals)))
             (printable (timeclock//seconds-to-display-time duration)))
        (insert (propertize (format "%s%s\n"
                                    (make-string 18 ?\ )
                                    printable)
                            'line-spacing 0.3))))))

(defun timeclock//toggle-notes (&rest args)
  (if (member 'timeclock-note buffer-invisibility-spec)
      (remove-from-invisibility-spec 'timeclock-note)
    (add-to-invisibility-spec 'timeclock-note)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility Functions

(defun timeclock//create-schema (db)
  (timeclock//create-table-timeclock db)
  db)

(defun timeclock//tasks ()
  (mapcar (lambda (task) (car task)) (timeclock//all-tasks)))

(defun timeclock//feature-for-task (task)
  (let ((feat (assoc task (timeclock//all-tasks))))
    (if (not (null feat))
        (timeclock//int-to-str (cadr feat))
      "n")))

(defun timeclock//punch-in (task is-feature notes)
  (let ((db (timeclock/database)))
    (sqlite-execute
     db
     "INSERT INTO timeclock (task, is_feature, clock_in, notes)
      VALUES (?, ?, ?, ?)"
     `(,task
       ,is-feature
       ,(floor (float-time))
       ,notes))))

(defun timeclock//active-task ()
  (let ((db (timeclock/database)))
    (car (sqlite-select
          db
          "SELECT task,
             datetime(clock_in, 'unixepoch'),
             time((unixepoch() - clock_in), 'unixepoch')
           FROM timeclock
           WHERE is_active = 1"))))

(defun timeclock//punch-out ()
  (let ((db (timeclock/database)))
    (sqlite-execute
     db
     "UPDATE timeclock
      SET clock_out = ?
      WHERE is_active = 1"
     `(,(floor(float-time))))))

(defun timeclock//all-tasks ()
  (let ((db (timeclock/database)))
    (sqlite-select
     db
     "SELECT DISTINCT task, is_feature
      FROM timeclock
      ORDER BY entry_id ASC")))

(defun timeclock//feature-clause (flag)
  (gethash flag (timeclock//feature-flag-clause-hash) ""))

(defun timeclock//report (range feature-flag)
  (let ((db (timeclock/database)))
    (sqlite-select
     db
     (concat "SELECT task,
                     sum(coalesce(duration,
                        (unixepoch('now') - clock_in))),
                     is_feature
              FROM timeclock
              WHERE " range " " (timeclock//feature-clause feature-flag) "
              GROUP BY task, is_feature"))))

;; NB: these epoch seconds are not converted to localtime because it
;; appears (second-to-time) or (format-time-string) does that itself.
(defun timeclock//detail (range feature-flag)
  (let ((db (timeclock/database)))
    (sqlite-select
     db
     (concat "SELECT task,
                     is_feature,
                     clock_in,
                     clock_out,
                     notes,
                     coalesce(duration,
                              (unixepoch('now') - clock_in)),
                     entry_id
              FROM timeclock
              WHERE " range " " (timeclock//feature-clause feature-flag)
              " ORDER BY clock_in"
              ))))

(defun timeclock//create-table-timeclock (db)
  (sqlite-execute
   db
   "CREATE TABLE IF NOT EXISTS timeclock (
    entry_id        INTEGER PRIMARY KEY,
    task            TEXT NOT NULL,
    is_feature      INTEGER NOT NULL,
    clock_in        INTEGER NOT NULL,
    clock_out       INTEGER NULL,
    notes           TEXT NULL,
    is_active       GENERATED ALWAYS AS
                       (CASE WHEN clock_in IS NOT NULL
                             AND clock_out IS NULL
                       THEN 1
                       ELSE 0
                       END) VIRTUAL,
   duration         GENERATED ALWAYS AS
                       (CASE WHEN clock_in IS NOT NULL
                              AND clock_out IS NOT NULL
                        THEN (clock_out - clock_in)
                        ELSE NULL
                        END) VIRTUAL
 )"))


(defun timeclock//bool-to-int (obj)
  (if obj 1 0))

(defun timeclock//int-to-bool (obj)
  (if (or (null obj) (= 0 obj)) nil t))

(defun timeclock//int-to-str (obj)
  (if (= 1 obj) "y" "n"))


(defun timeclock//span-today ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of day')")

(defun timeclock//span-yesterday ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of day', '-1 day')
   AND
   unixepoch(clock_in, 'unixepoch', 'localtime') <
   unixepoch('now', 'localtime', 'start of day')")

(defun timeclock//span-this-week ()
  (let ((dow (or timeclock/start-of-week 1)))
    (format
     "unixepoch(clock_in, 'unixepoch', 'localtime') >=
 iif(strftime('%%w', 'now', 'localtime') = '%1$d',
    unixepoch('now', 'localtime', 'weekday %1$d', 'start of day'),
    unixepoch('now', 'localtime', 'weekday %1$d', '-7 days', 'start of day'))"
     dow)))

(defun timeclock//span-last-week ()
  (let ((dow (or timeclock/start-of-week 1)))
    (format
     "unixepoch(clock_in, 'unixepoch', 'localtime') >=
 -- beginning of last week
 iif(strftime('%%w', 'now', 'localtime') = '%1$d',
    unixepoch('now', 'localtime', 'weekday %1$d', '-7 days', 'start of day'),
    unixepoch('now', 'localtime', 'weekday %1$d', '-14 days', 'start of day'))
 AND
 -- end of last week
 unixepoch(clock_in, 'unixepoch', 'localtime') <
 iif(strftime('%%w', 'now', 'localtime') = '%1$d',
    unixepoch('now', 'localtime', 'weekday %1$d', 'start of day'),
    unixepoch('now', 'localtime', 'weekday %1$d', '-7 days', 'start of day'))
"
     dow)))

(defun timeclock//span-this-month ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of month')")

(defun timeclock//span-last-month ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of month', '-1 month')
   AND
   unixepoch(clock_in, 'unixepoch', 'localtime') <
   unixepoch('now', 'localtime', 'start of month')")

(defun timeclock//default-report-span-hash ()
  (let ((spans (make-hash-table :test 'equal)))
    (puthash "Today"      (timeclock//span-today)      spans)
    (puthash "Yesterday"  (timeclock//span-yesterday)  spans)
    (puthash "This Week"  (timeclock//span-this-week)  spans)
    (puthash "Last Week"  (timeclock//span-last-week)  spans)
    (puthash "This Month" (timeclock//span-this-month) spans)
    (puthash "Last Month" (timeclock//span-last-month) spans)
    spans))

(defvar timeclock/report-span-hash (timeclock//default-report-span-hash)
  "Hash enumerating possible report time spans.")

;; "XXXXh YYm"
(defun timeclock//seconds-to-display-time (secs)
  (let* ((seconds/hour (* 60 60))
         (hours (/ secs seconds/hour))
         (minutes (/ (% secs seconds/hour) 60)))
    (format "%s %s"
            (if (= hours 0) (make-string 5 ?\ ) (format "%4sh" hours))
            (cond
             ((and (= hours 0) (< minutes 1)) "<1m")
             (t (format "%2sm" minutes))))))

(provide 'timeclock)
