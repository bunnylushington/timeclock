;; be sure to set timeclock/db-file!

(defvar timeclock/db nil
  "The sqlite object for timeclock operations.")

(defvar timeclock/db-file "/tmp/timeclock.db"
  "The filename of the timeclock sqlite DB.")

(defun timeclock/database (&optional file)
  (if (sqlitep timeclock/db)
      (timeclock//create-schema timeclock/db)
    (let ((db-file (or file timeclock/db-file)))
      (setq timeclock/db (sqlite-open db-file))
      (timeclock//create-schema timeclock/db))))

(defun timeclock//create-schema (db)
  (timeclock//create-table-timeclock db)
  db)

(defun timeclock/punch-in (&optional task is-feature notes)
  "Punch in, first punching out of any active tasks."
  (interactive)
  (let*
      ((task
        (or task
            (completing-read "Task: " (timeclock//tasks))))
       (is-feature
        (or is-feature
            (timeclock//y-or-n
             (completing-read
              "Is Feature? (y/n): "
              '("y" "n") nil t
              (timeclock//feature-for-task task)))))
       (notes (or notes (read-string "Notes: "))))
    (when (timeclock/active) (timeclock/punch-out))
    (timeclock//punch-in task is-feature notes)))

(defun timeclock/punch-out ()
  "Punch of all active tasks."
  (interactive)
  (when (timeclock/active) (timeclock//punch-out)))

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

(defface timeclock-smallskip-face
  '((t :inherit default
       :height 0.3))
  "Timeclock smallskip face"
  :group 'timeclock)

(defface timeclock-time-detail-face
  '((t :inherit font-lock-string-face))
  "Timeclock time face"
  :group 'timeclock)

(defface timeclock-time-elapsed-face
  '((t :inherit font-lock-comment-face))
  "Timeclock time face"
  :group 'timeclock)



(defun timeclock/report (&optional when)
  "Run a report."
  (interactive)
  (let* ((when (or when
                   (intern (completing-read "Report Span: "
                                            '(today
                                              yesterday
                                              this-week
                                              last-week
                                              this-month
                                              last-month)))))
         (tasks (timeclock//report when))
         (total 0)
         (buf (when tasks (get-buffer-create "*timeclock report*"))))
    (when buf
      (set-buffer buf)
      (erase-buffer)
      (insert (concat (propertize "Timeclock Report" 'face 'timeclock-header-1-face)
                      "\n\n"
                      (propertize "Summary" 'face 'timeclock-header-2-face)
                      "\n"
                      (propertize "\n" 'face 'timeclock-smallskip-face)))

      (dolist (task tasks)
        (setq total (+ total (nth 1 task)))
        (insert (format
                 "%15s%s - %s\n"
                 (timeclock//seconds-to-display-time (nth 1 task))
                 (if (timeclock//int-to-bool (nth 2 task))
                     "*" " ")
                 (nth 0 task))))
      (insert (concat (make-string 50 ?-) "\n"))
      (insert (format "%15s  - Total\n"
                      (timeclock//seconds-to-display-time total)))
      (insert (concat "\n\n"
                      (propertize "Detail" 'face 'timeclock-header-2-face)
                      "\n"
                      (propertize "\n" 'face 'timeclock-smallskip-face)))
      (let (current-day
            previous-current-day
            (day-total 0)
            (day-task-counter 0))
        (dolist (task (timeclock//detail when))

          (setq current-day (format-time-string "%A %B %e" (seconds-to-time (nth 2 task))))
          (if (not (string= current-day previous-current-day))
              ;; this is when it's a new day
              (progn
                (when (> day-total 0)
                  (insert (format "%s%s\n"
                                  (make-string 19 ?\ )
                                  (timeclock//seconds-to-display-time day-total))))
                (setq day-total (- (nth 3 task) (nth 2 task))
                      day-task-counter 0
                      previous-current-day current-day)
                (insert (format "%s%s\n"
                                (propertize "\n" 'face 'timeclock-smallskip-face)
                                current-day)))
            ;; this is when it's the same day
            (progn
              (setq day-task-counter (+ 1 day-task-counter)
                    day-total (+ day-total (- (nth 3 task) (nth 2 task))))))
          (let ((elapsed (timeclock//seconds-to-display-time (nth 5 task))))
            (insert (format
                     "    %s  %-8s   %s %s\n"
                     (propertize
                      (format "%s - %s"
                              (format-time-string "%R" (seconds-to-time (nth 2 task)))
                              (format-time-string "%R" (seconds-to-time (nth 3 task))))
                      'face 'timeclock-time-detail-face)
                     (propertize elapsed 'face 'timeclock-time-elapsed-face)
                     (nth 0 task)
                     (nth 4 task)))))
        (insert (format "%s%s\n"
                        (make-string 19 ?\ )
                        (timeclock//seconds-to-display-time day-total))))

      (display-buffer (current-buffer) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions

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

(defun timeclock//range (when)
  (cond
   ((eq when 'yesterday) (timeclock//yesterday))
   ((eq when 'this-week) (timeclock//this-week))
   ((eq when 'last-week) (timeclock//last-week))
   ((eq when 'this-month) (timeclock//this-month))
   ((eq when 'last-month) (timeclock//last-month))
   (t (timeclock//today))))

(defun timeclock//report (when)
  (let ((db (timeclock/database))
        (range (timeclock//range when)))
    (sqlite-select
     db
     (concat "SELECT task,
                     sum(coalesce(duration,
                        (unixepoch('now') - clock_in))),
                     is_feature
              FROM timeclock
              WHERE " range "
              GROUP BY task, is_feature"))))


;; NB: these epoch seconds are not converted to localtime because it
;; appears (second-to-time) or (format-time-string) does that itself.
(defun timeclock//detail (when)
  (let ((db (timeclock/database))
        (range (timeclock//range when)))
    (sqlite-select
     db
     (concat "SELECT task,
                     is_feature,
                     clock_in,
                     clock_out,
                     notes,
                     (clock_out - clock_in)
              FROM timeclock
              WHERE " range))))

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

(defun timeclock//y-or-n (obj)
  (if (string= "y" obj) 1 0))

(defun timeclock//int-to-str (obj)
  (if (= 1 obj) "y" "n"))


(defun timeclock//today ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of day')")

(defun timeclock//yesterday ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of day', '-1 day')
   AND
   unixepoch(clock_in, 'unixepoch', 'localtime') <
   unixepoch('now', 'localtime', 'start of day')")

(defun timeclock//this-week ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime',  'weekday 1', '-7 days', 'start of day')")

(defun timeclock//last-week ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'weekday 1', '-14 days', 'start of day')
   AND
   unixepoch(clock_in, 'unixepoch', 'localtime') <
   unixepoch('now', 'localtime', 'weekday 1', '-7 days', 'start of day')")

(defun timeclock//this-month ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of month')")

(defun timeclock//last-month ()
  "unixepoch(clock_in, 'unixepoch', 'localtime') >=
   unixepoch('now', 'localtime', 'start of month', '-1 month')
   AND
   unixepoch(clock_in, 'unixepoch', 'localtime') <
   unixepoch('now', 'localtime', 'start of month')")

(defun timeclock//seconds-to-display-time (secs)
  (let* ((hours (/ secs 3600))
         (minutes (/ (% secs 3600) 60))
         (seconds (% secs 60)))
    (format "%s%s"
            (if (> hours 0)
                (format "%sh " hours)
              "")
            (if (> minutes 0)
                (format "%sm " minutes)
              ""))))

(provide 'timeclock)
