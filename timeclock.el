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

(defun timeclock/punch-in (&optional task is_feature notes)
  "Punch in, first punching out of any active tasks."
  (interactive)
  (let*
      ((task
        (or task
            (completing-read "Task: " (timeclock//tasks))))
       (is_feature
        (or is_feature
            (timeclock//y-or-n
             (completing-read
              "Is Feature? (y/n): "
              '("y" "n") nil t
              (timeclock//feature-for-task task)))))
       (notes (or notes (read-string "Notes: "))))
    (when (timeclock/active) (timeclock/punch-out))
    (timeclock//punch-in task is_feature notes)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions


;; (mapcar (lambda (task) (car task)) (timeclock//all-tasks))
;; (cadr (assoc "a new task" (timeclock//all-tasks)))

(defun timeclock//tasks ()
  (mapcar (lambda (task) (car task)) (timeclock//all-tasks)))

(defun timeclock//feature-for-task (task)
  (let ((feat (assoc task (timeclock//all-tasks))))
    (if (not (null feat))
        (timeclock//int-to-str (cadr feat))
      "n")))

(defun timeclock//punch-in (task is_feature notes)
  (let ((db (timeclock/database)))
    (sqlite-execute
     db
     "INSERT INTO timeclock (task, is_feature, clock_in, notes)
      VALUES (?, ?, ?, ?)"
     `(,task
       ,(timeclock//bool-to-int is_feature)
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


(provide 'timeclock)
