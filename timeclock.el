;; be sure to set timeclock/db-file!

(require 'dash)

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

(defface timeclock-medskip-face
  '((t :inherit default
       :height 0.5))
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

(defvar timeclock/db-file "/tmp/timeclock.db"
  "The filename of the timeclock sqlite DB.")

(defun timeclock/database (&optional file)
  (if (sqlitep timeclock/db)
      (timeclock//create-schema timeclock/db)
    (let ((db-file (or file timeclock/db-file)))
      (setq timeclock/db (sqlite-open db-file))
      (timeclock//create-schema timeclock/db))))

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

(defun timeclock/report (&optional when)
  "Produce a timeclock report."
  (interactive)
  (let* ((span (timeclock//report-capture-span))
         (span-description (car span))
         (when (cadr span))
         (tasks (timeclock//report when))
         (buf (when tasks (get-buffer-create "*timeclock report*"))))
    (if buf
        (progn
          (set-buffer buf)
          (fundamental-mode)
          (erase-buffer)
          (add-to-invisibility-spec 'timeclock-note)
          (timeclock//report-summary-section-header span-description)
          (timeclock//report-summary-section-body tasks)
          (timeclock//report-detail-section-header)
          (timeclock//report-detail-section-body when)
          (view-mode)
          (goto-char (point-min))
          (display-buffer (current-buffer) t))
      (message "No timeclock data for date range."))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reporting Functions
(defun timeclock//capture-spans ()
  (let ((spans (make-hash-table :test 'equal)))
    (puthash "Today" 'today spans)
    (puthash "Yesterday" 'yesterday spans)
    (puthash "This Week" 'this-week spans)
    (puthash "Last Week" 'last-week spans)
    (puthash "This Month" 'this-month spans)
    (puthash "Last Month" 'last-month spans)
    spans))

(defun timeclock//report-capture-span ()
  (let* ((spans (timeclock//capture-spans))
         (span-text (completing-read "Report Span: "
                                     spans nil t))
         (span (gethash span-text spans)))
    (list span-text span)))

(defun timeclock//report-summary-section-header (span-description)
  (insert (concat (propertize "Timeclock Report" 'face 'timeclock-header-1-face)
                  "\n"
                  (propertize span-description 'face 'timeclock-description-face)
                  "   "
                  (buttonize "[toggle notes]" #'timeclock//toggle-notes)
                  "\n\n"
                  (propertize "Summary" 'face 'timeclock-header-2-face)
                  "\n"
                  (propertize "\n" 'face 'timeclock-smallskip-face))))

(defun timeclock//report-summary-section-body (tasks)
  (let ((total 0))
    (dolist (task tasks)
      (-let (((title duration is-feature) task))
        (setq total (+ total duration))
        (insert (format
                 "%15s%s %s\n"
                 (timeclock//seconds-to-display-time duration)
                 (if (timeclock//int-to-bool is-feature) "*" " ")
                 title))))
    (insert (concat (make-string 50 ?-) "\n"))
    (insert (format "%15s  Total\n"
                    (timeclock//seconds-to-display-time total)))))

(defun timeclock//report-detail-section-header ()
  (insert (concat "\n\n"
                  (propertize "Detail" 'face 'timeclock-header-2-face)
                  "\n"
                  (propertize "\n" 'face 'timeclock-smallskip-face))))

(defun timeclock//report-detail-section-day-total (day-total)
  (insert (format "%s%s\n"
                  (make-string 19 ?\ )
                  (timeclock//seconds-to-display-time day-total))))


(defun timeclock//report-detail-section-body (when)
  (let (current-day
        previous-current-day
        (day-total 0))
    (dolist (task (timeclock//detail when))
      (-let (((title is-feature punch-in punch-out notes duration) task))
        (setq current-day (format-time-string "%A %B %e" (seconds-to-time punch-in)))
        (if (not (string= current-day previous-current-day))
            ;; a brand new day
            (progn
              (when previous-current-day
                (timeclock//report-detail-section-day-total day-total))
              (setq day-total duration
                    previous-current-day current-day)
              (insert (format "%s%s\n"
                              (propertize "\n" 'face 'timeclock-smallskip-face)
                              current-day)))
          ;; same day as the last task
          (setq day-total (+ duration day-total)))

          (insert (format
                   "    %s  %-8s  %s\n%s"
                   (propertize (format "%s - %s"
                                       (format-time-string "%R" (seconds-to-time punch-in))
                                       (if punch-out
                                           (format-time-string "%R" (seconds-to-time punch-out))
                                         (make-string 5 ?\ )))
                               'face 'timeclock-time-detail-face)
                   (propertize (timeclock//seconds-to-display-time duration)
                               'face 'timeclock-time-elapsed-face)
                   title
                   (if (not (string= notes ""))
                       (propertize (format "%s%s\n"
                                           (make-string 29 ?\ )
                                           notes)
                               'face 'timeclock-note-face
                               'invisible 'timeclock-note)
                     "")))))
    ;; total for the last day of the report.
    (timeclock//report-detail-section-day-total day-total)))


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
                     coalesce(duration,
                              (unixepoch('now') - clock_in))
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
