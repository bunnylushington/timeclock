# timeclock.el

Provide a simple punch-in and -out clock for tasks and associated
report generation.

## Rationale

While org-mode's clock functionality is useful, I wanted something
much lighter weight to keep track of where I'm spending my work time.
timeclock.el uses sqlite as a datastore allowing for fast arbitrary
queries.

## Usage

  * `timeclock/punch-in` start a task (potentially punching out first)
  * `timeclock/punch-out` stop a task
  * `timeclock/report` generate a report
  * `timeclock/active` show the current task and elapsed time in the echo area
  * `timeclock/active-task-name` returns the active task name or nil
  * `timeclock/set-database` change the active timeclock database

No keybindings have been set.

### Notes

#### Database

Timeclock data are stored in a SQLite database.  The location of this
database can be configured by setting the variable `timeclock/db-file`
By default it is named `timeclock.db` in the user-emacs-directory.

A handle to the sqlite object can be obtained with the function
`timeclock/database`.  Calling this function will create the file and
schema if necessary.

The DB may be accessed directly.  Note that the 'clock_in' and
'clock_out' attributes are integers representing epoch seconds.

Because the database handle is cached, use `timeclock/set-database` to
change database files; simply setting `timeclock/db-file` to a new
value will not suffice.

#### Weeks

The variable `timeclock/start-of-week` may be customised to an integer
between 0 (Sunday) and 6.  It defaults to 1 (Monday) as the start of
the week for weekly reports.

#### Report Types

A number of reports (e.g., "Today", "Yesterday") are
defined.  To add custom reports put entries in the hashtable
`timeclock/report-span-hash`.  The key is a human readable description
of the span and the value a sqlite where clause.

For example, to create a report on all tasks starting with "t":

``` emacs-lisp
(puthash "Starts with t"
    "substr(task, 1, 1) = 't'"
    timeclock/report-span-hash)
```

#### Editing Timeclock Records

Timeclock records may be edited.  In a Timeclock Report Detail section
with point on a punch entry the following actions are supported:

| key | action |
| --- | ------ |
| <kbd>f</kbd> | toggle feature flag |
| <kbd>d</kbd> | delete timeclock entry |
| <kbd>t</kbd> | edit task name |
| <kbd>a</kbd> | adjust punch-in/out time |
| <kbd>n</kbd> | edit note |

Editing a note will pop open a dedicated buffer.  <kbd>C-c C-c</kbd>
will save the changes; <kbd>C-c C-k</kbd> will dismiss the buffer
without saving.  `auto-fill-mode` is turned on in the buffer with a
`fill-column` set to `timeclock/note-fill-column`, the same value used
to fill the note in the report.


#### Active Tasks

There may be only zero or one tasks active at any time.  Quitting
Emacs will not by default punch out of a currently active task.
`timeclock/maybe-punch-out` is provided as a possible hook function;
the user will be queried if they want to punch out before quitting.

#### is-feature Indicator

There is a boolean flag `is-feature` that can be optionally set for
each punch-in; these tasks are reflected in the report by a bullet.
When punching into a task the user will be asked if this flag should
be set.  The prompt text can be customized by setting the variable
`timeclock/feature-text`.  The is-feature indicator in reports can be
customized by setting `timeclock/feature-indicator`

Without prefix arguments `timeclock/report` will report on all tasks,
both those with the is-feature flag set and not.  With one prefix
argument (`C-u M-x timeclock/report`) only those tasks with is-feature
set will be reported.  With two prefix arguments (`C-u C-u M-x
timeclock/report`) only those tasks without is-feature set will be
reported.

#### Faces

A handful of faces are defined for displaying reports.  These all have
the prefix `timeclock-`.

#### Hooks

Some hooks are provided:

  * timeclock/pre-punch-in-hook
  * timeclock/post-punch-in-hook
  * timeclock/pre-punch-out-hook
  * timeclcok/post-punch-out-hook

## Installation

``` emacs-lisp
(use-package timeclock
  :straight '(timeclock
              :type git
              :host github
              :repo "bunnylushington/timeclock")
  :init
  (setq timeclock/db-file
        (expand-file-name "timeclock.db" user-emacs-directory)))
```


## Tab Bar Configuration

I find it useful to have punch in/out access directly in the tab-bar.
This functionality is not part of timeclock but here's one possible
implementation.  Clicking on the "➕" will punch into a task that will
then be displayed in the tab-bar.  Clicking on the task name will punch out.

``` emacs-lisp
  (defvar ii/timeclock-in-arrow
    (propertize " ➕ " 'display '(raise -0.20)))

  (defun ii/timeclock-active-task ()
    "If there's a current timeclock  task, display it."
    (when (fboundp #'timeclock/active-task-name)
      (let ((task (timeclock/active-task-name)))
        (if (not (null task))
            (propertize task 'display '(raise -0.20))))))

  (defun ii/tab-bar-timeclock ()
    "Punch in/out in the tab-bar"
    `((ii/timeclock-out
       menu-item ,(ii/timeclock-active-task) timeclock/punch-out
       :help "Punch out of this task.")
      (ii/timeclock-in
       menu-item ,ii/timeclock-in-arrow timeclock/punch-in
       :help "Punch into new task.")))

  (setq tab-bar-format '(tab-bar-format-tabs
                         tab-bar-format-align-right
                         ii/tab-bar-timeclock))
```


## Author

Bunny Lushington

## License

MIT License

Copyright (c) 2023 Bunny Lushington

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
