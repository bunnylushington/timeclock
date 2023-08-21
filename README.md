# timeclock.el

Provide a simple punch-in and -out clock for tasks.

## Rationale

While org-mode's clock functionality is useful, I wanted something
much lighter weight to keep track of where I'm spending my work time.
timeclock.el uses sqlite as a datastore and has the benefit of being
**much** faster than org and has the benefit of completion for task
names.

There is a flag `is-feature` that can be optionally set for each
punch-in; these tasks are reflected in the report by an asterisk.

## Configuration

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

## Usage

  * `timeclock/punch-in` start a task (potentially punching out of an
    existing one)
  * `timeclock/punch-out` stop a task
  * `timeclock/report` generate a report for a time period (today, yesterday, etc.)
  * `timeclock/active` show the current task in the echo area

The function `timeclock/active-task-name` returns the name of the
active task or nil if none are active.

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
