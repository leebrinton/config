;;; init-sql.el --- Initialize load-path -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Lee Brinton

;; Author: Lee Brinton <lbrinton@Lee-Brintons-MacBook-Pro-3.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;   Structured Query Language settings

;;; Code:

;;=============================================================================
;; Setup Sql mode
;;=============================================================================
(autoload 'sql-oracle "sql" "Interactive SQL mode." t)

(cond (running-ntemacs
       (setq sql-oracle-program "c:/instantclient_11_1/sqlplus"))
      (t
       (setq sql-oracle-program "/Users/lbrinton/bin/sqlplus")))

(add-hook 'sql-mode-hook
          (function (lambda ()
                      (sql-highlight-oracle-keywords))))

;; Making SQL*Plus line numbers disappear #####################################

;; SQL*Plus has an interesting feature: Whenever you type a line of input,
;; SQL*Plus  adds a line number to the beginning of the next line. This line
;; number is not part of the SQL command; it just allows you to refer to and
;; edit specific lines in your SQL command. SQL*Plus acts like the standard
;; text editor.
;; There are two possibilities now:
;; 1. If you enter SQL statements one at a time, you'll be fine. In this case
;; the rest of this section is of no interest to you.
;; 2. If you enter multi-line SQL statements by using C-j instead of RET
;; between lines (ie. using sql-accumulate-and-indent instead of
;; comint-send-input), you're in trouble: The error position doesn't match the
;; output anymore. 

;; Here is an example of the line number junk:
;; ...
;;   2    3    4       from v$parameter p, all_tables u
;;           *
;; ERROR at line 2:
;; ORA-00942: table or view does not exist
;; The following elisp function must be added to
;; comint-preoutput-filter-functions in order to strip the line numbers junk
;; from the output. 

(defun eat-sqlplus-junk (str)
  "Eat the line numbers SQL*Plus returns.
Put this on `comint-preoutput-filter-functions' if you are
running SQL*Plus.

If the line numbers are not eaten, you get stuff like this:
...
  2    3    4       from v$parameter p, all_tables u
          *
ERROR at line 2:
ORA-00942: table or view does not exist

The mismatch is very annoying."
  (interactive "s")
  (while (string-match " [ 1-9][0-9]  " str)
    (setq str (replace-match "" nil nil str)))
  str)

(defun install-eat-sqlplus-junk ()
  "Install `comint-preoutput-filter-functions' if appropriate.
Add this function to `sql-interactive-mode-hook' in your .emacs:
\(add-hook 'sql-mode-hook 'install-eat-sqlplus-junk)"
  (if (string= (car (process-command (get-buffer-process sql-buffer)))
               sql-oracle-program)
      (add-to-list 'comint-preoutput-filter-functions
                   'eat-sqlplus-junk)))

(add-hook 'sql-interactive-mode-hook 'install-eat-sqlplus-junk)
;;; end of sqlplus numbers ####################################################

(defun ora-playground ()
  "logon to oracle on playground"
  (interactive)
  (setq sql-user "sescouser")
  (setq sql-password "sescouser")
  (setq sql-database "playground")
  (sql-oracle))

(defun ora-prod ()
  "logon to oracle production"
  (interactive)
  (setq sql-user "lbrinton")
  (setq sql-password "sescouser")
  (setq sql-database "sescodb")
  (sql-oracle))

(defun ora-local ()
  "logon to local oracle db"
  (interactive)
  (setq sql-user "sescouser")
  (setq sql-password "sescouser")
  (setq sql-database "sescodev")
  (sql-oracle))

;;(setq sql-mysql-options "\"-C\" \"-t\" \"-f\" \"-n\"

(provide 'init-sql)
;;; init-sql.el ends here
