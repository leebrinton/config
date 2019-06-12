;;; init-vc.el --- Initialize load-path -*- lexical-binding: t; -*-

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
;;;   Settings and configuration related to version control.
;;; Code:

;;=============================================================================
;; Setup subversion mode
;;=============================================================================
;; Download from: http://www.xsteve.at/prg/emacs/psvn.el
;; Place in ~/.emacs.d/site-lisp/psvn/psvn.el
(require 'psvn)

(setq svn-status-verbose nil)
;; default to unified diffs
(setq diff-switches "-u")

(defun svncmdbuf (cmd &optional fn)
  "Run the given svn command on the current window."
  (interactive "scmd:")
  (let ((buf "*svn-cmd*"))
    (if (not fn)
        (setq fn (buffer-file-name)))
    (if (shell-command (format "svn %s %s" cmd fn) buf)
        (switch-to-buffer-other-window buf))))

(defun svndiff ()
  "Diff the current buffer with subversion."
  (interactive)
  (let
      ((fn (buffer-file-name)))
    (if (and fn (shell-command (format "svn diff %s" fn) "*svn-diff*"))
        (switch-to-buffer-other-window "*svn-diff*"))))
	
(defun svn (c)
  "Run svn command tool."
  (interactive "c: (a)dd (d)iff (D)iff revision (c)ommit (l)ock (r)esolve (R)evert (b)lame (u)p (x)delete")
  (case c
    (?a (svncmdbuf "add"))
    (?b (svncmdbuf "blame"))
    (?d (svncmdbuf "diff"))
    (?D (call-interactively 'svndiff))
    (?c (svncmdbuf (concat "commit -m \"" (read-string "checkin message:") "\"")))
    (?l (svncmdbuf "lock"))
    (?u (svncmdbuf "up"))
    (?r (svncmdbuf "resolved"))
    (?x (svncmdbuf "rm"))
    (?R (if (yes-or-no-p "revert?") (svncmdbuf "revert")))
    (t (message "unknown command key %c" c))))

;;=============================================================================
;; Setup magit mode (git integration)
;;=============================================================================
(require 'magit)
;;(autoload 'git-blame-mode "git-blame"
;;  "Minor mode for incremental blame for Git." t)
(require 'gitignore-mode)
(require 'gitconfig)
;;(require 'git-messenger)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

;;===========================================================================
;; Setup ahg mode (mercurial integration)
;;===========================================================================
(require 'ahg)

(provide 'init-vc)
;;; init-vc.el ends here
