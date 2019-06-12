;;; init-help.el --- Initialize load-path -*- lexical-binding: t; -*-

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
;;;   Help, info man settings.

;;; Code:

;;=============================================================================
;; Change INFOPATH to microsoft format for ntemacs
;;=============================================================================
(cond (running-ntemacs
       (setenv "INFOPATH"
               (concat
                ".;"
                "c:/cygwin/usr/local/info;"
                "c:/cygwin/usr/info;"
                "c:/cygwin/usr/local/share/info;"
                "c:/cygwin/usr/share/info;"
                "c:/cygwin/usr/autotool/devel/info;"
                "c:/cygwin/usr/autotool/stable/info;"
                "c:/cygwin"
                (getenv "EMACSHOME")
                "/info")))
      (t
       (setenv "INFOPATH"
               (concat
                ".:"
                "/opt/local/share/info:"
                "/usr/local/share/info:"
                "/usr/local/info:"
                "/usr/share/info:"
                "/usr/info:"
                (getenv "EMACSHOME")
                "/info"))))

;;(message (getenv "INFOPATH"))

;;=============================================================================
;; Use WoMan to read manual pages
;;=============================================================================
(autoload 'woman "woman"
  "Decode and browse a UN*X man page." t)

(autoload 'woman-find-file "woman"
  "Find, decode and browse a specific UN*X man-page file." t)

(autoload 'woman-dired-find-file "woman"
  "In dired, run the WoMan man-page browser on this file." t)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "W" 'woman-dired-find-file)))

(if running-ntemacs
    (setq woman-manpath
          (list
           "c:/cygwin/usr/share/man"
           "c:/cygwin/usr/X11R6/man"
           "c:/cygwin/usr/ssl/man"
           "c:/cygwin/usr/man"
           "c:/cygwin/usr/X11R6/share/man"
           "c:/cygwin/usr/local/man")))


(provide 'init-help)
;;; init-help ends here
