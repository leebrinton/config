;;; init-version.el --- Initialize version vars -*- lexical-binding: t; -*-

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
;;; Code:

;;=============================================================================
;; Define variables to indicate which type and version emacs we are running
;;=============================================================================
(defvar running-xemacs (if (string-match "XEmacs" emacs-version) t nil))
(defvar running-gnu-emacs (not running-xemacs))

(defvar running-ntemacs (and running-gnu-emacs (eq system-type 'windows-nt)))
(defvar running-ntxemacs (and running-xemacs (eq system-type 'windows-nt)))

(defvar running-cygwin-xemacs (and running-xemacs (eq system-type 'cygwin)))

(defvar running-cygwin-gnuemacs (and running-gnu-emacs (eq system-type 'cygwin)))

(defvar running-darwin-gnuemacs (and running-gnu-emacs (eq system-type 'darwin)))

(defvar running-darwin-xemacs (and running-xemacs
                                   (eq system-type 'darwin)))
                                   
(defvar system-is-w32 (or running-ntemacs
			  running-ntxemacs running-cygwin-xemacs))

(defvar system-is-unix (not system-is-w32))

(provide 'init-version)
;;; init-version.el ends here
