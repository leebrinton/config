;;; init-path.el --- Initialize load-path -*- lexical-binding: t; -*-

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

;; 

;;; Code:

;;=============================================================================
;; Define functions
;;=============================================================================
;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun sanityinc/directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory
   (file-name-directory (find-library-name library-name))))

;----------------------------------------------------------------------------
;; Add sub-directories to load-path
;;----------------------------------------------------------------------------
(eval-when-compile (require 'cl))
(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (loop for dir in (directory-files parent-dir)
                   unless (string-match "^\\." dir)
                   collecting (expand-file-name dir))
             load-path)))))

;; Add ~/config to load-path
(setq load-path
     (append
      (list
       (expand-file-name "config" "~")
       (expand-file-name "emacs-init" (expand-file-name "config" "~")))
       load-path))

;; Add ~/.emacs.d/site-lisp sub-directories to load-path
(sanityinc/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

;; (cond (running-ntemacs
;;        (let ((local-site-lisp-path
;;               "c:/cygwin/usr/local/share/emacs/site-lisp"))
;;          (setq load-path
;;                (append
;;                 (list
;;                  local-site-lisp-path
;;                  (concat local-site-lisp-path "/elib-1.0"))
;;                 load-path))))
;;       (running-cygwin-gnuemacs
;;        (let ((local-site-lisp-path
;;               "/usr/local/share/emacs/site-lisp"))
;;          (setq load-path
;;                (append 
;;                 (list
;;                  local-site-lisp-path
;;                  (concat local-site-lisp-path "/elib")
;;                  (concat local-site-lisp-path "/jdee-2.4.0.1/lisp")
;;                  load-path)))))
;;        (t
;;         (let ((local-site-lisp-path "/usr/local/share/emacs/site-lisp")
;;               (opt-site-lisp-path "/usr/local/share/emacs/site-lisp"))
;;           (add-to-list 'load-path local-site-lisp-path)
;;           (add-to-list 'load-path (concat local-site-lisp-path "/elib"))
;;           (add-to-list 'load-path (concat local-site-lisp-path "/lua-mode"))
;;           (add-to-list 'load-path (concat local-site-lisp-path "/javadoc-lookup"))
;;           (when (file-exists-p opt-site-lisp-path)
;;             (add-to-list 'load-path opt-site-lisp-path)))))

(provide 'init-path)
;;; init-path.el ends here
