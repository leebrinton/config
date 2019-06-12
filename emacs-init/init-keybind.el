;;; init-keybind.el --- Initialize load-path -*- lexical-binding: t; -*-

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
;;;   Key bindings

;;; Code:

;;----------------------------------------------------------------------------
;; Move beginning of line
;;----------------------------------------------------------------------------
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)

;;----------------------------------------------------------------------------
;; Toggle mouse avoidance mode
;;----------------------------------------------------------------------------
(defun toggle-mouse-avoidance-mode ()
  "If mouse-avoidance-mode is on turn it off; If it is off turn it on"
  (interactive)
  (mouse-avoidance-mode  (if mouse-avoidance-mode 'none 'banish)))

;;----------------------------------------------------------------------------
;; Switch to previous buffer
;;----------------------------------------------------------------------------
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently visited buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;----------------------------------------------------------------------------
;; Find file using ido restricted to the recent files list
;;----------------------------------------------------------------------------
(defun prelude-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))

;;----------------------------------------------------------------------------
;; Open the current file in an external program
;;----------------------------------------------------------------------------
(defun prelude-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (start-process "prelude-open-with-process" nil program current-file-name)))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun my-rename-file (old-name new-name)
  (if (vc-backend old-name)
      (vc-rename-file old-name new-name)
    (rename-file old-name new-name t)))

(defun rename-this-file-and-buffer (new-name)
  (interactive "sNew Name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file! name" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (my-rename-file filename new-name))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun my-delete-file (filename)
  (if (vc-backend filename)
      (vc-delete-file filename)
    (delete-file filename)))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (my-delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Move current line to to of window
;;----------------------------------------------------------------------------
(defun line-to-top ()
  "Move current line to top of window."
  (interactive)
  (recenter 0))

;;----------------------------------------------------------------------------
;; Open line function like vi
;;----------------------------------------------------------------------------
(defun prelude-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun prelude-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

;;----------------------------------------------------------------------------
;; Indent region or buffer
;;----------------------------------------------------------------------------
(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;;=============================================================================
;; Define keys in the global map 
;;=============================================================================
;; a complement to the zap-to-char command, that doesn't eat up the target character
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;; It's all in the Meta
(when running-darwin-gnuemacs   
  (setq ns-function-modifier 'hyper)
  (autoload 'vkill "vkill" nil t))

(global-set-key [delete]         'delete-forward-char)
;; This is for Carbon Emacs
(global-set-key (kbd "<kp-delete>") 'delete-forward-char)
(global-set-key (kbd "<home>")      'prelude-move-beginning-of-line)
(global-set-key (kbd "<end>")       'end-of-line)
(global-set-key [(S-f1)]            'woman)
(global-set-key [(f2)]              'toggle-mouse-avoidance-mode)
(global-set-key [(f4)]              'point-to-register)
(global-set-key [(f5)]              'jump-to-register)
(global-set-key [(f6)]              'apply-macro-to-region-lines)
(global-set-key [(f7)]              'compile)
(global-set-key [(f8)]              'next-error)
(global-set-key [(S-f8)]            'previous-error)
(global-set-key [(f9)]              'copy-to-register)
(global-set-key [(C-f9)]            'copy-rectangle-to-register)
(global-set-key [(f10)]             'insert-register)
(global-set-key [(C-f10)]           'insert-register)
(global-set-key [(f11)]             'balance-windows)
(global-set-key [(f12)]             'find-tag-other-frame)
(global-set-key (kbd "C-a")         'prelude-move-beginning-of-line)
(global-set-key (kbd "C-s")         'isearch-forward-regexp)
(global-set-key (kbd "C-r")         'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")       'isearch-forward)
(global-set-key (kbd "C-M-r")       'isearch-backward)
(global-set-key (kbd "C-+")         'text-scale-increase)
(global-set-key (kbd "C--")         'text-scale-decrease)
(global-set-key (kbd "C-c b")       'switch-to-previous-buffer)
(global-set-key (kbd "C-c f")       'prelude-recentf-ido-find-file)
(global-set-key (kbd "C-c j")       'join-line)
(global-set-key (kbd "C-c J")       (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "C-c o")       'prelude-open-with)
(global-set-key (kbd "C-c r")       'rename-this-file-and-buffer)
(global-set-key (kbd "C-c D")       'delete-this-file)
;;(global-set-key (kbd "C-x b")       'ibuffer)
(global-set-key (kbd "C-x b")       'ace-jump-buffer)
(global-set-key (kbd "C-x C-b")     'bs-show)
(global-set-key (kbd "C-x g")       'magit-status)

(global-set-key (kbd "C-x m")       'eshell)
(global-set-key (kbd "C-x M")       (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x o")       'ace-window)
(global-set-key (kbd "C-x t")       'line-to-top)
(global-set-key (kbd "C-x w")       'whitespace-mode)
(global-set-key [(shift return)]    'prelude-open-line)
(global-set-key (kbd "C-M-\\")      'indent-region-or-buffer)
(global-set-key (kbd "C-x \\")      'align-regex)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "M-g")         'goto-line)
(global-set-key (kbd "M-z")         'zap-up-to-char)
(global-set-key (kbd "M-/")         'hippie-expand)
(global-set-key (kbd "s-w")         'mark-word)
(global-set-key (kbd "s-/")         'comment-dwim)

(global-set-key [(control shift up)]   'move-text-up)
(global-set-key [(control shift down)] 'move-text-down)
(global-set-key [(meta shift up)]      'move-text-up)
(global-set-key [(meta shift down)]    'move-text-down)

;;=============================================================================
;; Start proced (or vkill on darwin) in a similar manner to dired
;;=============================================================================
(let ((process-view (if running-darwin-gnuemacs 'vkill 'proced)))
  (global-set-key (kbd "C-x p") process-view))

;;----------------------------------------------------------------------------
;; Additional help bindings
;;----------------------------------------------------------------------------
;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command "P" 'cperl-perldoc)

;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;;=============================================================================
;; Load CUA key bindings
;;
;; C-z undo
;; C-x cut
;; C-c copy
;; C-v paste
;;=============================================================================
(cua-mode)

(provide 'init-keybind)
;;; init-keybind.el ends here

