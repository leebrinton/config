;;; init-ui.el --- Initialize load-path -*- lexical-binding: t; -*-

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
;;;   User Interface settings

;;; Code:

;;=============================================================================
;; Keep the mode line tidy
;;=============================================================================
(require 'diminish)

;;=============================================================================
;; Don't show the toolbar
;;=============================================================================
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode nil))

;;=============================================================================
;; Show me what line and column I am in (appears in the mode line)
;;=============================================================================
(line-number-mode t)
(column-number-mode t)
;; display line numbers to the right of the window
(global-linum-mode)

;;=============================================================================
;; Show me the current time
;;=============================================================================
(display-time)

;;=============================================================================
;; Make all yes or no prompts be y or n prompts
;;=============================================================================
(fset 'yes-or-no-p 'y-or-n-p)

;;=============================================================================
;; Don't beep at me
;;=============================================================================
(setq visible-bell t)

;;=============================================================================
;; Turn on syntax coloring always
;;=============================================================================
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;=============================================================================
;; Make emacs scroll one line at a time instead of by chunks
;;=============================================================================
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;=============================================================================
;; Highlight the current line
;;=============================================================================
(global-hl-line-mode t)

;;=============================================================================
;; Show me the fill column
;;
;; To toggle indication of the fill column in a buffer, use "M-x fci-mode".
;;=============================================================================
(require 'fill-column-indicator)
(setq fci-rule-column 79)

;;=============================================================================
;; support for my wheel mouse
;;=============================================================================
(when window-system
  (when (require 'mwheel nil 'no-error) (mouse-wheel-mode t)))
;;(if (not running-ntemacs) (mwheel-install))

;;=============================================================================
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;=============================================================================
(winner-mode 1)

;;=============================================================================
;; use utf-8
;;=============================================================================
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;=============================================================================
;; Highlight operations
;;=============================================================================
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;;=============================================================================
;; Show whitespace
;; C-x w                  - toggle whitespace mode
;; M-x whitespace-cleanup - let emacs clean up a buffer
;;=============================================================================
(require 'whitespace)

;;=============================================================================
;; Setup uniquify, better buffer names when the visited filename is not unique
;;=============================================================================
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;=============================================================================
;; Setup diff-hl highlighs uncommited changes 
;;=============================================================================
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
  (diff-hl-update))

;;=============================================================================
;; Setup guide-key
;; C-x a    abbrev
;; C-c p    projectile
;; C-x r    rectangles and registers
;; C-x v    version control
;;=============================================================================
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x a"  "C-c p" "C-x r" "C-x v"
        "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-c !")) 
(guide-key-mode 1)
(diminish 'guide-key-mode)

;;=============================================================================
;; Setup which-function-mode - display current function on mode line
;;=============================================================================
(require 'which-func)
(which-function-mode 1)

;;=============================================================================
;; Setup ace jump mode
;;=============================================================================
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(global-set-key (kbd "C-\"") 'avy-goto-word-1)

;;=============================================================================
;; Set the color theme
;;=============================================================================
(setq-default custom-enabled-themes '(zenburn))
(load-theme 'zenburn t)

;;(load-theme 'sanityinc-solarized-light t))
;;(load-theme 'tsdh-dark t))
;;(load-theme 'wombat t))
;;(load-theme 'wheatgrass t))
;;(load-theme 'deeper-blue t))

;;=============================================================================
;; Keep the mouse pointer out of my way
;;=============================================================================
;;(if window-system (mouse-avoidance-mode 'banish))


(provide 'init-ui)
;;; init-ui.el ends here
