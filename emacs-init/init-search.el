;;; init-search.el --- Initialize load-path -*- lexical-binding: t; -*-

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
;;;   Settings and configuration related to searching for somthing.

;;; Code:

;;=============================================================================
;; Setup search
;;
;; anzu-query-replace
;; anzu-query-replace-regex
;; anzu-replace-at-cursor
;; anzu-replace-at-cursor-thing
;;=============================================================================
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode +1)

;;----------------------------------------------------------------------------
;; Activate occur easily inside isearch
;;----------------------------------------------------------------------------
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;;=============================================================================
;; Setup IDo
;;=============================================================================
(require 'ido)
(ido-mode t)

;; ;; Improved flex matching
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-everywhere nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-auto-merge-work-directories-length -1
      ido-file-extensions-order '(".java" ".sql" ".js" ".sh" ".el" ".xml")
      ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory))

(ido-mode 'buffer)

;; Vertical completion menu
(require 'ido-vertical-mode)
(ido-vertical-mode t)

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; ;; IDO support pretty much every where, including eclim-java-implement

;;(require 'ido-ubiquitous)
;;(ido-ubiquitous-mode +1)

; disable ido faces to see flx highlights
(setq ido-use-faces nil)

(global-set-key (kbd "C-x C-f")    'ido-find-file)


;; Setup smex which integrates M-x with ido
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;=============================================================================
;; Setup Helm
;;
;; C-c p h> to navigate a project in Helm.
;;=============================================================================
(require 'helm-config)
(require 'helm-projectile)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
;; discussion of these options.
(setq helm-split-window-in-side-p t
      helm-buffers-fuzzy-matching t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-ff-file-name-history-use-recentf t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "g") 'helm-do-grep)
(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "SPC") 'helm-all-mark-rings)

;;=============================================================================
;; Ack support
;;=============================================================================
;(require 'ack-and-a-half)
;(defalias 'ack 'ack-and-a-half)
;(defalias 'ack-same 'ack-and-a-half-same)
;(defalias 'ack-find-file 'ack-and-a-half-find-file)
;(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;;=============================================================================
;; Ripgrep support
;;
;; M-x rg
;; M-x rg-project - search in a project projectile, find-file-in-project or vc
;; M-x rg-literal - non regex search
;;
;; C-c s S - rg-save-search-as-name
;;=============================================================================
(require 'rg)

;;=============================================================================
;; Setup grep command
;;=============================================================================
(setq-default grep-highlight-matches t
              grep-scroll-output t)

;;----------------------------------------------------------------------------
;; Run grep on the current directory
;;   also see builtins rgrep, find-grep, lgrep
;;
;; -s   Suppress error messages about nonexistent or unreadable files
;; -i   Ignore case
;; -n   Show line numbers
;;----------------------------------------------------------------------------
(defun dir-grep ()
  "grep the whole directory for something. defaults to term at cursor position"
  (interactive)
  (let* ((default (thing-at-point 'symbol))
         (prompt (concat "grep for <" default "> ")) 

         (user-input (read-string prompt))
         (needle (or user-input default))
         (grep-for (if (equal needle "") default needle))
         (cmd (concat "egrep -s -i -n " grep-for " * /dev/null")))
    (grep cmd)))

;;=============================================================================
;; General project support
;;
;; C-c p a  Switch between files with the same name but different extensions
;; C-c p b  Switch to project buffer
;; C-c p d  Find directory
;; C-c p D  Open project in dired
;; C-c p e  Find recently visited file in project
;; C-c p f  Find file in project
;; C-c p F  Find file in known projects
;; C-c p k  Kill project buffers
;; C-c p p  Switch to project
;;
;; C-c p o   Multi-occur on project buffers
;; C-c p r   Replace in project
;; C-c p R   Regenerate [eg]tags
;; C-c p s g Search project files with grep
;; C-c p s a Search project files with ack
;; C-c p s s Search project files with ag
;;
;; C-c p c   Compile project
;; C-c p P   Test project
;; C-c p S   Save all project buffers
;;
;; C-c p !   Run shell command in poroject root dir
;;=============================================================================
(require 'projectile)
(projectile-global-mode t)
(setq projectile-enable-caching nil
      projectile-globally-ignored-directories '("target" "idea"))

;;=============================================================================
;; Saner regex syntax
;;
;; M-x re-builder
;; in re-builder:
;; C-c TAB   select the re syntax used
;; C-c C-e   sub-expression mode (only highlight capturing groups)
;; C-c C-i   toggle case sensitivity
;; C-c C-s   move to next match
;; C-c C-r   move to previous match
;; C-c C-w   copy the re to a string format suitable for use is elisp
;;=============================================================================
(require 're-builder)
(setq reb-re-syntax 'string)

(provide 'init-search)
;;; init-search.el ends here
