;;; init-edit.el --- Initialize load-path -*- lexical-binding: t; -*-

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
;;;   Editor settings

;;; Code:

;;=============================================================================
;; Use space instead of tabs
;;=============================================================================
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;;=============================================================================
;; always end a file with a newline
;;=============================================================================
(setq require-final-newline 'query)
(setq-default indicate-empty-lines t)

;;=============================================================================
;; Don't automatically add newlines when scrolling
;;=============================================================================
(setq-default next-line-add-newlines nil)

;;=============================================================================
;; Revert buffers automatically when underlying files are changed externally
;;=============================================================================
(global-auto-revert-mode t)

;;=============================================================================
;; Make a shell script executable automatically on save
;;=============================================================================
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;=============================================================================
;; Delete the selection with a keypress; I think cua-mode enables this ...
;;=============================================================================
;;(delete-selection-mode t)

;;=============================================================================
;; Setup saveplace, save point location in buffers
;;=============================================================================
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;;=============================================================================
;; savehist keeps track of some history
;;=============================================================================
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" user-emacs-directory))

(savehist-mode +1)

;;=============================================================================
;; Setup recentf - recent files menu
;;=============================================================================
(require 'recentf)

(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(add-to-list 'recentf-exclude "/tmp/")
(add-to-list 'recentf-exclude "/ssh:")

(recentf-mode t)

;;=============================================================================
;; Setup backups - write all backup files to ~/.emacs.d/backups
;;=============================================================================
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))

(setq delete-by-moving-to-trash t)

;;=============================================================================
;; use the X clipboard
;;=============================================================================
(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t)

;;=============================================================================
;; Enable disabled commands
;;=============================================================================
;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;;=============================================================================
;; easy-kill
;;=============================================================================
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;;=============================================================================
;; Setup expand-region
;;=============================================================================
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;=============================================================================
;; Setup browse-kill-ring - kill ring navigation
;;=============================================================================
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Make M-v invoke browse-kill-ring
(global-set-key [remap cua-repeat-replace-region] 'browse-kill-ring)

(setq browse-kill-ring-highlight-current-entry t
      browse-kill-ring-highlight-inserted-item t)

;;=============================================================================
;; Use the undo tree system
;; C-/       undo-tree-undo
;; C-?       undo-tree-redo
;; C-x u     undo--tree-visualize
;;=============================================================================
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;=============================================================================
;; Show me matching parens
;;=============================================================================
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;;=============================================================================
;; Type ), ] and } for me
;;=============================================================================
(electric-pair-mode t)

;; (defun prelude-wrap-with (s)
;;   "Create a wrapper function for smartparens using S."
;;   `(lambda (&optional arg)
;;      (interactive "P")
;;      (sp-wrap-with-pair ,s)))

;; (unless (fboundp 'cua-replace-region)
;;   (defun cua-replace-region ()
;;     "Replace the active region with the character you type."
;;     (interactive)
;;     (let ((not-empty (and cua-delete-selection (cua-delete-region))))
;;       (unless (eq this-original-command this-command)
;;         (let ((overwrite-mode
;;                (and overwrite-mode
;;                     not-empty
;;                     (not (eq this-original-command 'self-insert-command)))))
;;           (cua--fallback))))))

;; (require 'smartparens-config)
;; (setq sp-base-key-bindings 'paredit)
;; (setq sp-autoskip-closing-pair 'always)
;; (setq sp-hybrid-kill-entire-symbol nil)
;; (sp-use-paredit-bindings)
;; (show-smartparens-global-mode +1)
;; (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
;; (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))
;; (define-key prog-mode-map (kbd "M-{") (prelude-wrap-with "{"))

;; ;; disable annoying blink-matching-paren
;; (setq blink-matching-paren nil)

(defun my-prog-mode-defaults ()
  "Defaults for all programming modes"
  (when (executable-find ispell-program-name)
    (flyspell-prog-mode)))
  ;; (smartparens-mode +1))

(setq my-prog-mode-hook 'my-prog-mode-defaults)
(add-hook 'prog-mode-hook (lambda ()
                           (run-hooks 'my-prog-mode-hook)))


(with-eval-after-load 'evil-core
  (setq evil-default-state 'emacs
        evil-emaacs-state-modes '(bs-show)
        evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-normal-state-modes '(text-mode prog-mode fundamental-mode
                                            css-mode conf-mode
                                            TeX-mode LaTeX-mode
                                            diff-mode)))

(provide 'init-edit)
;;; init-edit.el ends here
