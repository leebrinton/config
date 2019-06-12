
;;; init-package.el --- Initialize load-path -*- lexical-binding: t; -*-

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
;;;    Setup the Emacs package system

;;; Code:

;;=============================================================================
;; Setup packages
;;
;; M-x list-packages
;;=============================================================================
(require 'cl)

(progn
  (require 'package)
  ; (add-to-list
  ;  'package-archives
  ;  '("marmalade" . "http://marmalade-repo.org/packages/"))

  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/"))

  (add-to-list
   'package-archives
   '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

  (setq package-enable-at-startup nil)
  (package-initialize))

  (defvar required-packages
    '(ace-jump-buffer
      ace-window
      ag
      ahg
      anzu
      auto-complete
      browse-kill-ring
      cmake-mode
      cobol-mode
      company
      csharp-mode
      csv-mode
      diff-hl
      diminish
      ;dired+
      discover-my-major
      easy-kill
      elisp-slime-nav
      evil
      expand-region
      fill-column-indicator
      flx-ido
      flycheck
      flycheck-kotlin
      flycheck-perl6
      ggtags
      git-timemachine
      gitconfig
      gitignore-mode
      go-mode
      go-snippets
      gradle-mode
      groovy-mode
      guide-key
      helm
      helm-projectile
      htmlize
      ido-completing-read+
      ido-vertical-mode
      jedi
      js2-mode
      json-mode
      kotlin-mode
      lua-mode
      magit
      markdown-mode
      move-text
      perl6-mode
      projectile
      rainbow-mode
      rg
      smex
      ;tidy
      undo-tree
      volatile-highlights
      web-mode
      yasnippet
      zenburn-theme)
  "a list of packages to ensure are installed at launch.")

;;      smartparens

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'init-package)
;;; init-package.el ends here
