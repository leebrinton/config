;;; emacs --- initialization file for all emacsen
;;; Commentary:
;;; emacsen agnostic init file
;;; Code:

(let* ((running-xemacs (string-match "XEmacs" emacs-version))
       (running-gnu-emacs (not running-xemacs))
       
       (version-init-file
        (if running-xemacs
            (expand-file-name "xemacs-init"
                              (concat (getenv "HOME") "/config"))
          (expand-file-name "gnu-emacs"
                            (concat (getenv "HOME") "/config")))))

  
  ;; config changes made through the customize UI will be stored here
  (setq custom-file
        (if running-xemacs
            (expand-file-name "xemacs-custom"
                              (concat (getenv "HOME") "/config/emacs-init"))
          (expand-file-name "gnu-custom.el"
                            (concat (getenv "HOME") "/config/emacs-init"))))
                   
  (load-file version-init-file)
  (if custom-file (load-file custom-file)))

;;; emacs ends here
