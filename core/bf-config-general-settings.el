;;; bf-config-general-settings.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   General configuration settings.
;;;   Typically, not buffer local but global.

;;; Code:
(require 'bf-config-vars)

(defun bf-config--general-settings--config-backup-files ()
  "Configure backup files.
Use the variable `bf-config-general-settings-make-backups'
to enable (t) or disable (nil) backup functionality."
  (if bf-config-general-settings-make-backups
      (let ((backup-directory
             (concat temporary-file-directory ".emacs_backup_files/")))
        (setq backup-by-copying-when-linked t)
        (setq backup-directory-alist `(("." . ,backup-directory)))
        (setq version-control t)
        (setq delete-old-versions t)
        (setq kept-new-versions 6)
        (setq kept-old-versions 2)
        (setq make-backup-files t))
    (setq make-backup-files nil)))

(defun bf-config--general-settings ()
  "Apply all general configuration settings."
  (bf-config--general-settings--config-backup-files))

(provide 'bf-config-general-settings)
;;; bf-config-general-settings.el ends here
