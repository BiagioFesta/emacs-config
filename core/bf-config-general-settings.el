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

(defun bf-config--general-settings--lock-if-big-buffer ()
  "If the current buffer's size is bigger than a threshold mark it read-only.
Otherwise it does nothing."
  (let ((max-size bf-config-general-settings-size-big-file))
    (when (> (buffer-size) max-size)
      (read-only-mode 1))))

(defun bf-config--general-settings--config-big-buffers ()
  "Configure 'big' buffers.
The variable `bf-config-general-settings-size-big-file' defines the size of
big buffers."
  (if bf-config-general-settings-lock-big-files
      (add-hook 'find-file-hook
                #'bf-config--general-settings--lock-if-big-buffer)
    (remove-hook 'find-file-hook
                 #'bf-config--general-settings--lock-if-big-buffer)))

(defun bf-config--general-settings--global-keybind ()
  "Set the global key bindings."
  (global-set-key (kbd "<f5>") 'revert-buffer))

(defun bf-config--general-settings ()
  "Apply all general configuration settings."
  (bf-config--general-settings--config-backup-files)
  (bf-config--general-settings--config-big-buffers)
  (bf-config--general-settings--global-keybind))

(provide 'bf-config-general-settings)
;;; bf-config-general-settings.el ends here
