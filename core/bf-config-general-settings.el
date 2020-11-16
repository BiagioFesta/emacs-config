;;; bf-config-general-settings.el  --- Emacs Configuration
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

(defun bf-config--general-settings--no-line-number-if-big-buffer ()
  "If current buffer's size is big dont display line number on the buffer side."
  (let ((max-size bf-config-general-settings-size-big-file))
    (when (> (buffer-size) max-size)
      (display-line-numbers-mode -1))))

(defun bf-config--general-settings--config-big-buffers ()
  "Configure 'big' buffers.
The variable `bf-config-general-settings-size-big-file' defines the size of
big buffers."
  (if bf-config-general-settings-lock-big-files
      (add-hook 'find-file-hook
                #'bf-config--general-settings--lock-if-big-buffer)
    (remove-hook 'find-file-hook
                 #'bf-config--general-settings--lock-if-big-buffer))
  (if bf-config-general-settings-no-line-number-big-files
      (add-hook 'find-file-hook
                #'bf-config--general-settings--no-line-number-if-big-buffer)
    (remove-hook 'find-file-hook
                 #'bf-config--general-settings--no-line-number-if-big-buffer)))

(defun bf-config--general-settings--global-keybind ()
  "Set the global key bindings."
  (global-set-key (kbd "<f5>") 'revert-buffer))

(defun bf-config--general-settings--hl-line ()
  "Configure `hl-line-mode'.
Use the variable `bf-config-general-settings-hl-line'
to enable (t) or disable (nil) the highlight of current line."
  (if bf-config-general-settings-hl-line
      (global-hl-line-mode 1)
    (global-hl-line-mode -1)))

(defun bf-config--general-settings--fill-column-indicator ()
  "Configure the display for the fill-column-indicator.
To control this configuration use the variable
`bf-config-general-settings-fill-column-indicator' to t or nil."
  (if bf-config-general-settings-fill-column-indicator
      (global-display-fill-column-indicator-mode 1)
    (global-display-fill-column-indicator-mode -1)))

(defun bf-config--general-settings--display-line-numbers ()
  "Configure display of line numbers on the buffer side."
  (if bf-config-general-settings-display-line-number
      (global-display-line-numbers-mode 1)
    (global-display-line-numbers-mode -1)))

(defun bf-config--general-settings ()
  "Apply all general configuration settings."
  (bf-config--general-settings--config-backup-files)
  (bf-config--general-settings--config-big-buffers)
  (bf-config--general-settings--global-keybind)
  (bf-config--general-settings--hl-line)
  (bf-config--general-settings--fill-column-indicator)
  (bf-config--general-settings--display-line-numbers)
  (global-auto-revert-mode 1)
  (show-paren-mode 1)
  (delete-selection-mode 1)
  (put 'narrow-to-region 'disabled nil)
  (setq sentence-end-double-space nil)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq gc-cons-threshold 20000000)
  (column-number-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq compilation-scroll-output 'first-error)
  (setq gdb-display-io-nopopup t)
  (setq split-height-threshold nil)
  (setq split-width-threshold 200)
  (put 'upcase-region 'disabled nil)
  (setq-default fill-column 80)
  (setq ring-bell-function 'ignore)
  (setq enable-recursive-minibuffers t))

(provide 'bf-config-general-settings)
;;; bf-config-general-settings.el ends here
