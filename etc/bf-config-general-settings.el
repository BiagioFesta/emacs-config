;;; bf-config-general-settings.el  --- Emacs Configuration
;;; Commentary:
;;;   General configuration settings.
;;;   Typically, not buffer local but global.

;;; Code:
(require 'saveplace)
(require 'bf-custom-vars)
(require 'compile)
(require 'gdb-mi)
(require 'ansi-color)
(require 'scroll-bar)

(defun bf-config-general-settings--config-bar()
  "Configure general bars."
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))

(defun bf-config-general-settings--disable-line-numbers-large-file ()
  "Disable minor mode `display-line-numbers-mode' in the local buffer.
Only if the buffer size is too big."
  (let ((max-size bf-config-general-settings-size-big-file))
    (when (> (buffer-size) max-size)
      (display-line-numbers-mode -1))))

(defun bf-config-general-settings--config-line-numbers ()
  "Configure line numbers.
Use the variable `bf-config-general-settings-line-number'
to enable (t) or disable (nil) line numbers functionality."
  (if (and bf-config-general-settings-line-number
           (not bf-config-minimal-config))
      (progn
        (add-hook
         'find-file-hook
         #'bf-config-general-settings--disable-line-numbers-large-file)
        (global-display-line-numbers-mode 1))
    (global-display-line-numbers-mode -1)
    (remove-hook 'find-file-hook
                 #'bf-config-general-settings--disable-line-numbers-large-file)))

(defun bf-config-general-settings--config-backup-files ()
  "Configure backup files.
Use the variable `bf-config-general-settings--config-backup-files'
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

(defun bf-config-general-settings--config-hl-line ()
  "Configure `hl-line-mode'.
Use the variable `bf-config-general-settings--config-hl-line'
to enable (t) or disable (nil) the highlight of current line."
  (if (and bf-config-general-settings-highlight-current-line
           (not bf-config-minimal-config))
      (global-hl-line-mode 1)
    (global-hl-line-mode -1)))

(defun bf-config-general-settings--config-save-place ()
  "Configure command `save-place-mode'.
Use the variable `bf-config-general-settings-save-place'
to enable (t) or disable (nil) save-place functionality."
  (if bf-config-general-settings-save-place
      (let ((saveplace-dir (concat user-emacs-directory "saveplace" )))
        (setq save-place-file saveplace-dir)
        (save-place-mode 1))
    (save-place-mode -1)))

(defun bf-config-general-settings--lock-if-big-buffer ()
  "If the current buffer's size is bigger than a threshold mark it read-only.
Otherwise it does nothing."
  (let ((max-size bf-config-general-settings-size-big-file))
    (when (> (buffer-size) max-size)
      (read-only-mode 1))))

(defun bf-config-general-settings--config-lock-big-file ()
  "Add an hook when opening files.
If the file is too big, it will be marked as read-only.
The hook is controlled by the variable
`bf-config-general-settings-lock-big-files'."
  (if bf-config-general-settings-lock-big-files
      (add-hook
       'find-file-hook
       #'bf-config-general-settings--lock-if-big-buffer)
    (remove-hook 'find-file-hook #'bf-config-general-settings--lock-if-big-buffer)))

(defun bf-config-general-settings--config-column-indicator ()
  "Configure the display for the fill-column-indicator.
To control this configuration use the variable
`bf-config-general-settings-column-indicator' to t or nil."
  (unless (version< emacs-version "27")
    (if (and bf-config-general-settings-column-indicator
             (not bf-config-minimal-config))
        (let ((column-limit 80))
          (setq-default display-fill-column-indicator-column column-limit)
          (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))
      (remove-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
      (global-display-fill-column-indicator-mode -1))))

(defun bf-config-general-settings--auto-revert-mode ()
  "Configure `auto-revert-mode'."
  (global-auto-revert-mode 1))

(defun bf-config-general-settings--global-keybind ()
  "Set the global key bindings."
  (global-set-key (kbd "<f5>") 'revert-buffer))

(defun bf-config-general-settings--enable-compilation-buffer-colors ()
  "Enable ANSI color in the *compilation* buffer."
  (add-hook 'compilation-filter-hook
            (lambda ()
              (ansi-color-apply-on-region compilation-filter-start (point)))))

(defun bf-config-general-settings--scroll-config ()
  "Configure the settings for scrolling."
  (if bf-config-general-settings-smooth-scrolling
      (setq scroll-conservatively 101)
    (setq scroll-conservatively 0)))

(defun bf-config-general-settings ()
  "Apply all general configuration settings."
  (setq inhibit-startup-screen t)
  (bf-config-general-settings--config-bar)
  (bf-config-general-settings--config-line-numbers)
  (bf-config-general-settings--config-backup-files)
  (bf-config-general-settings--config-hl-line)
  (bf-config-general-settings--config-save-place)
  (bf-config-general-settings--config-lock-big-file)
  (bf-config-general-settings--config-column-indicator)
  (bf-config-general-settings--auto-revert-mode)
  (bf-config-general-settings--global-keybind)
  (bf-config-general-settings--enable-compilation-buffer-colors)
  (bf-config-general-settings--scroll-config)
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
  (setq split-width-threshold 180)
  (put 'upcase-region 'disabled nil)
  (setq-default fill-column 80)
  (setq ring-bell-function 'ignore)
  nil)

(provide 'bf-config-general-settings)
;;; bf-config-general-settings.el ends here
