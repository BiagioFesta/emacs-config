;;; bf-config-vars.el --- Emacs Configuration
;;; Commentary:
;;;   Variables used in the configuration.

;;; Code:

(defvar bf-config-packages-management-use-ssl t
  "Use the secure protocol for packages download.")

(defvar bf-config-packages-management-archives
  '(("gnu" . "elpa.gnu.org/packages/")
    ("melpa" . "melpa.org/packages/"))
  "Associative list with repositories for packages download.")

(defvar bf-config-general-settings-make-backups t
  "Enable backup files functionality.")

(defvar bf-config-general-settings-size-big-file 1048576
  "Define the size of a big file in terms of num of chars.")

(defvar bf-config-general-settings-lock-big-files t
  "When file is too big, open in read-only mode.")

(defvar bf-config-general-settings-no-line-number-big-files t
  "When file is too big, do not display line number anyway.")

(defvar bf-config-general-settings-hl-line t
  "Enable highlight of current line.")

(defvar bf-config-general-settings-fill-column-indicator t
  "Enable highlight column rule on limit.")

(defvar bf-config-general-settings-display-line-number nil
  "Enable display of line number on buffer side.")

(defvar bf-config-general-settings-tab-width 2
  "Simple wrapper for `tab-width'.")

(defun bf-config--vars--validate-vars ()
  "Validate configuration variables.")

(provide 'bf-config-vars)
;;; bf-config-vars.el ends here
