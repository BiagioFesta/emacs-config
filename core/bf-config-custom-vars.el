;;; bf-config-custom-vars.el --- Emacs Configuration
;;; Commentary:
;;;   Custom variables used in the configuration.

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

(defvar bf-config-general-settings-hl-line t
  "Enable highlight of current line.")

(defvar bf-config-general-settings-fill-column-indicator t
  "Enable highlight column rule on limit.")

(defvar bf-config-general-packages-evil nil
  "Enable evil-mode.")

(defun bf-config--custom-vars--validate-vars ()
  "Validate variables.")

(provide 'bf-config-custom-vars)
;;; bf-config-custom-vars.el ends here
