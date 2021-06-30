;;; bf-config-vars.el --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Variables used in the configuration.

;;; Code:

(defvar bf-config-packages-management-use-ssl t
  "Use the secure protocol for packages download.")

(defvar bf-config-packages-management-archives
  '(("gnu" . "elpa.gnu.org/packages/")
    ("melpa" . "melpa.org/packages/"))
  "Associative list with repositories for packages download.")

(defvar bf-config-general-settings-make-backups nil
  "Enable backup files functionality.")

(defvar bf-config-general-settings-size-big-file 1048576
  "Define the size of a big file in terms of num of chars.")

(defvar bf-config-general-settings-lock-big-files t
  "When file is too big, open in read-only mode.")

(defvar bf-config-general-settings-auto-revert-mode nil
  "Whether to enable the auto-revert mode globally.")

(defvar bf-config-general-settings-auto-revert-mode-interval nil
  "The auto-revert mode timer. It is a wrapper to `auto-revert-interval'.")

(provide 'bf-config-vars)
;;; bf-config-vars.el ends here
