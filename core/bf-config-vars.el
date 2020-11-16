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

(defun bf-config--vars--validate-vars ()
  "Validate variables.")

(provide 'bf-config-vars)
;;; bf-config-vars.el ends here
