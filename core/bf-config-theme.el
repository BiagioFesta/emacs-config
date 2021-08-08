;;; bf-config-theme.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Configuration for theme.

;;; Code:
(require 'bf-config-utilities)

(defun bf-config--theme--advicing-load-theme ()
  "Advice `load-theme' to disable all active themes before applying the new one."
  (defadvice load-theme (before disable-themes-first activate)
    (bf-disable-all-active-themes)))

(defun bf-config--theme ()
  "Apply theme configuration."
  (bf-config--theme--advicing-load-theme))

(provide 'bf-config-theme)
;;; bf-config-theme.el ends here
