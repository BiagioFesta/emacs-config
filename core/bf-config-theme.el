;;; bf-config-theme.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Configuration for theme.

;;; Code:

(defun bf-config--theme--disable-all-active-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun bf-config--theme--advicing-load-theme ()
  "Advice `load-theme' to disable all active themes before applying the new one."
  (defadvice load-theme (before disable-themes-first activate)
    (bf-config--theme--disable-all-active-themes)))

(defun bf-config--theme ()
  "Apply theme configuration."
  (bf-config--theme--advicing-load-theme))

(provide 'bf-config-theme)
;;; bf-config-theme.el ends here
