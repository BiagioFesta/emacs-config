;;; bf-config-custom-vars.el --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Setting custom vars.
;;;   Custom configuration here.

;;; Code:
(require 'bf-config-vars)

(defun bf-config--custom-vars ()
  "Add custom variable settings here."
  (setq bf-config-general-packages-evil nil)
  (setq bf-config-theme-with-display 'doom-palenight)
  (setq bf-config-theme-with-console 'doom-palenight)
  (setq bf-config-theme-font "Source Code Pro:slant=normal"))

(provide 'bf-config-custom-vars)
;;; bf-config-custom-vars.el ends here
