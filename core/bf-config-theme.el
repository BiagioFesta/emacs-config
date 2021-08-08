;;; bf-config-theme.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Configuration for theme.

;;; Code:
(require 'bf-config-utilities)

(defun bf-config--theme--advicing-load-theme ()
  "Advice `load-theme' to disable all active themes before applying the new one."
  (defadvice load-theme (before disable-themes-first activate)
    "Invoke `bf-disable-all-active-themes' in order to disable active themes."
    (bf-disable-all-active-themes)))

(defun bf-config--theme--doom-themes ()
  "Install and configure `doom-themes' package."
  (use-package doom-themes
    :ensure t))

(defun bf-config--theme--monokai-theme ()
  "Install and configure `monokai-theme' package."
  (use-package monokai-theme
    :ensure t))

(defun bf-config--theme ()
  "Apply theme configuration."
  (bf-config--theme--advicing-load-theme)
  (bf-config--theme--doom-themes)
  (bf-config--theme--monokai-theme))

(provide 'bf-config-theme)
;;; bf-config-theme.el ends here
