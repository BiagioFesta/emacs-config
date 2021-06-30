;;; early-init.el --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Early Init configuration.

;;; Code:

(defun bf-config--early-init--config-bars ()
  "Configure interface bars and menu."
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))

(defun bf-config--early-init--config-modeline ()
  "Configure some apperances for the modeline."
  (column-number-mode 1))

(defun bf-config--early-init ()
  "Launch early-init configuration."
  (bf-config--early-init--config-bars)
  (bf-config--early-init--config-modeline)
  (setq inhibit-startup-screen t))

(bf-config--early-init)
;;; early-init.el ends here
