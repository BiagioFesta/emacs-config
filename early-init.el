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

(defun bf-config--early-init--config-scroll ()
  "Configure scrolling (mouse and keyboard)."
 (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
 (setq mouse-wheel-progressive-speed nil)
 (setq mouse-wheel-follow-mouse t)
 (setq scroll-step 1))

(defun bf-config--early-init ()
  "Launch early-init configuration."
  (bf-config--early-init--config-bars)
  (bf-config--early-init--config-modeline)
  (bf-config--early-init--config-scroll)
  (setq inhibit-startup-screen t)
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 10 1024 1024)))

(bf-config--early-init)
;;; early-init.el ends here
