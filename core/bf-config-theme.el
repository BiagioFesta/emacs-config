;;; bf-config-theme.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Configuration for theme.

;;; Code:
(require 'bf-config-utilities)
(require 'bf-config-vars)

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

(defun bf-config--theme--load-theme ()
  "Load the theme in both graphical display and console mode."
  (let ((theme (if (display-graphic-p) bf-config-theme-with-display
                 bf-config-theme-with-console)))
    (when theme (load-theme theme t))))

(defun bf-config--theme--set-font ()
  "Load font in graphical display."
  (when (and bf-config-theme-font (display-graphic-p))
    (set-frame-font bf-config-theme-font)))

(defun bf-config--theme--daemon ()
  "Setup theme when running Emacs as daemon."

  (defun bf-config--theme--daemon--terminal-no-bg (FRAME)
    (unless (display-graphic-p FRAME)
      (set-face-background 'default "unspecified-bg" FRAME)))

  (defun bf-config--theme--daemon--gui-set-font (FRAME)
    (when (display-graphic-p FRAME)
      (set-frame-font bf-config-theme-font nil (list FRAME))))

  (add-hook 'after-make-frame-functions 'bf-config--theme--daemon--terminal-no-bg)
  (add-hook 'after-make-frame-functions 'bf-config--theme--daemon--gui-set-font))

(defun bf-config--theme ()
  "Apply theme configuration."
  (bf-config--theme--advicing-load-theme)
  (bf-config--theme--doom-themes)
  (bf-config--theme--monokai-theme)
  (bf-config--theme--load-theme)
  (bf-config--theme--set-font)
  (bf-config--theme--daemon)
  (setq-default indicate-buffer-boundaries t))

(provide 'bf-config-theme)
;;; bf-config-theme.el ends here
