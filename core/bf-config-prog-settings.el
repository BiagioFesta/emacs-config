;;; bf-config-prog-settings.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Programming configuration settings.

;;; Code:
(require 'bf-config-vars)
(require 'cc-vars)

(defun bf-config--prog-settings---define-c-bf-style-1 ()
  "Define C/C++ style description: bf-style-1.
It also adds this style to the list of the available c styles."
  (defconst bf-config--prog-settings--c-bf-style-1-description
    `("google"
       (c-basic-offset . 4)))
  (c-add-style "bf-style-1" bf-config--prog-settings--c-bf-style-1-description))

(defun bf-config--prog-settings--set-default-c-style ()
  "Configure the default c-styles for different modes.
In short, it sets the variable `c-default-style'.
In particular, the assigned style for each mode is the following:
       - `c-mode' -> `bf-config-prog-settings-c/c++-default-style';
       - `c++-mode' -> `bf-config-prog-settings-c/c++-default-style'."
  (push (cons 'c-mode bf-config-prog-settings-c/c++-default-style) c-default-style)
  (push (cons 'c++-mode bf-config-prog-settings-c/c++-default-style) c-default-style))

(defun bf-config--prog-settings ()
  "Apply all programming configuration settings."
  (bf-config--prog-settings---define-c-bf-style-1)
  (bf-config--prog-settings--set-default-c-style)
  (setq-default indent-tabs-mode nil))

(provide 'bf-config-prog-settings)
;;; bf-config-prog-settings.el ends here
