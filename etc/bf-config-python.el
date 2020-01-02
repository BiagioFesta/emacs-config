;;; bf-config-python.el  --- Emacs Configuration
;;; Commentary:
;;;   Setup Python mode.

;;; Code:
(require 'use-package)

(defun bf-config-python--pyvenv ()
  "Install and configure package `pyvenv'."
  (use-package pyvenv
    :ensure t))

(defun bf-config-python ()
  "Setup environment fpr mode python."
  (bf-config-python--pyvenv)
  nil)

(provide 'bf-config-python)
;;; bf-config-python.el ends here
