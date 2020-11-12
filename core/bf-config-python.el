;;; bf-config-python.el  --- Emacs Configuration
;;; Commentary:
;;;   Configure development enviroment for Python language.

;;; Code:
(require 'use-package)

(defun bf-config--python--pyvenv ()
  "Install and configure package `pyvenv'."
  (use-package pyvenv
    :ensure t))

(defun bf-config--python--pipenv ()
  "Install and configure package `pipenv'."
  (use-package pipenv
    :ensure t))

(defun bf-config--python ()
  "Configure development enviroment for Python language."
  (bf-config--python--pyvenv)
  (bf-config--python--pipenv))

(provide 'bf-config-python)
;;; bf-config-python.el ends here
