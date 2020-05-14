;;; bf-config-python.el  --- Emacs Configuration
;;; Commentary:
;;;   Setup Python mode.

;;; Code:
(require 'use-package)

(defun bf-config-python--pyvenv ()
  "Install and configure package `pyvenv'."
  (use-package pyvenv
    :ensure t))

(defun bf-config-python--pipenv ()
  "Install and configure package `pipenv'."
  (use-package pipenv
    :ensure t))

(defun bf-config-python--company-jedi ()
  "Install and configure package `company-jedi'."
  (use-package company-jedi
    :ensure t
    :init
    (setq jedi:use-shortcuts t)
    :config
    (push 'company-jedi company-backends)
    :hook
    (python-mode . jedi:setup)))

(defun bf-config-python ()
  "Setup environment fpr mode python."
  (bf-config-python--pyvenv)
  (bf-config-python--pipenv)
  (bf-config-python--company-jedi)
  nil)

(provide 'bf-config-python)
;;; bf-config-python.el ends here
