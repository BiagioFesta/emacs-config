;;; bf-config-python.el  --- Emacs Configuration
;;; Commentary:
;;;   Setup Python mode.

;;; Code:
(require 'use-package)
(require 'bf-custom-vars)
(require 'bf-config-prog-packages)

(defun bf-config-python--pyvenv ()
  "Install and configure package `pyvenv'."
  (use-package pyvenv
    :ensure t))

(defun bf-config-python--pipenv ()
  "Install and configure package `pipenv'."
  (use-package pipenv
    :ensure t))

(defun bf-config-python--lsp ()
  "Configure LSP for python mode."
  (let ((enable (and (not bf-config-minimal-config)
                     bf-config-python-use-lsp)))
    (bf-config-prog-packages--lsp-config-hook 'python-mode-hook enable)))

(defun bf-config-python--company-jedi ()
  "Install and configure package `company-jedi'."
  (when bf-config-python-use-jedi
    (use-package company-jedi
      :ensure t
      :after company
      :init
      (setq jedi:use-shortcuts t)
      :config
      (push 'company-jedi company-backends)
      :hook
      (python-mode . jedi:setup))))

(defun bf-config-python ()
  "Setup environment fpr mode python."
  (bf-config-python--pyvenv)
  (bf-config-python--pipenv)
  (bf-config-python--lsp)
  (bf-config-python--company-jedi)
  nil)

(provide 'bf-config-python)
;;; bf-config-python.el ends here
