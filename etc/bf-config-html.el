;;; bf-config-html.el --- Emacs Configuration
;;; Commentary:
;;;   Web development.

;;; Code:
(require 'use-package)

(defun bf-config-html--web-mode ()
  "Install and configure package `web-mode'."
  (use-package web-mode
    :ensure t))

(defun bf-config-html--impatient-mode ()
  "Install and configure package `impatient-mode'."
  (use-package impatient-mode
    :ensure t))

(defun bf-config-html ()
  "Setup environment for html web."
  (bf-config-html--web-mode)
  (bf-config-html--impatient-mode))

(provide 'bf-config-html)
;;; bf-config-html.el ends here
