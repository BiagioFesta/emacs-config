;;; bf-config-prog-packages.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Install and configure programming packages.

;;; Code:
(require 'use-package)

(defun bf-config--prog-packages--ws-butler ()
  "Install and configure package `ws-butler'."
  (use-package ws-butler
    :ensure t
    :diminish
    :hook
    (prog-mode . ws-butler-mode)))

(defun bf-config--prog-packages ()
  "Install and configure all programming packages."
  (bf-config--prog-packages--ws-butler))

(provide 'bf-config-prog-packages)
;;; bf-config-prog-packages.el ends here
