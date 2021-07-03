;;; bf-config-general-packages.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Install and configure general packages.

;;; Code:
(require 'use-package)

(defun bf-config--general-packages--dimish ()
  "Install and configure `dimish' package."
  (use-package diminish
    :ensure t))

(defun bf-config--general-packages--which-key ()
  "Install and configure `which-key' package."
  (use-package which-key
    :ensure t
    :diminish
    :config
    (which-key-mode 1)))

(defun bf-config--general-packages ()
  "Install and configure all general packages."
  (bf-config--general-packages--dimish)
  (bf-config--general-packages--which-key))

(provide 'bf-config-general-packages)
;;; bf-config-general-packages.el ends here
