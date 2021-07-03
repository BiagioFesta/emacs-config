;;; bf-config-general-packages.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Install and configure general packages.

;;; Code:
(require 'use-package)
(require 'bf-config-vars)

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

(defun bf-config--general-packages--undo-tree ()
  "Install and configure `undo-tree' package."
  (use-package undo-tree
    :ensure t
    :diminish
    :config
    (global-undo-tree-mode 1)
    :bind
    ("C-c z" . undo-tree-visualize)))

(defun bf-config--general-packages--evil-mode ()
  "Install and configure `evil' package."
  (use-package evil
    :ensure t
    :config
    (when bf-config-general-packages-evil
      (evil-mode 1))))

(defun bf-config--general-packages ()
  "Install and configure all general packages."
  (bf-config--general-packages--dimish)
  (bf-config--general-packages--which-key)
  (bf-config--general-packages--undo-tree)
  (bf-config--general-packages--evil-mode))

(provide 'bf-config-general-packages)
;;; bf-config-general-packages.el ends here
