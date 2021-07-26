;;; bf-config-prog-packages.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Install and configure programming packages.

;;; Code:
(require 'use-package)

(defun bf-config--prog-packages--eldoc ()
  "Install and configure package `eldoc'."
  (use-package eldoc
    :diminish))

(defun bf-config--prog-packages--ws-butler ()
  "Install and configure package `ws-butler'."
  (use-package ws-butler
    :ensure t
    :diminish
    :hook
    (prog-mode . ws-butler-mode)))

(defun bf-config--prog-packages--company ()
  "Install and configure package `company'."
  (use-package company
    :ensure t
    :diminish
    :hook
    ((prog-mode gud-mode inferior-python-mode) . company-mode)
    :config
    (push 'company-capf company-backends)
    (progn
      (define-key company-active-map (kbd "C-n") 'company-select-next)
      (define-key company-active-map (kbd "C-p") 'company-select-previous))
    :bind
    ("M-;" . company-complete)))

(defun bf-config--prog-packages--c-cpp--ccls ()
  "Install and configure package `ccls'."
  (use-package ccls
    :ensure t))

(defun bf-config--prog-packages--rust--rustic ()
  "Install and configure package `rustic'."
  (use-package rustic
    :ensure t))

(defun bf-config--prog-packages ()
  "Install and configure all programming packages."
  (bf-config--prog-packages--eldoc)
  (bf-config--prog-packages--ws-butler)
  (bf-config--prog-packages--company)
  (bf-config--prog-packages--c-cpp--ccls)
  (bf-config--prog-packages--rust--rustic))

(provide 'bf-config-prog-packages)
;;; bf-config-prog-packages.el ends here
