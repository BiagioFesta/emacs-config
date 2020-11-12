;;; Configuration --- Emacs Configuration
;;; Commentary:
;;;   Emacs configuration by Biagio Festa

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "core/"))
(add-to-list 'load-path (concat user-emacs-directory "etc/"))

(require 'bf-config-set-vars)
(bf-config--set-vars)

(require 'bf-config-packages-management)
(bf-config--packages-management)

(require 'bf-config-general-settings)
(require 'bf-config-general-packages)
(require 'bf-config-python)

(defun bf-config--load-config ()
  "Load the entire configuration."
  (bf-config--custom-vars--validate-vars)
  (bf-config--general-settings)
  (bf-config--general-packages)
  (bf-config--python))

(bf-config--load-config)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(xclip ws-butler which-key use-package projectile pipenv magit helm flycheck expand-region evil diminish counsel company ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
