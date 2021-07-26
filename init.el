;;; Configuration --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Emacs configuration by Biagio Festa

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "core/"))
(add-to-list 'load-path (concat user-emacs-directory "etc/"))

(require 'bf-config-custom-vars)
(bf-config--custom-vars)

(require 'bf-config-packages-management)
(bf-config--packages-management)

(require 'bf-config-general-settings)
(require 'bf-config-general-packages)
(require 'bf-config-prog-settings)
(require 'bf-config-prog-packages)
(require 'bf-config-theme)
(require 'bf-config-utilities)

(defun bf-config--load-config ()
  "Load the entire configuration."
  (bf-config--general-settings)
  (bf-config--general-packages)
  (bf-config--prog-settings)
  (bf-config--prog-packages)
  (bf-config--theme))

(bf-config--load-config)

;;; init.el ends here
