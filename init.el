;;; Configuration --- Emacs Configuration
;;; Commentary:
;;;   Emacs configuration by Biagio Festa

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "etc/"))
(add-to-list 'load-path (concat user-emacs-directory "etc/ext-packages/vline"))
(add-to-list 'load-path (concat user-emacs-directory "etc/ext-packages/col-highlight"))

(require 'bf-config-packages-management)
(bf-config-packages-management)

(require 'bf-basic-utilities)
(require 'bf-custom-vars)
(require 'bf-config-general-settings)
(require 'bf-config-basic-packages)
(require 'bf-config-themes)
(require 'bf-config-prog-packages)
(require 'bf-config-cpp)
(require 'bf-config-python)
(require 'bf-config-org-mode)
(require 'bf-config-html)

(defun bf-config-load-config ()
  "Load the configuration."
  (bf-config-general-settings)
  (bf-config-themes)
  (bf-config-basic-packages)
  (bf-config-prog-packages)
  (bf-config-cpp)
  (bf-config-python)
  (bf-config-org-mode)
  (bf-config-html))

(defun bf-config-reload-with-min-config ()
  "Set the variable `bf-config-minimal-config' to t.
Afterward reload the configuration."
  (interactive)
  (setq bf-config-minimal-config t)
  (bf-config-load-config))

(defun bf-config-reload-config ()
  "Interactive function for `bf-config-load-config'.
Reloads the configuration."
  (interactive)
  (bf-config-load-config))

(bf-config-load-config)

;;; init.el ends here
