;;; bf-custom-vars.el  --- Emacs Configuration
;;; Commentary:
;;;   Custom variables used in the configuration.

;;; Code:

(defvar bf-config-minimal-config nil
  "Set t for load a minimal configuration.
Some packages will not be enabled.")

(defvar bf-config-packages-management-use-ssl t
  "Use the secure protocol for packages download.")

(defvar bf-config-packages-management-archives
  '(("gnu" . "elpa.gnu.org/packages/")
    ("melpa" . "melpa.org/packages/"))
  "Associative list with repositories for packages download.")

(defvar bf-config-general-settings-line-number t
  "Enable configuration for line numbers.")

(defvar bf-config-general-settings-highlight-current-line t
  "Enable highlight of current line.")

(defvar bf-config-general-settings-make-backups nil
  "Enable backup files functionality.")

(defvar bf-config-general-settings-save-place t
  "Enable save-place functionality.")

(defvar bf-config-general-settings-lock-big-files t
  "When file is too big, open in read-only mode.")

(defvar bf-config-general-settings-column-indicator t
  "Enable highlight column rule on limit 80.")

(defvar bf-config-general-settings-smooth-scrolling t
  "Configure smooth scrolling.")

(defvar bf-config-themes-custom-themes t
  "Enable configuration of custom themes.")

(defvar bf-config-themes-default-preset 'monokai
  "Set the default preset theme to load during load configuration.
See `bf-config-themes-list-custom-presets' for a list of preset-themes.")

(defvar bf-config-prog-highlight-indent-guides nil
  "Enable highlight indent guides.")

(defvar bf-config-general-settings-size-big-file (* 1024 1024)
  "Define the size of a big file in terms of num of chars.")

(defvar bf-config-cpp-use-lsp t
  "Enable LSP for C/C++ modes.")

(defvar bf-config-cpp-use-ccls t
  "Enable ccls (C++ client) for LSP.")

(defvar bf-config-cpp-ccls-executable "/usr/bin/ccls"
  "Path for binary ccls (C++ client for LSP).")

(defvar bf-config-python-use-lsp nil
  "Enable LSP for python mode.")

(defvar bf-config-python-use-jedi t
  "Use JEDI for python mode.")

(provide 'bf-custom-vars)
;;; bf-custom-vars.el ends here
