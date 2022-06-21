;;; bf-config-vars.el --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Variables used in the configuration.

;;; Code:

(defvar bf-config-packages-management-use-ssl t
  "Use the secure protocol for packages download.")

(defvar bf-config-packages-management-archives
  '(("gnu" . "elpa.gnu.org/packages/")
    ("melpa" . "melpa.org/packages/"))
  "Associative list with repositories for packages download.")

(defvar bf-config-general-settings-make-backups nil
  "Enable backup files functionality.")

(defvar bf-config-general-settings-size-big-file 1048576
  "Define the size of a big file in terms of num of chars.")

(defvar bf-config-general-settings-lock-big-files t
  "When file is too big, open in read-only mode.")

(defvar bf-config-general-settings-auto-revert-mode nil
  "Whether to enable the auto-revert mode globally.")

(defvar bf-config-general-settings-auto-revert-mode-interval nil
  "The auto-revert mode timer. It is a wrapper to `auto-revert-interval'.")

(defvar bf-config-general-settings-windows-split-width-threshold 180
  "This is going to set the variable `split-width-threshold'.")

(defvar bf-config-general-settings-quick-file nil
  "Path to a local file which might be open with a shortcut.
This should be a filename used as argument for the function call `find-file'.")

(defvar bf-config-prog-settings-c/c++-default-style "google"
  "The default c-style for `c-mode' and `c++-mode'.
You can see a list of defined c-styles at `c-style-alist'.")

(defvar bf-config-general-packages-evil nil
  "Enable evil-mode.")

(defvar bf-config-general-packages-project-ignore-regex '(".*\\.cargo/.*"
                                                          ".*\\.rustup/.*"
                                                          "/tmp/.*")
  "List of regex for ignoring `projectile-known-projects'.")

(defvar bf-config-theme-with-display nil
  "Theme to enable when graphical display is enabled.")

(defvar bf-config-theme-with-console nil
  "Theme to enable when graphical display is not enabled.")

(defvar bf-config-theme-font nil
  "Font to set when graphical display is enabled.")

(provide 'bf-config-vars)
;;; bf-config-vars.el ends here
