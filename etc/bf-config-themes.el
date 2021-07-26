;;; bf-config-themes.el  --- Emacs Configuration
;;; Commentary:
;;;   Configure and load the themes.

;;; Code:
(require 'bf-custom-vars)
(require 'bf-config-packages-management)
(require 'doom-modeline)

(defvar bf-config-themes-list-custom-presets nil
  "A list of custom presets can be loaded.")

(defun bf-config-themes--install-all-the-icons ()
  "Install `all-the-icons' packages and launch fonts installation."
  (bf-install-package-if-missing 'all-the-icons)
  (let ((placeholder-file-installed-fonts
         (concat user-emacs-directory "/placeholder-installed-fonts")))
    (unless (or (file-exists-p placeholder-file-installed-fonts)
                (not (display-graphic-p)))
      (all-the-icons-install-fonts t)
      (write-region
       "Placeholder file. Checks all-the-icons fonts are already installed."
       nil
       placeholder-file-installed-fonts))))

(defun bf-config-themes--install-themes-packages ()
  "Install theme packages and dependencies."
  (bf-install-package-if-missing 'monokai-theme)
  (bf-install-package-if-missing 'zerodark-theme)
  (bf-install-package-if-missing 'cyberpunk-theme)
  (bf-install-package-if-missing 'cyberpunk-2019-theme)
  (bf-install-package-if-missing 'doom-themes)
  (bf-install-package-if-missing 'doom-modeline)
  (bf-install-package-if-missing 'vscode-dark-plus-theme)
  (bf-config-themes--install-all-the-icons))

(defun bf-config-themes-disable-all-themes ()
  "Invoke the function `disable-theme' on all custom themes enabled.
Enabled themes are read from the variable `custom-enabled-themes'.
Moreover it disables additional graphical/theme modes (e.g. modeline theme)."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (bf-config-themes--doom-modeline -1)
  (setq-default cursor-type t))

(defun bf-config-themes--set-frame-font-if-possible (font-name)
  "Pass FONT-NAME as argument to `set-frame-font'.
However, before it checks the font-name exists.
If the font does not exist it throws a warning."
  (when (display-graphic-p)
    (if (find-font (font-spec :name font-name))
        (set-frame-font font-name)
      (warn (format "Required font '%s' not found." font-name)))))

(defun bf-config-themes--doom-modeline (ARG)
  "Enable or disable `doom-modeline-mode' (Enable when ARG is positive).
Moreover sets customization."
  (if (> ARG 0)
      (progn
        (setq doom-modeline-height 18)
        (setq doom-modeline-unicode-fallback t)
        (doom-modeline-mode 1))
    (doom-modeline-mode -1)))

(defun bf-config-themes--Monokai-preset ()
  "Load the preset `monokai'."
  (bf-config-themes-disable-all-themes)
  (load-theme 'monokai t)
  (bf-config-themes--set-frame-font-if-possible "Inconsolata 10")
  (set-face-background 'hl-line "black"))

(defun bf-config-themes--Doom-preset ()
  "Load the preset `doom'."
  (bf-config-themes-disable-all-themes)
  (load-theme 'doom-one t)
  (bf-config-themes--doom-modeline 1)
  (bf-config-themes--set-frame-font-if-possible "Source Code Pro-10"))

(defun bf-config-themes--VSCode-preset ()
  "Load the preset `vscode'."
  (bf-config-themes-disable-all-themes)
  (load-theme 'vscode-dark-plus t)
  (setq-default cursor-type '(bar . 1))
  (bf-config-themes--set-frame-font-if-possible "Cascadia Code 10"))

(defun bf-config-themes-load-preset (preset)
  "Load a custom 'theme-preset' defined in the configuration.
PRESET is the preset to load:
    - monokai;
    - doom;
    - vscode.

For a complete list of preset see `bf-config-themes-list-custom-presets'."
  (interactive
   (list (completing-read
          "Theme Names: " bf-config-themes-list-custom-presets nil t)))
  (when (stringp preset) (setq preset (intern preset)))
  (let ((load-preset-fn (alist-get preset bf-config-themes-list-custom-presets)))
    (if load-preset-fn
        (funcall load-preset-fn)
      (error (format "The theme-preset to load not found: %s" preset)))))

(defun bf-config-themes--generate-list-preset ()
  "Build the list of custom present.
That is, set the value for `bf-config-themes-list-custom-presets'."
  (setq bf-config-themes-list-custom-presets
        (list (cons 'monokai #'bf-config-themes--Monokai-preset)
              (cons 'doom #'bf-config-themes--Doom-preset)
              (cons 'vscode #'bf-config-themes--VSCode-preset))))

(defun bf-config-themes()
  "Configure and load custom themes."
  (bf-config-themes--generate-list-preset)
  (if (and bf-config-themes-custom-themes (not bf-config-minimal-config))
      (progn
        (bf-config-themes--install-themes-packages)
        (bf-config-themes--install-all-the-icons)
        (bf-config-themes-load-preset bf-config-themes-default-preset))
    (bf-config-themes-disable-all-themes)))

(provide 'bf-config-themes)
;;; bf-config-themes.el ends here
