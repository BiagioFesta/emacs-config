;;; bf-config-prog-packages.el  --- Emacs Configuration
;;; Commentary:
;;;   Configure a bunch of packages for programming purpose.

;;; Code:
(require 'use-package)
(require 'bf-custom-vars)

(defun bf-config-prog-packages--projectile ()
  "Install and configure package `projectile'."
  (use-package projectile
    :ensure t
    :diminish
    :config
    (projectile-mode 1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(defun bf-config-prog-packages--magit ()
  "Install and configure package `magit'."
  (use-package magit
    :ensure t
    :bind
    ("C-x g" . magit-status)))

(defun bf-config-prog-packages--flycheck ()
  "Install and configure package `flycheck'."
  (use-package flycheck
    :ensure t
    :init
    (setq flycheck-emacs-lisp-load-path 'inherit)
    :config
    (global-flycheck-mode 1)))

(defun bf-config-prog-packages--company ()
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

(defun bf-config-prog-packages--highlight-indent-guides ()
  "Install and configure package `highlight-indent-guides'."
  (use-package highlight-indent-guides
    :ensure t
    :diminish
    :init
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive 'top)
    (setq highlight-indent-guides-character ?\|)
    (setq highlight-indent-guides-auto-character-face-perc 10)
    (setq highlight-indent-guides-auto-top-character-face-perc 50)
    :config
    (if (and (not bf-config-minimal-config)
             bf-config-prog-highlight-indent-guides)
        (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
      (remove-hook 'prog-mode-hook highlight-indent-guides-mode)
      (highlight-indent-guides-mode -1))
    (global-set-key (kbd "C-c \\") 'highlight-indent-guides-mode)))

(defun bf-config-prog-packages--expand-region ()
  "Install and configure package `expand-region'."
  (use-package expand-region
    :ensure t
    :bind
    ("M-h" . 'er/expand-region)
    ("M-H" . er/contract-region)))

(defun bf-config-prog-packages--focus ()
  "Install and configure package `focus'."
  (use-package focus
    :ensure t))

(defun bf-config-prog-packages--lsp ()
  "Install and configure packages:
* `spinner';
* `yasnippet';
* `lsp-mode';
* `lsp-ui'."
  (use-package spinner
    :ensure t)
  (use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :hook
    (prog-mode . yas-minor-mode))
  (use-package lsp-mode
    :ensure t
    :init
    (setq lsp-keymap-prefix "C-c l"))
  (use-package lsp-ui
    :ensure t
    :init
    (setq lsp-ui-sideline-enable nil)
    :bind
    ("C-c /" . lsp-ui-peek-find-references)))

(defun bf-config-prog-packages--lsp-config-hook (hook enable)
  "Enable or disable lsp function to HOOK.
HOOK is the hook to bind the `lsp' function.
ENABLE is a boolean (t or nil) which specifies if add or remove from the hook."
  (if enable
      (add-hook hook 'lsp)
    (remove-hook hook 'lsp)))

(defun bf-config-prog-packages--eldoc ()
  "Install and configure package `eldoc'."
  (use-package eldoc
    :diminish))

(defun bf-config-prog-packages--cmake-mode ()
  "Install and configure package `cmake-mode'."
  (use-package cmake-mode
    :ensure t))

(defun bf-config-prog-packages--ws-butler ()
  "Install and configure package `ws-butler'."
  (use-package ws-butler
    :ensure t
    :diminish
    :hook
    (prog-mode . ws-butler-mode)))

(defun bf-config-prog-packages--smartparens ()
  "Install and configure package `smartparens'."
  (use-package smartparens
    :ensure t
    :diminish
    :config
    (require 'smartparens-config)
    (setq sp-autodelete-opening-pair nil)
    (setq sp-autodelete-closing-pair nil)
    (setq sp-autodelete-pair nil)
    (setq sp-escape-quotes-after-insert nil)
    :hook
    (prog-mode . smartparens-mode)))

(defun bf-config-prog-packages--neotree ()
  "Install and configure package `neotree'."
  (use-package neotree
    :ensure t
    :config
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    :bind
    ("<f8>" . neotree-toggle)))

(defun bf-config-prog-packages--col-highlight ()
  "Configure packages `vline' and `col-highlight'."
  (use-package vline)
  (use-package col-highlight
    :config
    (setq col-highlight-period 3)
    :custom-face
    (col-highlight ((t (:background "black"))))
    :bind
    ("C-\\" . column-highlight-mode)))

(defun bf-config-prog-packages--docker-file-mode ()
  "Install and configure package `dockerfile-mode'."
  (use-package dockerfile-mode
    :ensure t))

(defun bf-config-prog-packages--origami ()
  "Install and configure package `origami'."
  (use-package origami
    :ensure t
    :bind
    (:map origami-mode-map
          ("C-c o o" . origami-recursively-toggle-node)
          ("C-c o r" . origami-reset)
          ("C-c o s" . origami-show-only-node))
    :hook
    (prog-mode . origami-mode)))

(defun bf-config-prog-packages ()
  "Install and configure all programming packages."
  (bf-config-prog-packages--projectile)
  (bf-config-prog-packages--magit)
  (bf-config-prog-packages--flycheck)
  (bf-config-prog-packages--company)
  (bf-config-prog-packages--highlight-indent-guides)
  (bf-config-prog-packages--expand-region)
  (bf-config-prog-packages--focus)
  (bf-config-prog-packages--lsp)
  (bf-config-prog-packages--eldoc)
  (bf-config-prog-packages--cmake-mode)
  (bf-config-prog-packages--ws-butler)
  (bf-config-prog-packages--smartparens)
  (bf-config-prog-packages--neotree)
  (bf-config-prog-packages--col-highlight)
  (bf-config-prog-packages--docker-file-mode)
  (bf-config-prog-packages--origami)
  nil)


(provide 'bf-config-prog-packages)
;;; bf-config-prog-packages.el ends here
