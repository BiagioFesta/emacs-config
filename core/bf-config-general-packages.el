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

(defun bf-config--general-packages--ace-window ()
  "Install and configure `ace-window' package."
  (use-package ace-window
    :ensure t
    :init
    (setq aw-ignore-current t)
    :bind
    ([remap other-window] . ace-window)))

(defun bf-config--general-packages--projectile ()
  "Install and configure `projectile' package."
  (use-package projectile
    :ensure t
    :diminish
    :config
    (projectile-mode 1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(defun bf-config--general-packages--magit ()
  "Install and configure `magit' packages."
  (use-package magit
    :ensure t))

(defun bf-config--general-packages--windmove ()
  "Install and configure `windmove' package."
  (use-package windmove
    :ensure t
    :bind
    ("S-<right>" . windmove-right)
    ("S-<left>" . windmove-left)
    ("S-<up>" . windmove-up)
    ("S-<down>" . windmove-down)))

(defun bf-config--general-packages--winner ()
  "Install and configure `winner' package."
  (use-package winner
    :ensure t
    :config
    (winner-mode 1)))

(defun bf-config--general-packages--xclip ()
  "Install and configure package `xclip'."
  (use-package xclip
    :ensure t
    :config
    (if (executable-find "xclip")
        (progn
          (setq xclip-method 'xclip)
          (xclip-mode 1))
      (warn "xsel program not found."))))

(defun bf-config--general-packages--ivy-counsel-swiper ()
  "Install and configure packages `ivy' `counsel' and `swiper'."
  (use-package ivy
    :ensure t
    :diminish
    :init
    (setq ivy-count-format "(%d/%d) ")
    :config
    (ivy-mode 1))
  (use-package counsel
    :ensure t
    :after ivy
    :diminish
    :config
    (counsel-mode 1))
  (use-package swiper
    :ensure t
    :after ivy
    :bind
    (("C-s" . swiper)
     ("M-s s" . swiper-thing-at-point))))

(defun bf-config--general-packages--avy ()
  "Install and configure package `avy'."
  (use-package avy
    :ensure t
    :diminish
    :config
    (setq avy-background t)
    :bind
    ("C-c C-SPC" . avy-goto-char-timer)))


(defun bf-config--general-packages ()
  "Install and configure all general packages."
  (bf-config--general-packages--dimish)
  (bf-config--general-packages--which-key)
  (bf-config--general-packages--undo-tree)
  (bf-config--general-packages--ace-window)
  (bf-config--general-packages--projectile)
  (bf-config--general-packages--magit)
  (bf-config--general-packages--windmove)
  (bf-config--general-packages--winner)
  (bf-config--general-packages--xclip)
  (bf-config--general-packages--ivy-counsel-swiper)
  (bf-config--general-packages--avy))

(provide 'bf-config-general-packages)
;;; bf-config-general-packages.el ends here
