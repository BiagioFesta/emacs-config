;;; bf-config-basic-packages.el  --- Emacs Configuration
;;; Commentary:
;;;   Configure a bunch of packages general purpose.

;;; Code:
(require 'use-package)
(require 'bf-custom-vars)

(defun bf-config-basic-packages--dimish ()
  "Install and configure package `dimish'."
  (use-package diminish
    :ensure t))

(defun bf-config-basic-packages--which-key ()
  "Install and configure package `which-key'."
  (use-package which-key
    :ensure t
    :diminish
    :config
    (which-key-mode 1)))

(defun bf-config-basic-packages--undo-tree ()
  "Install and configure package `undo-tree'."
  (use-package undo-tree
    :ensure t
    :diminish
    :init
    (setq undo-tree-visualizer-diff t)
    :config
    (global-undo-tree-mode 1)
    :bind
    ("C-c z" . undo-tree-visualize)))

(defun bf-config-basic-packages--ace-window ()
  "Install and configure package `ace-window'."
  (use-package ace-window
    :ensure t
    :init
    (setq aw-ignore-current t)
    :bind
    ([remap other-window] . ace-window)))

(defun bf-config-basic-packages--avy ()
  "Install and configure package `avy'."
  (use-package avy
    :ensure t
    :diminish
    :init
    (setq avy-background (not bf-config-minimal-config))
    :bind
    ("C-c C-SPC" . avy-goto-char)))

(defun bf-config-basic-packages--beacon ()
  "Install and configure package `beacon'."
  (use-package beacon
    :ensure t
    :diminish
    :config
    (beacon-mode (if bf-config-minimal-config -1 1))))

(defun bf-config-basic-packages--ivy-swiper-counsel ()
  "Install and configure packages `ivy' `swiper' and `counsel'."
  (use-package ivy
    :ensure t
    :diminish
    :init
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "(%d/%d) ")
    :config
    (ivy-mode 1))
  (use-package swiper
    :ensure t
    :after ivy
    :bind
    (("C-s" . swiper)
     ("M-s s" . swiper-thing-at-point)))
  (use-package counsel
    :ensure t
    :after ivy
    :diminish
    :config
    (counsel-mode 1)
    (add-hook 'find-file-hook
              (lambda ()
                (let ((max-size bf-config-general-settings-size-big-file))
                  (when (> (buffer-size) max-size)
                    (let ((map (make-sparse-keymap)))
                      (set-keymap-parent map (current-local-map))
                      (use-local-map map)
                      (local-set-key [remap swiper] 'counsel-grep))))))
    :bind
    ("C-c C-u" . counsel-mark-ring)))

(defun bf-config-basic-packages--windmove ()
  "Install and configure package `windmove'."
  (use-package windmove
    :ensure t
    :bind
    ("S-<right>" . windmove-right)
    ("S-<left>" . windmove-left)
    ("S-<up>" . windmove-up)
    ("S-<down>" . windmove-down)))

(defun bf-config-basic-packages--xclip ()
  "Install and configure package `xclip'."
  (use-package xclip
    :ensure t
    :init
    (setq xclip-method 'xsel)
    :config
    (if (executable-find "xsel")
        (xclip-mode 1)
      (warn "xsel program not found."))))

(defun bf-config-basic-packages--goto-line-preview ()
  "Install and configure package `goto-line-preview'."
  (use-package goto-line-preview
    :ensure t
    :bind
    ([remap goto-line] . goto-line-preview)))

(defun bf-config-basic-packages--winner ()
  "Install and configure package `winner'."
  (use-package winner
    :ensure t
    :config
    (winner-mode 1)))

(defun bf-config-basic-packages--flyspell ()
  "Install and configure package `flyspell'."
  (use-package flyspell
    :ensure t
    :init
    (let ((spell-program (executable-find "aspell")))
      (if spell-program (setq ispell-program-name spell-program)
        (warn "aspell program not found.")))
    :config
    (add-hook 'text-mode-hook
              (lambda ()
                (flyspell-mode
                 (let ((max-size bf-config-general-settings-size-big-file))
                   (if (> (buffer-size) max-size) -1 1)))))))

(defun bf-config-basic-packages--multiple-cursors ()
  "Install and configure package `multiple-cursors'."
  (use-package multiple-cursors
    :ensure t
    :init
    (setq mc/insert-numbers-default 1)))

(defun bf-config-basic-packages ()
  "Install and configure all basic packages."
  (bf-config-basic-packages--dimish)
  (bf-config-basic-packages--which-key)
  (bf-config-basic-packages--undo-tree)
  (bf-config-basic-packages--ace-window)
  (bf-config-basic-packages--avy)
  (bf-config-basic-packages--beacon)
  (bf-config-basic-packages--ivy-swiper-counsel)
  (bf-config-basic-packages--windmove)
  (bf-config-basic-packages--xclip)
  (bf-config-basic-packages--goto-line-preview)
  (bf-config-basic-packages--winner)
  (bf-config-basic-packages--flyspell)
  (bf-config-basic-packages--multiple-cursors)
  nil)


(provide 'bf-config-basic-packages)
;;; bf-config-basic-packages.el ends here
