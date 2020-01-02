;;; bf-config-cpp.el  --- Emacs Configuration
;;; Commentary:
;;;   Setup C++ mode.

;;; Code:
(require 'use-package)
(require 'bf-custom-vars)

(defun bf-config-cpp--config-abrbev-mode ()
  "Configure the function `abbrev-mode'."
  (add-hook 'c-mode-common-hook #'(lambda () (abbrev-mode -1))))

(defun bf-config-cpp--ccls ()
  "Install and configure package `ccls'."
  (if bf-config-cpp-use-ccls
      (use-package ccls
        :ensure t
        :init
        (if bf-config-cpp-ccls-executable
            (setq ccls-executable bf-config-cpp-ccls-executable)))))

(defun bf-config-cpp--google-c-style ()
  "Install and configure package `google-c-style'."
  (use-package google-c-style
    :ensure t
    :hook
    ((c-mode c++-mode) . google-set-c-style)
    ((c-mode c++-mode) . google-make-newline-indent)))

(defun bf-config-cpp--modern-cpp-font-lock ()
  "Install and configure package `modern-cpp-font-lock'."
  (use-package modern-cpp-font-lock
    :ensure t
    :diminish modern-c++-font-lock-mode
    :hook
    (c++-mode . modern-c++-font-lock-mode)))

(defun bf-config-cpp--clang-format ()
  "Install and configure package `clang-fromat'."
  (use-package clang-format
    :ensure t
    :init
    (setq clang-format-executable "/usr/bin/clang-format")
    (setq-default clang-format-style "{ BasedOnStyle: Google,
                                        AlignAfterOpenBracket: Align,
                                        AllowAllParametersOfDeclarationOnNextLine: false,
                                        BinPackArguments: false,
                                        BinPackParameters: false,
                                        IncludeBlocks: Merge,
                                        ColumnLimit: 80,
                                        PointerAlignment: Left }")
    :hook
    ((c-mode c++-mode) . (lambda ()
                           (local-set-key (kbd "C-c TAB") 'clang-format-buffer)))))

(defun bf-config-cpp ()
  "Setup environment for mode C and C++."
  (bf-config-cpp--config-abrbev-mode)
  (bf-config-cpp--ccls)
  (bf-config-cpp--google-c-style)
  (bf-config-cpp--modern-cpp-font-lock)
  (bf-config-cpp--clang-format)
  nil)

(provide 'bf-config-cpp)
;;; bf-config-cpp.el ends here
