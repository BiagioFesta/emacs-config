;;; bf-config-org-mode.el  --- Emacs Configuration
;;; Commentary:
;;;   Setup ORG mode.

;;; Code:
(require 'org)
(require 'ob-core)

(defun bf-config-org-mode--load-babel-languages ()
  "Load babel languages. See the variable `org-babel-load-languages'."
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (org . t)
                                 (shell . t)
                                 (C . t)
                                 (python . t))))

(defun bf-config-org-mode ()
  "Setup environment for `org-mode'."
  (setq org-confirm-babel-evaluate nil)
  (bf-config-org-mode--load-babel-languages)
  nil)

(provide 'bf-config-org-mode)
;;; bf-config-org-mode.el ends here
