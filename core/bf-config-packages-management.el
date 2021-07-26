;;; bf-config-packages-management.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Configure packages archive and manager.
;;;   Install `use-package' as packages manager.
;;;   Install additional dependency libraries.

;;; Code:
(require 'package)
(require 'bf-config-vars)

(defun bf-config-install-package-if-missing (package)
  "Install PACKAGE if missing."
  (unless (package-installed-p package)
    (package-install package)))

(defun bf-config--packages-management--set-archives ()
  "Initialize the variable `package-archives'.
In accordance with the archives defined in the list
`bf-config-packages-management-archives'."
  (setq package-archives nil)
  (dolist (archive bf-config-packages-management-archives package-archives)
    (let ((archive-name (car archive))
          (archive-uri (cdr archive))
          (proto
           (if bf-config-packages-management-use-ssl "https" "http")))
      (let ((archive-url (concat proto "://" archive-uri)))
        (push (cons archive-name archive-url) package-archives)))))

(defun bf-config--packages-management--install-dependencies ()
  "Install packages needed for the configuration."
  (let ((list-deps '(use-package
		      f
		      seq)))
    (dolist (package list-deps)
      (bf-config-install-package-if-missing package))))

(defun bf-config--packages-management ()
  "Apply and configure package management."
  (bf-config--packages-management--set-archives)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (bf-config--packages-management--install-dependencies))

(provide 'bf-config-packages-management)
;;; bf-config-packages-management.el ends here
