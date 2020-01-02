;;; bf-config-packages-management.el  --- Emacs Configuration
;;; Commentary:
;;;   Configure packages archive and manager.
;;;   Install `use-package' as packages manager.

;;; Code:
(require 'package)
(require 'bf-custom-vars)

(defun bf-install-package-if-missing(package)
  "Install PACKAGE if missing."
  (unless (package-installed-p package)
    (package-install package)))

(defun bf-config-packages-management--set-archives ()
  "Initialize the variable `package-archives'.
In accordance with the archives defined in the list
`bf-config-packages-management-archives'."
  (setq package-archives nil)
  (dolist (archive bf-config-packages-management-archives package-archives)
    (let ((archive-name (car archive))
          (archive-uri (cdr archive))
          (proto
           (if bf-config-packages-management-use-ssl
               "https"
             "http")))
      (let ((archive-url (concat proto "://" archive-uri)))
        (push (cons archive-name archive-url) package-archives)))))

(defun bf-config-packages-management()
  "Apply and configure package management."
  (bf-config-packages-management--set-archives)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (bf-install-package-if-missing 'use-package))

(provide 'bf-config-packages-management)
;;; bf-config-packages-management.el ends here
