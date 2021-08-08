;;; bf-config-utilities.el  --- Biagio's Emacs Configuration
;;; Commentary:
;;;   Define simple function utilities.

;;; Code:
(require 'seq)
(require 'f)
(unless (require 'projectile nil 'noerror)
  (warn "`projectile' package cannot be found. Some functions might not be available."))
(require 'display-fill-column-indicator)

(defun bf-config-utilities-find-compilation-database (DIRECTORY)
  "Recursively scan  DIRECTORY in order to find the compilation database.
The compilation database is a file named 'compile_commands.json'
Returns the filename if found, nil otherwise."
  (if (not (and (file-exists-p DIRECTORY) (file-directory-p DIRECTORY)))
      (error "Directory does not exist: %s" DIRECTORY))
  (message (format "Scan directory '%s'" DIRECTORY))
  (let* ((compilation-db-filename "compile_commands.json")
         (compilation-db-file (directory-files DIRECTORY t compilation-db-filename)))
    (if compilation-db-file
        (car compilation-db-file)
      (let ((all-dirs (seq-filter 'file-directory-p (directory-files DIRECTORY t ".*[^(\\.\\.)(\\.)]$")))
            (db-found nil))
        (while (and (not db-found) all-dirs)
          (let ((db-indir (bf-config-utilities-find-compilation-database (car all-dirs))))
            (if db-indir (setq db-found db-indir) (setq all-dirs (cdr all-dirs)))))
        db-found))))

(with-eval-after-load "projectile"
  (if (fboundp 'projectile-project-root)
      (defun bf-create-link-to-compilation-db (COMPILATION-DATABASE &optional PROJECT_ROOT_DIR)
        "Create a symbolic-link to the file COMPILATION-DATABASE.
When PROJECT_ROOT_DIR is omitted, it tries to retrieve the project root
directory from the Projectile settings.
In case Projectile is missing or the root directory is not available,
the function will prompt the user in order to provide the root project
directory."
        (interactive (let ((compilation-db-found
                            (if (and (boundp 'projectile-project-root) (projectile-project-root))
                                (bf-config-utilities-find-compilation-database (projectile-project-root)))))
                       (list (read-file-name
                              "Compilation database: "
                              compilation-db-found
                              "compile_commands.json"
                              t)
                             nil)))
        (unless PROJECT_ROOT_DIR
          (setq PROJECT_ROOT_DIR
                (if (and (boundp 'projectile-project-root) (projectile-project-root))
                    (projectile-project-root)
                  (read-directory-name "Project root directory: "))))
        (let* ((compilation-db-filename "compile_commands.json")
               (target-db-link (concat PROJECT_ROOT_DIR compilation-db-filename)))
          (cond
           ((not (file-exists-p PROJECT_ROOT_DIR))
            (error (format "The project root directory does not exist: %s" PROJECT_ROOT_DIR)))
           ((not (file-directory-p PROJECT_ROOT_DIR))
            (error (format "The project root directory is not a directory: %s" PROJECT_ROOT_DIR)))
           ((not (file-exists-p COMPILATION-DATABASE))
            (error (format "The compilation database does not exist: %s" COMPILATION-DATABASE)))
           ((not (string-equal (file-name-nondirectory COMPILATION-DATABASE) compilation-db-filename))
            (error (format "The compilation database file is not correct: %s" COMPILATION-DATABASE)))
           ((file-exists-p target-db-link)
            (error (format "The compilation database is already present in the project root: %s" target-db-link))))
          (f-symlink COMPILATION-DATABASE target-db-link)
          (message (format "Link to the compilation database created: %s" target-db-link))))
    (warn "Cannot define the function `bf-create-link-to-compilation-db' because `projectile-project-root' is missing")))

(defun bf-column-indicator-at-point (ARG &optional COLUMN)
  "Enable/Disable `display-fill-column-indicator-mode' with column margin.
It sets the position of the margin displayed at COLUMN (using
`display-fill-column-indicator-column').
If COLUMN is nil or if this function is called interactively, it sets the COLUMN
at `current-column'.

If ARG is positive then it enables the mode, otherwise it disables.
ARG can be `toggle' (default when call with interatvice) for toggling.

When disabled, the colum is reset to default."
  (interactive '(toggle))
  (setq display-fill-column-indicator-column
        (let ((column (cond ((eq ARG 'toggle) (unless (bound-and-true-p display-fill-column-indicator-mode)
                                                (if COLUMN COLUMN (current-column))))
                            ((> ARG 0) (if COLUMN COLUMN (current-column))))))
          (if column column
            (default-value 'display-fill-column-indicator-column))))
  (display-fill-column-indicator-mode ARG))

(provide 'bf-config-utilities)
;;; bf-config-utilities.el ends here
