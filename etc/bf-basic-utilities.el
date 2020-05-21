;;; bf-basic-utilities.el --- Emacs Configuration
;;; Commentary:
;;;   Define simple function utilities.
;;;   Those function will be defined before packages initialization!

;;; Code:
(require 'seq)
(require 'f)
(require 'projectile)

(defun bf-copyfilepath()
  "Insert the FilePath into the kill ring."
  (interactive)
  (kill-new (concat default-directory (buffer-name))))

(defun bf-basic-utilities-find-compilation-database (DIRECTORY)
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
          (let ((db-indir (bf-basic-utilities-find-compilation-database (car all-dirs))))
            (if db-indir (setq db-found db-indir) (setq all-dirs (cdr all-dirs)))))
        db-found))))

(defun bf-create-link-to-compilation-db (COMPILATION-DATABASE &optional PROJECT_ROOT_DIR)
  "Create a symbolic-link to the file COMPILATION-DATABASE.
When PROJECT_ROOT_DIR is omitted, it tries to retrieve the project root
directory from the Projectile settings.
In case Projectile is missing or the root directory is not available,
the function will prompt the user in order to provide the root project
directory."
  (interactive (let ((compilation-db-found
                      (if (and (boundp 'projectile-project-root) (projectile-project-root))
                          (bf-basic-utilities-find-compilation-database (projectile-project-root)))))
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

(defun bf-compile-cpp-cmake (GENERATOR COMPILER BUILD_TYPE PROJECT_ROOT BUILD_DIR)
  "Inizialize a CMake compilation for a project.
GENERATOR should be a string: 'default', 'makefile' or 'ninja'.

COMPILER should be a string: 'default', 'gcc' or 'clang'.

BUILD_TYPE should be a string: 'default', 'debug' or 'release'.

PROJECT_ROOT is an esisting directory, the root of the project.
Where CMakeLists.txt is present.

BUILD_DIR is a directory where the project will be compiled.
If the directory does not exist it will be created.
Moreover, if it does exist then the CMake cache file will be deleted in order
to start a  new fresh configuration."
  (interactive
   (list (completing-read "Generator: " '("default" "makefile" "ninja") nil t)
         (completing-read "Compiler: " '("default" "gcc" "clang") nil t)
         (completing-read "Build Type: " '("default" "debug" "release") nil t)
         (read-directory-name "Project Directory: " (projectile-project-root) nil t)
         (read-directory-name "Build Directory: " (concat (projectile-project-root) "build"))))
  (let ((cmake-cache-file (concat BUILD_DIR "/" "CMakeCache.txt")))
    (if (file-directory-p BUILD_DIR)
        (when (file-exists-p cmake-cache-file) (delete-file cmake-cache-file))
      (make-directory BUILD_DIR)))
  (let ((cmake-bin "cmake")
        (generator (cond ((string= GENERATOR "makefile") "Unix Makefile")
                         ((string= GENERATOR "ninja") "Ninja")))
        (build-type (cond ((string= BUILD_TYPE "debug") "Debug")
                          ((string= BUILD_TYPE "release") "Release")))
        (cc (cond ((string= COMPILER "gcc") "-DCMAKE_C_COMPILER=\"gcc\" -DCMAKE_CXX_COMPILER=\"g++\"")
                  ((string= COMPILER "clang") "-DCMAKE_C_COMPILER=\"clang\" -DCMAKE_CXX_COMPILER=\"clang++\"")))
        (extra-flags (when (or (string= COMPILER "gcc") (string= COMPILER "clang"))
                       (cond ((string= BUILD_TYPE "debug") "-Wall -Wextra -pedantic -fsanitize=undefined")
                             ((string= BUILD_TYPE "release") "")))))
    (let ((cmake-shell-cmd
           (mapconcat 'identity (list cmake-bin
                                      (when generator (format "-G\"%s\"" generator))
                                      (when build-type (format "-DCMAKE_BUILD_TYPE=\"%s\"" build-type))
                                      cc
                                      "-DCMAKE_EXPORT_COMPILE_COMMANDS=YES"
                                      (when extra-flags (format "-DCMAKE_C_FLAGS=\"%s\" -DCMAKE_CXX_FLAGS=\"%s\""
                                                                extra-flags extra-flags))
                                      PROJECT_ROOT)
                      " ")))
      (compile (format "cd %s && %s && %s --build ." BUILD_DIR cmake-shell-cmd cmake-bin)))))

(defun bf-basic-utilities-transpose-line-up ()
  "Transpose the current line with the previous one."
  (transpose-lines 1)
  (forward-line -2))

(defun bf-basic-utilities-transpose-line-down ()
  "Transpose the current line with the following one."
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(provide 'bf-basic-utilities)
;;; bf-basic-utilities.el ends here
