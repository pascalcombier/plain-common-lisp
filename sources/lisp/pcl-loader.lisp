;;;
;;; +----------+---------------------------------------------------------------+
;;; | Info     | Value                                                         |
;;; +----------+---------------------------------------------------------------+
;;; | Filename | pcl-loader.lisp                                               |
;;; | Project  | plain-common-lisp                                             |
;;; | License  | Simplified BSD License (details in attached LICENSE file)     |
;;; +----------+---------------------------------------------------------------+
;;; | Copyright (C) 2014-2024 Pascal COMBIER <combier@outlook.com>             |
;;; +--------------------------------------------------------------------------+
;;;
;;; Loader for plain-common-lisp applications
;;;
;;; plain-common-lisp is a "portable" Common Lisp distribution for Microsoft
;;; Windows. It's portable in the sense that it does not require any
;;; installation. The distribution is downloaded, extracted and executed,
;;; regardless the location on the disk. A standard user (i.e. not
;;; administrator) could use it on his computer. While reported working on
;;; Linux, Windows X86-64 is the only supported target.
;;;
;;; This file will initialize ASDF, Quicklisp, CFFI and
;;; applications. plain-common-lisp is based on ASDF 3 and use the UIOP
;;; portability layer.
;;;
;;; This file creates a package named "pcl" which exports the function
;;; "initialize". This function *could* be used if the application is
;;; distributed as an executable generated with "save-lisp-and-die" *and* want
;;; to expose ASDF/Quicklisp to its application users. The second case where
;;; this function could be used is if the developer wants to distribute a
;;; standalone executable and keep the same application structure (DLL files
;;; placed in the application tree). This should be rare, so this function
;;; should probably not be called outside this file. Note that the file
;;; "pcl-loader.lisp" is not needed when an executable from "save-lisp-and-die"
;;; is used.
;;;
;;; The small inconvenience in the implementation of this file is that the
;;; packages are not known at the load time. So the packages can cannot be
;;; referenced directly (quicklisp-quickstart:install, quicklisp:quickload,
;;; cffi:*foreign-library-directories*). An indirect way to do it is used, which
;;; is a little bit less readable. The functions symbol-call and find-symbol*
;;; from uiop are used from this purpose.
;;;
;;; For example, (quicklisp-quickstart:install) become
;;;              (uiop:symbol-call :quicklisp-quickstart :install ...)
;;;
;;; pcl-root-dir
;;;   - applications/ user applications
;;;   - libraries/    user libraries
;;;   - cache/        Common Lisp FASL compilation cache
;;;   - sources/      Sources of PCL framework (this file)
;;;   - third-party/  Third-party binaries/libraries shipped with PCL
;;;                   but which do not belongs to PCL project: sbcl
;;;
;;; Documentation for UIOP:
;;;   https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/README.md
;;;   https://asdf.common-lisp.dev/uiop.pdf
;;;
;;; Performances
;;;
;;;   The loading time is not very fast, especially on older computers. This is
;;;   probably not an issue for most usages. If this is a real problem, the
;;;   developer still can generate an executable file with the function
;;;   save-lisp-and-die and the loading time will be much faster. The tests on a
;;;   laptop from 2011 (Intel 2310M) shows the following performances:
;;;   * load time: 2.78 seconds (totally)
;;;     * initialize-asdf         0.01  ( 0%)
;;;     * initialize-quicklisp    0.79  (27%)
;;;     * initialize-cffi         1.62  (54%)
;;;     * print-quicklisp-systems 0.49  (16%)
;;;     * initialize-applications 0.07  ( 2%)
;;;

(format t "Loading plain-common-lisp~%")

;;; +--------------------------------------------------------------------------+
;;; | PACKAGE INITIALIZATION                                                   |
;;; +--------------------------------------------------------------------------+

(in-package #:common-lisp-user)

(require "asdf")
#-asdf3 (error "PCL requires ASDF 3 because it uses UIOP's functions")

(defpackage #:pcl
  (:use    #:common-lisp)
  (:export #:initialize))

(in-package #:pcl)

;;; +--------------------------------------------------------------------------+
;;; | CONFIGURATION                                                            |
;;; +--------------------------------------------------------------------------+

(defconstant +pcl-cache-directory+   "cache/")
(defconstant +pcl-ql-init-file+      "third-party/quicklisp/quicklisp.lisp")
(defconstant +pcl-ql-repository-dir+ "third-party/quicklisp-repository/")
(defconstant +pcl-lib-dir+           "libraries/")
(defconstant +pcl-app-dir+           "applications/")
(defconstant +pcl-app-subdir-dll+    "third-party/binaries/")
(defconstant +pcl-app-subdir-sys+    "systems/")
(defconstant +pcl-app-subdir-lib+    "third-party/systems/")

;;; +--------------------------------------------------------------------------+
;;; | GLOBAL VARIABLES                                                         |
;;; +--------------------------------------------------------------------------+

(defvar pcl-root-dir nil
  "Root directory of plain-common-lisp's applications")

(defvar pcl-sbcl-default-libraries nil
  "List containing all the foreign libraries opened by SBCL at startup")

(defvar pcl-foreign-libraries nil
  "List of relative directories pathnames containing the DLLs at
sb-ext:*save-hook*. This list is restored during
sb-ext:*pre-foreign-init-hooks*")

;;; +--------------------------------------------------------------------------+
;;; | INITIALIZATION                                                           |
;;; +--------------------------------------------------------------------------+

(defun plainstarter-get-root-dir ()
  "This function is used to retrieve pcl-root-dir when plain-common-lisp is
started from plainstarter (plain-common-lisp.exe). It takes advantage from the
fact that plainstarter is always providing the full path in argv[0]. When
started from plainstarter, the executable is stored in
pcl-root-dir/third-party/bin/sbcl.exe."
  (let* ((arg0-string       (first (uiop:raw-command-line-arguments)))
         (arg0-pathname     (uiop:parse-native-namestring arg0-string))
         (arg0-dir-pathname (uiop:pathname-directory-pathname arg0-pathname)))
    (uiop:pathname-parent-directory-pathname
     (uiop:pathname-parent-directory-pathname arg0-dir-pathname))))

(defun standalone-get-root-dir ()
  "This function can be called to update the pathnames at run-time. It can be
useful if plain-common-lisp is called from a binary file generated from
save-lisp-and-die and want to provide a full Common Lisp + Quicklisp
experience."
  (let* ((arg0-string       (first (uiop:raw-command-line-arguments)))
         (arg0-pathname     (uiop:parse-native-namestring arg0-string))
         (arg0-dir-pathname (uiop:pathname-directory-pathname arg0-pathname)))
    ;; get rid of possible relative path "../../", if the program is called from
    ;; another directory such as "..\..\..\myprog.exe"
    (uiop:truename* arg0-dir-pathname)))

(defun pcl-relative-pathname (filename &optional sub-directory)
  (let ((directory (if sub-directory
                       (uiop:merge-pathnames* sub-directory pcl-root-dir)
                       pcl-root-dir)))
    (uiop:merge-pathnames* filename directory)))

(defun pcl-subdirectories (relative-pathname)
  (uiop:subdirectories (pcl-relative-pathname relative-pathname)))

;;; +--------------------------------------------------------------------------+
;;; | ASDF FASL FILES RELOCATION                                               |
;;; +--------------------------------------------------------------------------+

(defun pcl-compute-filename (fasl-pathname directory-str prefix)
  (let* ((unix-namestring  (uiop:unix-namestring fasl-pathname))
         (matched-position (search directory-str unix-namestring)))
    (when matched-position
      (let* ((dir-length (length directory-str))
             (result-1   (subseq unix-namestring (+ matched-position dir-length)))
             (result-2   (substitute #\- #\/ result-1)))
           (concatenate 'string prefix result-2)))))

(defun pcl-relocate-fasl (file designator)
  "Function called by ASDF to locate FASL binary files. Store all the FASL files
in the same directory with a specific prefix according to the original
directory (ql- for quicklisp, qls- for quicklisp-installed library, dep- for
dependancy source, src for sources)."
  (declare (ignorable designator))
  (let ((new-filename
         (or
          (pcl-compute-filename file "third-party/quicklisp-repository/dists/quicklisp/software" "qls")
          (pcl-compute-filename file "third-party/quicklisp-repository/quicklisp"                "ql")
          (pcl-compute-filename file "third-party/systems"                                       "dep")
          (pcl-compute-filename file "libraries"                                                 "lib")
          (pcl-compute-filename file "applications"                                              "src"))))
    (if new-filename
       (pcl-relative-pathname new-filename +pcl-cache-directory+)
        file)))

(defun initialize-asdf ()
  ;; Configure ASDF to relocate the FASL file according to the pcl-relocate-fasl
  ;; function
  (asdf:initialize-output-translations
   `(:output-translations
     (t (:function pcl-relocate-fasl))
     :ignore-inherited-configuration)))

;;; +--------------------------------------------------------------------------+
;;; | QUICKLISP MISSING FUNCTIONS                                              |
;;; +--------------------------------------------------------------------------+

;; Quicklisp system is not 100% standalone. By design, the package ql-setup is
;; not included inside the Quicklisp system. So, if one try to use the Quicklisp
;; functions, the system will report an error during the execution because of
;; the missing functions qmerge and qenough. This is actually very handy. We
;; just need to implement this package here and we get a Quicklisp installation
;; which can be moved accross the disk just like PortableApps.
;;
;; The implementation is actually mostly a copy of the original implementation
;; in setup.lisp. With the exception of *quicklisp-home* which is configured
;; in initialize-quicklisp.

(defpackage #:ql-setup
  (:use #:cl)
  (:export #:*quicklisp-home*
           #:qmerge
           #:qenough))

(in-package #:ql-setup)

(defvar *quicklisp-home* nil)

(defun qmerge (pathname)
  (merge-pathnames pathname *quicklisp-home*))

(defun qenough (pathname)
  (enough-namestring pathname *quicklisp-home*))

(in-package #:pcl)

;;; +--------------------------------------------------------------------------+
;;; | QUICKLISP PATCH                                                          |
;;; +--------------------------------------------------------------------------+

;;; Unfortunately, bundle.lisp has a small portability issue because the
;;; function write-loader-script creates a pathname at load-time. If the
;;; application directory is being moved to another location, the pathname
;;; become invalid.
;;;
;;; Here we patch the source code right after Quicklisp installation.

;; patched write-loader-script function use ql-setup:qmerge
(defvar write-loader-code "(defmethod write-loader-script ((bundle bundle) stream)
  (let ((template-lines
         (load-time-value
          (with-open-file (stream (ql-setup:qmerge \"quicklisp/bundle-template.lisp\"))
            (loop for line = (read-line stream nil)
                  while line collect line)))))
    (dolist (line template-lines)
      (write-line line stream))))
")

(defun patch-quicklisp ()
  (let* ((filename-orig (pcl-relative-pathname "third-party/quicklisp-repository/quicklisp/bundle.lisp"))
         (filename-copy (pcl-relative-pathname "third-party/quicklisp-repository/quicklisp/bundle-orig.lisp"))
         (lines         (uiop:read-file-lines filename-orig))
         (state         'copy-code-before))
    ;; keep a version of the original code before patch
    (uiop:rename-file-overwriting-target filename-orig filename-copy)
    ;; patch the original file
    (with-open-file (stream filename-orig :direction :output)
      (dolist (line lines)
        (cond
          ;; Copy all the lines before method "write-loader-script"
          ((eq state 'copy-code-before)
           (if (uiop:string-prefix-p "(defmethod write-loader-script ((bundle bundle) stream)" line)
               (setq state 'find-next-function)
               (write-line line stream)))
          ;; Insert new version of "write-loader-script"
          ((eq state 'find-next-function)
           (when (uiop:string-prefix-p "(defun coerce-to-directory (pathname)" line)
             (write-line write-loader-code stream)
             (write-line line stream)
             (setq state 'copy-code-after)))
          ;; Copy all the lines after
          ((eq state 'copy-code-after)
           (write-line line stream)))))))

;;; +--------------------------------------------------------------------------+
;;; | INITIALIZE QUICKLISP                                                     |
;;; +--------------------------------------------------------------------------+

(defun initialize-quicklisp ()
  ;; Setting *quicklisp-home* is required early because
  ;; quicklisp-quickstart:install will use qmerge and qenough functions.
  (setf ql-setup:*quicklisp-home* (pcl-relative-pathname "third-party/quicklisp-repository/"))
  ;; Download and install Quicklisp from the Internet if necessary
  (let ((ql-setup-file (pcl-relative-pathname "setup.lisp" +pcl-ql-repository-dir+)))
    (unless (uiop:file-exists-p ql-setup-file)
      ;; Initialize Quicklisp by loading quicklisp.lisp *silently* (except for
      ;; errors) by redirecting the standard streams.
      (let* ((*error-output*    *standard-output*)
             (*standard-output* (make-broadcast-stream))
             (ql-init-file      (pcl-relative-pathname +pcl-ql-init-file+)))
        (load ql-init-file))
      ;; Download and install Quicklisp
      (uiop:with-current-directory (pcl-root-dir)
        (uiop:symbol-call :quicklisp-quickstart :install :path +pcl-ql-repository-dir+))
      ;; Apply Quicklisp patch
      (patch-quicklisp)))
  ;; Quicklisp system is saved separately from Quicklisp-installed systems, so
  ;; we need to register Quicklisp directory to ASDF.
  (push (pcl-relative-pathname "third-party/quicklisp-repository/quicklisp/")
        asdf:*central-registry*)
  ;; Simply load the Quicklisp system
  (asdf:load-system "quicklisp")
  (uiop:symbol-call :quicklisp :setup))

(defun print-quicklisp-systems ()
  ;; Display the list of ASDF systems installed from Quicklisp
  (let ((distribution (uiop:symbol-call :ql-dist :dist "quicklisp")))
    (dolist (package (uiop:symbol-call :ql-dist :installed-releases distribution))
      (format t "ASDF ~A~%" (uiop:symbol-call :ql-dist :base-directory package)))))

;;; +--------------------------------------------------------------------------+
;;; | INITIALIZE CFFI                                                          |
;;; +--------------------------------------------------------------------------+

(defun initialize-cffi ()
  ;; CFFI is a hard-dependancy for pcl, we use it to configure applications.
  ;; For convenience we install/load it directly from Quicklisp
  (format t "LOAD Foreign Interface...")
  (uiop:symbol-call :quicklisp :quickload :cffi :silent t)
  (format t "ok~%"))

;;; +--------------------------------------------------------------------------+
;;; | LOAD APPLICATIONS DEPENDANCIES                                           |
;;; +--------------------------------------------------------------------------+

;; Note: isr-parameters means initialize-source-registry function parameters
;; isr-parameters is a list as following: ((:directory *absolute-path-1*)
;; (:directory *absolute-path-2*) etc). Each of these directories will be added
;; to ASDF search path. While traversing the application directories, the
;; isr-parameters list will be built and then the ASDF registry will be
;; initialized by calling asdf:initialize-source-registry with this list.

(defun initialize-applications ()
  (let ((isr-parameters nil)
        (cffi-directories (uiop:find-symbol* :*foreign-library-directories* :cffi nil)))
    ;; If "initialize" is called from a standalone executable,
    ;; cffi:*foreign-library-directories* need to be overridden with new value.
    (setf (symbol-value cffi-directories) (list pcl-root-dir))
    ;; List the libraries and applications
    (dolist (app-dir (append (pcl-subdirectories +pcl-lib-dir+)
                             (pcl-subdirectories +pcl-app-dir+)))
      (let ((dll-dir (pcl-relative-pathname +pcl-app-subdir-dll+ app-dir))
            (src-dir (pcl-relative-pathname +pcl-app-subdir-sys+ app-dir))
            (dep-dir (pcl-relative-pathname +pcl-app-subdir-lib+ app-dir)))
        ;; For each application, add the "third-party/binaries/" to the CFFI
        ;; search path if the directory exists
        (when (uiop:directory-exists-p dll-dir)
          (let ((dir-tree (cons dll-dir (uiop:subdirectories dll-dir))))
            (dolist (sub-dir dir-tree)
              (format t "DLLP ~A~%" sub-dir)
              (push sub-dir (symbol-value cffi-directories)))))
        ;; For each application and if the directory exists, add application
        ;; source code to ASDF registry
        (when (uiop:directory-exists-p src-dir)
          (let ((dir-tree (cons src-dir (uiop:subdirectories src-dir))))
            (dolist (sub-dir dir-tree)
              (format t "ASDF ~A~%" sub-dir)
              (setq isr-parameters (append isr-parameters (list `(:directory ,sub-dir)))))))
        ;; For each application and if the directory exists, add application
        ;; dependancies source code to ASDF registry
        (when (uiop:directory-exists-p dep-dir)
          (let ((dir-tree (cons dep-dir (uiop:subdirectories dep-dir))))
            (dolist (sub-dir dir-tree)
              (format t "ASDF ~A~%" sub-dir)
              (setq isr-parameters (append isr-parameters (list `(:directory ,sub-dir)))))))))
    ;; initialize ASDF
    (asdf:initialize-source-registry (append '(:source-registry) isr-parameters '(:inherit-configuration)))))

;;; +--------------------------------------------------------------------------+
;;; | FOREIGN LIBRARIES MANAGEMENT                                             |
;;; +--------------------------------------------------------------------------+

;;;
;;; By default, SBCL try to save and restore open shared libraries from their
;;; absolute pathnames. This probably works well on Unix-based systems, but this
;;; is not working well on Windows.
;;;
;;; This part of the code is a small work-around:
;;; - At startup, we save the initial list of open DLLs (msvcrt.dll, kernel32.dll, etc)
;;; - When saving the image, we save the newly opened DLLs as a relative namestring list
;;; - Just before SBCL's foreign engine is initialized, we re-open the saved libraries
;;;
;;; NOTE: might not work properly with libraries being relocated *or* library dependancies
;;;
;;; Vanilla SBCL does not have the hook sb-ext:*pre-foreign-init-hooks*,
;;; plain-common-lisp use a slighly modified version of SBCL adding this new
;;; hook.
;;;

(defun relativize-directory-pathname (pathname)
  (let ((root-namestring     (uiop:native-namestring pcl-root-dir))
        (pathname-namestring (uiop:native-namestring pathname)))
    (when (uiop:string-prefix-p root-namestring pathname-namestring)
      (let ((prefix-length (length root-namestring)))
        (subseq pathname-namestring prefix-length)))))

(defun load-library-success (pathname)
  (let ((success t))
    (handler-case
        (progn
          ;; try to load libary
          (sb-alien:load-shared-object pathname)
          (format t "DLL ~A~%" pathname))
      (error (condition)
        (declare (ignore condition))
        (setf success nil)))
    ;; return value
    success))

(defun collect-foreign-libraries-information ()
  (dolist (library sb-sys:*shared-objects*)
    (unless (member library pcl-sbcl-default-libraries)
      (let* ((library-pathname    (sb-alien::shared-object-pathname library))
             (relative-namestring (relativize-directory-pathname library-pathname)))
        ;; unload will remove the library from the list sb-sys:*shared-objects*,
        ;; preventing SBCL from trying to re-load the library at the next
        ;; startup
        (sb-alien:unload-shared-object library-pathname)
        ;; save the namestring in order to manually restore it at the next startup
        (if relative-namestring
            (pushnew relative-namestring pcl-foreign-libraries)
            (pushnew (uiop:native-namestring library-pathname) pcl-foreign-libraries)))))
  ;; Display the list for information
  (dolist (foreign-library pcl-foreign-libraries)
    (format t "DLL ~A~%" foreign-library)))

(defun restore-foreign-library-information ()
  ;; This is important to set pcl-root-dir to the proper value
  (setf pcl-root-dir (standalone-get-root-dir))
  ;; try to load the remaining DLLs
  (dolist (namestring pcl-foreign-libraries)
    ;; first case: try to load system liraries (i.e. "gdi32" "opengl32" etc)
    (unless (load-library-success namestring)
      ;; second case: the library is in the same directory as previously located
      (let ((pathname-candidate-1 (pcl-relative-pathname namestring)))
        (unless (load-library-success pathname-candidate-1)
          ;; last case: the library has been moved and is now beside the executable
          (let* ((basename (format nil
                                   "~A.~A"
                                   (pathname-name pathname-candidate-1)
                                   (or (pathname-type pathname-candidate-1) "dll")))
                 (pathname-candidate-2 (pcl-relative-pathname basename)))
            (unless (load-library-success pathname-candidate-2)
              (format t "WARNING: could not restore the library~%")
              (format t "~A~%" namestring))))))))

(defun configure-sbcl-hooks ()
  ;; save the list of libraries opened at SBCL startup
  (unless pcl-sbcl-default-libraries
    (dolist (library sb-sys:*shared-objects*)
      (pushnew library pcl-sbcl-default-libraries)))
  ;; register hooks
  (pushnew #'collect-foreign-libraries-information sb-ext:*save-hooks*)
  (pushnew #'restore-foreign-library-information   sb-ext:*pre-foreign-init-hooks*))

;;; +--------------------------------------------------------------------------+
;;; | WELCOME MESSAGE                                                          |
;;; +--------------------------------------------------------------------------+

(defun print-introduction ()
  (format t "~%Welcome to plain-common-lisp, a portable Common Lisp distribution~%")
  (format t "- Quicklisp: to load a system, use: (ql:quickload \"system-name\" :silent t)~%")
  (format t "- Quicklisp: to find systems,  use: (ql:system-apropos \"system-name\")~%")
  (format t "- Quicklisp: to remove system, use: (ql:uninstall  \"system-name\")~%")
  (format t "- Quicklisp: to update  everything: (ql:update-all-dists)~%")
  (format t "- Quicklisp: to clean old software: (ql-dist:clean (ql-dist:dist \"quicklisp\"))~%")
  (format t "- To measure performances, use (time (...))~%")
  (format t "- To load a system, use (asdf:load-system \"system-name\")~%")
  (format t "- To run tests, use (asdf:test-system \"system-name\")~%")
  (format t "- To start a better REPL, use (require \"sb-aclrepl\")~%")
  (format t "~%"))

;;; +--------------------------------------------------------------------------+
;;; | MAIN FUNCTION                                                            |
;;; +--------------------------------------------------------------------------+

(defun initialize (&optional (from-standalone-executable t))
  (setf pcl-root-dir (if from-standalone-executable
                         (standalone-get-root-dir)
                         (plainstarter-get-root-dir)))
  (configure-sbcl-hooks)
  (format t "~A~%" (first (uiop:raw-command-line-arguments)))
  (format t "Working directory '~A'~%" (uiop:getcwd))
  (format t "Default directory '~A'~%" *default-pathname-defaults*)
  (initialize-asdf)
  (initialize-quicklisp)
  (initialize-cffi)
  (print-quicklisp-systems)
  (initialize-applications)
  (print-introduction)
  (unless from-standalone-executable
    (let* ((pcl-progname (uiop:getenv "PCL_PROGNAME"))
           (filename (if pcl-progname (format nil "~A.lisp" pcl-progname)
                         "main.lisp"))
           (startup-file (pcl-relative-pathname +pcl-app-dir+ filename)))
      (when (uiop:file-exists-p startup-file)
        (load startup-file)))))

(initialize nil)

;; Return to the default package
(in-package #:common-lisp-user)
