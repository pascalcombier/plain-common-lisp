;;;
;;; +----------+---------------------------------------------------------------+
;;; | Info     | Value                                                         |
;;; +----------+---------------------------------------------------------------+
;;; | Filename | pcl-loader.lisp                                               |
;;; | Project  | plain-starter                                                 |
;;; | License  | Simplified BSD License (details in attached LICENSE file)     |
;;; +----------+---------------------------------------------------------------+
;;; | Copyright (C) 2014-2022 Pascal COMBIER <combier@outlook.com>             |
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
;;;   The loading time is not very fast, especially on older computer. This is
;;;   probably not an issue for most usages. If this is a real problem, the
;;;   developer still can generate an executable file with the function
;;;   save-lisp-and-die and the loading time will be much faster. The tests on a
;;;   laptop from 2011 (Intel 2310M) shows the following performances:
;;;   * load time: 3.1 seconds (totally)
;;;     * initialize-asdf         0.01 ( 0%)
;;;     * initialize-quicklisp    1.54 (49%)
;;;     * initialize-cffi         1.18 (37%)
;;;     * print-quicklisp-systems 0.38 (12%)
;;;     * initialize-applications 0.05 ( 2%)
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

(defvar pcl-foreign-directories nil
  "List of relative directories containing the DLLs at sb-ext:*save-hook*. This
list is restored during sb-ext:*pre-foreign-init-hooks*")

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
save-lisp-and-die."
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
depandancy source, src for sources)."
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
;;; | INITIALIZE QUICKLISP                                                     |
;;; +--------------------------------------------------------------------------+

(defun initialize-quicklisp ()
  ;; Load Quicklisp *silently* (except for errors) by redirecting the standard
  ;; streams.
  (let* ((*error-output*    *standard-output*)
         (*standard-output* (make-broadcast-stream))
         (ql-init-file      (pcl-relative-pathname +pcl-ql-init-file+)))
    (load ql-init-file))
  ;; Install Quicklisp if necessary
  (let ((ql-setup-file (pcl-relative-pathname "setup.lisp" +pcl-ql-repository-dir+)))
    ;; if directory is empty, reinstall Quicklisp from the Internet
    (unless (uiop:file-exists-p ql-setup-file)
      (uiop:with-current-directory (pcl-root-dir)
        (uiop:symbol-call :quicklisp-quickstart :install :path +pcl-ql-repository-dir+)))
    ;; most of the cases, simply load Quicklisp
    (when (uiop:file-exists-p ql-setup-file)
      (format t "LOAD Quicklisp...........")
      (load ql-setup-file)
      (format t "ok~%"))))

(defun print-quicklisp-systems ()
  ;; Display the list of ASDF systems installed from Quicklisp
  (let ((distribution (uiop:symbol-call :ql-dist :dist "quicklisp")))
    (dolist (package (uiop:symbol-call :ql-dist :installed-releases distribution))
      (format t "ASDF ~A~%" (uiop:symbol-call :ql-dist :base-directory package)))))

;;; +--------------------------------------------------------------------------+
;;; | INITIALIZE CFFI                                                          |
;;; +--------------------------------------------------------------------------+

(defun initialize-cffi ()
  ;; cffi is a plain-common-lisp dependancy. We need to load CFFI before
  ;; configuring cffi:*foreign-library-directories* for applications.
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
    (push pcl-root-dir (symbol-value cffi-directories))
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
;;; is absolutely not working when you start an executable on Windows using
;;; external DLLs.
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

(defun pathname-basename (pathname)
  (format nil
          "~A.~A"
          (pathname-name pathname)
          (pathname-type pathname)))

(defun relativize-directory-pathname (pathname)
  (let ((root-namestring     (uiop:native-namestring pcl-root-dir))
        (pathname-namestring (uiop:native-namestring pathname)))
    (when (uiop:string-prefix-p root-namestring pathname-namestring)
      (let ((prefix-length (length root-namestring)))
        (subseq pathname-namestring prefix-length)))))
        
(defun collect-foreign-libraries-information ()
  (dolist (library sb-sys:*shared-objects*)
    (unless (member library pcl-sbcl-default-libraries)
      (let ((lib-pathname (sb-alien::shared-object-pathname library)))
        ;; unload will remove the library from the list sb-sys:*shared-objects*,
        ;; preventing SBCL from trying to re-load the library at next startup
        ;; (i.e. preventing the runtime error to appear)
        (sb-alien:unload-shared-object lib-pathname)
        (format t "DLL ~A~%" (pathname-basename lib-pathname))
        ;; remember that the library need to be loaded in sb-ext:*pre-foreign-init-hooks*
        (pushnew (relativize-directory-pathname lib-pathname) pcl-foreign-directories)))))
      
(defun restore-foreign-library-information ()
  (dolist (lib-name pcl-foreign-directories)
    (let ((pathname (pcl-relative-pathname lib-name)))
      (if (uiop:file-exists-p pathname)
          ;; try to load the library at the indicated directory
          (let ((printable-name (pathname-basename pathname)))
            (format t "DLL ~A~%" printable-name)
            (sb-alien:load-shared-object pathname))
          ;; maybe the developper put all the DLLs near the executable
          (let ((printable-name (pathname-basename pathname)))
            (format t "DLL ~A~%" printable-name)
            (sb-alien:load-shared-object (pcl-relative-pathname printable-name)))))))

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
