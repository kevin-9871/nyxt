;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always '*auto-config-file-path*)
(defvar *auto-config-file-path* (make-instance 'data-path :basename "auto-config")
  "The path of the generated configuration file.")

(export-always 'funcall-safely)
(defun funcall-safely (f &rest args)
  "Like `funcall' except that if `*keep-alive*' is nil (e.g. the program is run
from a binary) then any condition is logged instead of triggering the debugger."
  (if *keep-alive*
      (handler-case (apply f args)
        (nyxt-minibuffer-canceled ()
          (log:debug "Minibuffer interrupted")
          nil))
      (handler-case (apply f args)
        (nyxt-minibuffer-canceled ()
          (log:debug "Minibuffer interrupted")
          nil)
        (error (c)
          (log:error "In ~a: ~a" f c)
          nil))))

(defun user-class-name (class-sym)
  (intern (str:concat "USER-" (string class-sym))
          (symbol-package class-sym)))

(export-always 'define-user-class)
(defmacro define-user-class (name &optional superclasses)
  "Define the user class of NAME.
This helper function is useful to compose the customizations of a class.

This may be called multiple times.
NAME must be an existing class.
NAME is automatically append to SUPERCLASSES, so that user-name inherits
from NAME last."
  (let ((user-name (user-class-name name))
        (superclasses-with-original (remove-duplicates
                                     (append superclasses (list name)))))
    `(progn
       (export-always ',user-name (symbol-package ',user-name))
       ;; Probably no need to call the defclass macro if we just need to
       ;; set the superclasses.
       (closer-mop:ensure-class ',user-name
                                :direct-superclasses ',superclasses-with-original))))

(defun user-class-p (class-specifier)
  (not (mopu:direct-slot-names class-specifier)))

(defmacro with-user-class ((class-sym new-superclasses) &body body) ; TODO: Export if users ever demand it.
  "Dynamically override the superclasses of the user class corresponding to
CLASS-SYM to NEW-SUPERCLASSES.  The class is restored when exiting BODY."
  ;; Test:
  ;; (with-user-class (buffer (buffer))
  ;;   (mopu:direct-superclasses 'user-buffer))
  (let ((user-class (user-class-name class-sym)))
    (unless (user-class-p user-class)
      (error "Argument must be a user class (see `user-class-p')."))
    (let ((old-superclasses (mapcar #'class-name (mopu:direct-superclasses user-class))))
      `(unwind-protect
            (progn
              (define-user-class ,class-sym ,new-superclasses)
              ,@body)
         (define-user-class ,class-sym ,old-superclasses)))))

(export-always '%slot-default)
(defmacro %define-configuration (name &body slots)
  (let* ((final-name (user-class-name name))
         (temp-name (gentemp (string final-name) (symbol-package name))))
    (dolist (name (list name final-name))
      (unless (find-class name nil)
        (error "define-configuration argument ~a is not a known class." name)))
    `(progn
       (define-class ,temp-name ()
         ,(loop with super-class = (closer-mop:ensure-finalized (find-class final-name))
                for slot in (first slots)
                for known-slot? = (find (first slot) (mopu:slot-names super-class))
                for initform = (and known-slot?
                                    (getf (mopu:slot-properties super-class (first slot))
                                          :initform))
                if known-slot?
                  collect (list (first slot)
                                :initform `(funcall (lambda (%slot-default)
                                                      (declare (ignorable %slot-default))
                                                      ,(cadr slot))
                                                    ,initform))
                else do
                  (log:warn "Undefined slot ~a in ~a" (first slot) final-name))
         (:accessor-name-transformer #'class*:name-identity))
       (define-user-class ,name ,(cons temp-name
                                            (mapcar #'class-name
                                                    (mopu:direct-superclasses final-name)))))))

(defun get-initform (class-symbol class-slot)
  (getf (mopu:slot-properties (find-class class-symbol) class-slot) :initform))

(export-always 'define-configuration)
(defmacro define-configuration (names &body slots)
  "Helper macro to customize the class slots of the NAMES classes.
NAMES is either a symbol or a list of symbols.

Classes can be modes or a one of the user-configurable classes like `browser',
`buffer', `minibuffer', `window'.  Note that the classes must _not_ be prefixed
by 'user-'.

The `%slot-default' variable is replaced by the slot initform.

Example that sets some defaults for all buffers:

\(define-configuration (buffer web-buffer)
  ((status-buffer-height 24)
   (default-modes (append '(vi-normal-mode) %slot-default))))

Example to get the `blocker-mode' command to use a new default hostlists:

\(define-configuration nyxt/blocker-mode:blocker-mode
  ((nyxt/blocker-mode:hostlists (append (list *my-blocked-hosts*) %slot-default))))

In the above, `%slot-default' will be substituted with the default value of
`default-modes'.

In the last example, `nyxt/blocker-mode:user-blocker-mode' is defined to inherit
from the original `blocker-mode' and a generated class containing the
specialized hostlists.

To discover the default value of a slot or all slots of a class, use the
`describe-slot' or `describe-class' commands respectively."
  (if (listp names)
      `(progn
         ,@(mapcar (lambda (name)
                     `(%define-configuration ,name ,@slots))
                   names))
      `(%define-configuration ,names ,@slots)))

(export-always 'load-after-system)
(defun load-after-system (system &optional file)
  "Load Common Lisp SYSTEM, afterwards if system was loaded, load file.
Use Quicklisp if possible.

Initialization file use case:

(load-after-system :xyz \"configure-xyz.lisp\")"
  (flet ((load-system (system)
           (ignore-errors
            #+quicklisp
            (ql:quickload system :silent t)
            #-quicklisp
            (asdf:load-system system))))
    (when (and (load-system system) file)
      (load file))))

(defun make-ring (&key (size 1000))
  "Return a new ring buffer."
  (containers:make-ring-buffer size :last-in-first-out))

(export-always 'trim-list)
(defun trim-list (list &optional (limit 100))
  (handler-case
      (if (< limit (length list))
          (nconc (sera:nsubseq list 0 (1- limit)) (list "…"))
          list)
    (error ()
      ;; Improper list.
      list)))

(defun public-initargs (class-specifier)
  (delete-if (lambda (name) (eq :internal (nth-value 1 (find-symbol (string name)))))
             (mopu:direct-slot-names class-specifier)))
