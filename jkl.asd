;;;; -*- Mode: Lisp -*-

(defpackage :jkl-sys
  (:use :CL :uiop :asdf))

(in-package :jkl-sys)

(defsystem jkl
  :version (:read-file-form "version")
  :author "ccQpein"
  :maintainer "ccQpein"
  :license "Apache"
  :homepage "https://github.com/ccqpein/jkl"
  :bug-tracker "https://github.com/ccqpein/jkl/issues"
  :source-control (:git "git@github.com:ccqpein/jkl.git")
  :description "Running shell command as Lisp way"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :defsystem-depends-on ("str")
  :serial t
  :components ((:file "options")
               (:file "core"
                :depends-on ("options"))
               (:module "cmds"
                :depends-on ("options" "core")
                :components #.(mapcar #'(lambda (p) (list :file (pathname-name p)))
                                      (directory-files
                                       (pathname-directory-pathname
                                        (uiop/lisp-build:current-lisp-file-pathname))
                                       "cmds/*.lisp"))))
  :in-order-to ((test-op (test-op "jkl/tests"))))

(defsystem jkl/tests
  :depends-on ("jkl" "fiveam")
  :components ((:file "options-test")
               (:file "core-test")
               )
  :perform (test-op
            (op c)
            (uiop:symbol-call :fiveam :run!
                              (uiop:find-symbol* :options :jkl-options-test))
            (uiop:symbol-call :fiveam :run!
                              (uiop:find-symbol* :core :jkl-test))))
