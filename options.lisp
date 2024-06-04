(defpackage :jkl-options
  (:use :cl)
  (:export :option
           :option1
           :option2

           :short-option
           :long-option
           :arg
           :description

           :option-match-string
           :restore-back-to-string
           ))

(in-package :jkl-options)

(defclass option ()
  ((short-option
    :initarg :short-option
    :initform ""
    :accessor short-option)
   (long-option
    :initarg :long-option
    :initform ""
    :accessor long-option)
   (arg
    :initarg :arg
    :initform ""
    :accessor arg)
   (description
    :initarg :description
    :initform ""
    :accessor description))
  (:documentation "option class"))

(defmethod print-object ((opt option) stream)
  (format stream "short-option: ~a~%long-option: ~a~%argument: ~a~%description: ~a~%"
          (short-option opt)
          (long-option opt)
          (arg opt)
          (description opt)))

(defmethod equal-option ((opt1 option) (opt2 option))
  (and (equal (short-option opt1) (short-option opt2))
       (equal (long-option opt1) (long-option opt2))
       (equal (arg opt1) (arg opt2))
       (equal (description opt1) (description opt2))))

(defclass option1 (option)
  ()
  (:documentation "curl option style:

-d, --data <data> HTTP POST data
"))

(defmethod option-match-string ((opt option1) input &key &allow-other-keys)
  (declare (string input))
  (multiple-value-bind (short-name long-name arg des)
      (option1-match-string input)
    (setf (short-option opt) short-name
          (long-option opt) long-name
          (arg opt) arg
          (description opt) des)))

(defun option1-match-string (input)
  "function for option1 match string. easy to test"
  (declare (string input))
  (str:match input
    (("\\s*-" short-name ", " "--" long-name "\\s+<" arg ">\\s+" des)
     (values short-name long-name arg des))
    (("\\s*-" short-name ", " "--" long-name "\\s+" des)
     (values short-name long-name "" des))
    (("\\s*--" long-name "\\s+<" arg ">\\s+" des)
     (values "" long-name arg des))
    (("\\s*--" long-name "\\s+" des)
     (values "" long-name "" des))
    ))

(defmethod restore-back-to-string ((opt option1) value &optional short-option)
  (if (string/= "" (arg opt))
      (if short-option
          (if (string/= "" (short-option opt))
              (list (format nil "-~a" (short-option opt))
                    (format nil "~a" value))
              (error "option doesn't has short option"))
          (list (format nil "--~a" (long-option opt))
                (format nil "~a" value)))
      (if value
          (if short-option
              (if (string/= "" (short-option opt))
                  (list (format nil "-~a" (short-option opt)))
                  (error "option doesn't has short option"))
              (list (format nil "--~a" (long-option opt))))
          (error "flag option has to give some value"))))

;;; ===============================================

(defclass option2 (option)
  ()
  (:documentation "wget option style:

  -A,  --accept=LIST               comma-separated list of accepted extensions

"))

(defmethod option-match-string ((opt option2) input &key &allow-other-keys)
  (declare (string input))
  (multiple-value-bind (short-name long-name arg des)
      (option2-match-string input)
    (setf (short-option opt) short-name
          (long-option opt) long-name
          (arg opt) arg
          (description opt) des)))

(defun option2-match-string (input)
  "function for option1 match string. easy to test"
  (declare (string input))
  (str:match input
    (("\\s*-" short-name ",\\s+--" long-name "=" arg "\\s+" des)
     (values short-name long-name arg des))
    (("\\s*-" short-name ",\\s+--" long-name "\\s+" des)
     (values short-name long-name "" des))
    (("\\s*" "--" long-name "=" arg "\\s+" des)
     (values "" long-name arg des))
    (("\\s*" "--" long-name "\\s+" des)
     (values "" long-name "" des))
    ))

(defmethod restore-back-to-string ((opt option2) value &optional short-option)
  (if (string/= "" (arg opt))
      (if short-option
          (if (string/= "" (short-option opt))
              (list (format nil "-~a" (short-option opt)) (format nil "~a" value))
              (error "option doesn't has short option"))
          (list (format nil "--~a=~a" (long-option opt) value)))
      (if value
          (if short-option
              (if (string/= "" (short-option opt))
                  (list (format nil "-~a" (short-option opt)))
                  (error "option doesn't has short option"))
              (list (format nil "--~a" (long-option opt))))
          (error "flag option has to give some value"))))

;;; ===============================================

(defclass option3 (option1)
  ()
  (:documentation "clingon option style (kind of curl style):
      --version         display version and exit
  -n, --id <INT>        the id of quiz
  -o, --output <VALUE>  output file
"))

;;; ===============================================

(defclass option4 (option1)
  ()
  (:documentation "clap option style (kind of curl style):
  -D, --del                            Delete the crumbs
  -R, --restore                        Restore the crumbs back to normal comment
      --fmt <FMT_COMMAND>              Format command after delete crumbs
  -O, --output-format <OUTPUT_FORMAT>  Output format: json, list
"))
