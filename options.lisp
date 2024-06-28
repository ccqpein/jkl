(defpackage :jkl-options
  (:use :cl)
  (:export :option
           :option1
           :option2
           :option3
           :option4

           :short-option
           :long-option
           :arg
           :description

           :option-match-string
           :restore-back-to-string
           :equal-option
           ))

(in-package :jkl-options)

(declaim (optimize (speed 3)))

(defclass option ()
  ((short-option
    :initarg :short-option
    :initform ""
    :accessor short-option
    :type string)
   (long-option
    :initarg :long-option
    :initform ""
    :accessor long-option
    :type string)
   (arg
    :initarg :arg
    :initform ""
    :accessor arg
    :type string)
   (description
    :initarg :description
    :initform ""
    :accessor description
    :type string))
  (:documentation "option class"))

(defmethod print-object ((opt option) stream)
  (declare (stream stream))
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
  (multiple-value-bind (short-name long-name arg des parsed)
      (option1-match-string input)
    (if parsed
        (setf (short-option opt) short-name
              (long-option opt) long-name
              (arg opt) arg
              (description opt) des)
        nil)))

(defun option1-match-string (input)
  "function for option1 match string. easy to test"
  (declare (string input))
  (str:match input
    (("\\s*-" short-name ", " "--" long-name "\\s+<" arg ">\\s+" des)
     (values short-name long-name arg des t))
    (("\\s*-" short-name ", " "--" long-name "\\s+" des)
     (values short-name long-name "" des t))
    (("\\s*--" long-name "\\s+<" arg ">\\s+" des)
     (values "" long-name arg des t))
    (("\\s*--" long-name "\\s+" des)
     (values "" long-name "" des t))
    (t (values "" "" "" "" nil))
    ))

(defmethod restore-back-to-string ((opt option1) value &optional short-option)
  (if (string/= "" (the string (arg opt)))
      (if short-option
          (if (string/= "" (the string (short-option opt)))
              (list (format nil "-~a" (short-option opt))
                    (format nil "~a" value))
              (error "option doesn't has short option"))
          (list (format nil "--~a" (long-option opt))
                (format nil "~a" value)))
      (if value
          (if short-option
              (if (string/= "" (the string (short-option opt)))
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
  (multiple-value-bind (short-name long-name arg des parsed)
      (option2-match-string input)
    (if parsed
        (setf (short-option opt) short-name
              (long-option opt) long-name
              (arg opt) arg
              (description opt) des)
        nil)))

(defun option2-match-string (input)
  "function for option2 match string. easy to test"
  (declare (string input))
  (str:match input
    (("\\s*-" short-name ",\\s+--" long-name "=" arg "\\s+" des)
     (values short-name long-name arg des t))
    (("\\s*-" short-name ",\\s+--" long-name "\\s+" des)
     (values short-name long-name "" des t))
    (("\\s*" "--" long-name "=" arg "\\s+" des)
     (values "" long-name arg des t))
    (("\\s*" "--" long-name "\\s+" des)
     (values "" long-name "" des t))
    (t (values "" "" "" "" nil))
    ))

(defmethod restore-back-to-string ((opt option2) value &optional short-option)
  (if (string/= "" (the string (arg opt)))
      (if short-option
          (if (string/= "" (the string (short-option opt)))
              (list (format nil "-~a" (short-option opt)) (format nil "~a" value))
              (error "option doesn't has short option"))
          (list (format nil "--~a=~a" (long-option opt) value)))
      (if value
          (if short-option
              (if (string/= "" (the string (short-option opt)))
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
  (:documentation "clap (Rust) option style (kind of curl style):
  -D, --del                            Delete the crumbs
  -R, --restore                        Restore the crumbs back to normal comment
      --fmt <FMT_COMMAND>              Format command after delete crumbs
  -O, --output-format <OUTPUT_FORMAT>  Output format: json, list
"))

;;; ===============================================

;;:= TODO: git help format
(defclass option5 (option)
  ()
  (:documentation "git and git subcommand options:
  Several examples:

  -n, --no-checkout
           Fail if the source repository is a shallow repository. The clone.rejectShallow
           configuration variable can be used to specify the default.

  --[no-]reject-shallow
           Fail if the source repository is a shallow repository. The clone.rejectShallow
           configuration variable can be used to specify the default.

  --server-option=<option>
           Transmit the given string to the server when communicating using protocol version 2. The
           given string must not contain a NUL or LF character. The server’s handling of server
           options, including unknown ones, is server-specific. When multiple --server-option=<option>
           are given, they are all sent to the other side in the order listed on the command line

  --reference[-if-able] <repository>
           If the reference <repository> is on the local machine, automatically setup

  --no-hardlinks
           Force the cloning process from a repository on a local filesystem to copy the files under

  -b <name>, --branch <name>
           Instead of pointing the newly created HEAD to the branch pointed to by the cloned...

  -c <key>=<value>, --config <key>=<value>
           Set a configuration variable in the newly-created repository; this takes effect immediately...

  -S[<keyid>], --gpg-sign[=<keyid>], --no-gpg-sign
           GPG-sign the resulting merge commit.

  --log[=<n>], --no-log
           In addition to branch names,...

  --squash, --no-squash
           Produce the working tree and index...


  This option may don't have the short option because the one-line may have different options. Like \"--log[=<n>], --no-log\"
"))

(defmethod option-match-string ((opt option5) input &key &allow-other-keys)
  (declare (cons input)) ;; option5 might have multiple lines
  (multiple-value-bind (short-name long-name arg des parsed)
      (option5-match-string input)
    (if parsed
        (setf (short-option opt) short-name
              (long-option opt) long-name
              (arg opt) arg
              (description opt) des)
        nil)))

(defun option5-match-string (input)
  (declare (cons input)) ;; option5 might have multiple lines
  ;; first line including all options

  ;; rest lines including description
  
  )
