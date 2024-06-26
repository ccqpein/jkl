(defpackage :jkl
  (:use :CL :jkl-options)
  (:export
   :*jkl-cmd-folder*
   :command
   :list-options
   :get-options
   :gen-options
   :make-new-command
   :parse-option-from-help
   :read-line-content
   :run))

(in-package :jkl)

(declaim (optimize (speed 3)))

(defparameter *jkl-cmd-folder* (merge-pathnames (asdf:system-source-directory :jkl) "cmd")
  "default commands folder")

(defclass command ()
  ((name
    :initarg :name
    :accessor name
    :type string)
   (subcommand
    :initarg :subcommand
    :accessor subcommand
    :initform nil
    :documentation "the subcommand, hashtable")
   (options
    :initarg :options
    :accessor options
    :initform nil
    :documentation "hashtable of options, key => (short? option)"))
  (:documentation "the class of command"))

(defmethod print-object ((cmd command) stream)
  (declare (stream stream))
  (format stream "name: ~a
subcommand:~%~{~%~@{key:~a~%subcommand:~%~a~}~}
options:~%~{~%~@{key:~a~%option:~%~a~}~}"
          (name cmd)
          (if (subcommand cmd)
              (loop for sc being the hash-keys of (subcommand cmd)
                      using (hash-value vv)
                    collect (list sc vv))
              nil)
          (if (options cmd)
              (loop for op being the hash-keys of (options cmd)
                      using (hash-value vv)
                    collect (list op vv))
              nil)
          ))

(defmethod list-options ((comm command) &key short-option-start-with long-option-start-with with-key)
  "list all options of command"
  (loop for k being the hash-keys of (options comm)
          using (hash-value opt)
        when (and short-option-start-with
                  (str:starts-with? (str:upcase short-option-start-with) (str:upcase (short-option (second opt)))))
          collect (if with-key (list k opt) opt) into result

        when (and long-option-start-with
                  (str:starts-with? (str:upcase long-option-start-with) (str:upcase (long-option (second opt)))))
          collect (if with-key (list k opt) opt) into result

        when (not (or short-option-start-with long-option-start-with))
          collect (if with-key (list k opt) opt) into result

        finally (return result)))

(defmethod get-options ((comm command) &rest keys)
  "get the option of command by keyword"
  (loop with opts = (options comm)
        for key string in keys
        collect (gethash (str:upcase (string key)) opts)
        ))

(defmethod gen-options ((comm command) &rest args)
  "give command and the keyword/value pairs and more argvs to get command line options"
  (do ((this (car args) (car args))
       result)
      ((not this) result)
    (if (keywordp this)
        ;; if keyword
        (let ((option (gethash (string this) (options comm)))
              )
          (if option
              (setf result (append result
                                   (if (first option) ;; if it is short keyword
                                       (restore-back-to-string (second option) (second args) :short-option)
                                       (restore-back-to-string (second option) (second args))))))
          (setf args (cddr args)))
        ;; if not keyword
        (let ((subcmd (if (subcommand comm)
                          (gethash this (subcommand comm) nil)
                          nil)))
          (if subcmd
              ;; if subcommand
              (progn (setf result (append result (cons this (apply #'gen-options subcmd (cdr args)))))
                     (setf args nil))
              (progn (setf result (append result (list this)))
                     (setf args (cdr args))))))))

(defun check-output-and-error-kw (args)
  (do ((this (car args) (car args))
       (jkl-output *standard-output*)
       (jkl-error *error-output*))
      ((not args) (values jkl-output jkl-error))
    (cond ((and (keywordp this) (eq this :jkl-output))
           (setf jkl-output (second args)
                 args (cddr args)))
          ((and (keywordp this) (eq this :jkl-error))
           (setf jkl-error (second args)
                 args (cddr args)))
          (t (setf args (cdr args))))))

#+sbcl
(defun sbcl-run (name options &key (output *standard-output*) (error :output))
  "run in sbcl and return the output stream"
  (sb-ext:run-program name options
                      :search t
                      :output output
                      :error error)
  output)

(defmethod run ((comm command) &rest args)
  (multiple-value-bind (jkl-output jkl-error)
      (check-output-and-error-kw args)
    #+sbcl
    (sbcl-run
     (name comm)
     (apply #'gen-options comm args)
     :output jkl-output
     :error jkl-error)
  
    #-(or sbcl)
    (error "no implemented"))
  )

;;;;;;;;;;;; tools for generate command
#|
(make-new-command "curl"
                  (mapcar (lambda (line) (parse-option-from-help 'option1 line))
                          help-lines)
                  :subcommand `(("subcommand name" '(options...) :subcommand ...))
                  )
|#

(defun short-option-table-insert (table opt)
  "insert the short options and solve the conflicts"
  (when (string= "" (the string (short-option opt))) (return-from short-option-table-insert nil))
  (if (gethash (str:upcase (short-option opt)) table)
      (loop with orginal-o = (str:upcase (short-option opt))
            for n fixnum from 1
            ;; add the number after the keyword become new-keyword
            for new-keyword = (str:concat orginal-o (write-to-string n)) 
            unless (gethash new-keyword table)
              do (setf (gethash new-keyword table) `(t ,opt))
              and return new-keyword)
      (setf (gethash (str:upcase (short-option opt)) table) `(t ,opt))))

(defun make-new-command (name options &key subcommand)
  "make the new command"
  (let ((cmd (make-instance 'command :name name)))
    (loop with table = (make-hash-table :test 'equal)
          for opt in options
          do (short-option-table-insert table opt)
          do (if (long-option opt)
                 (setf (gethash (str:upcase (long-option opt)) table)
                       `(nil ,opt)))
          finally (setf (options cmd) table))

    (if subcommand
        (loop with table = (make-hash-table :test 'equal)
              for subc in subcommand
              do (setf (gethash (first subc) table) (apply #'make-new-command subc))
              finally (setf (subcommand cmd) table)
              ))
    cmd
    ))

(defun parse-option-from-help (class line)
  "parse line to option class"
  (let ((opt (make-instance class)))
    (if (option-match-string opt line)
        opt)
    ))

(defun read-line-content (content)
  "return each lines of content"
  (loop with s = (make-string-input-stream content)
        for l = (read-line s nil 'eof)
        if (eql 'eof l)
          return result
        else 
          collect l into result
        ))
