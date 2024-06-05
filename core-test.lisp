(defpackage :jkl-test
  (:use :cl :jkl :jkl-options)
  (:import-from :fiveam
                :def-suite
                :in-suite
                :test
                :is
                :signals
                :def-fixture
                :with-fixture)
  (:export
   :core
   :gen-options)
  )

(in-package :jkl-test)

(def-suite core
  :description "test core")

(def-fixture ignore-case ()
  (let ((*ignore-case* t))
    (&body)))

(def-fixture omit-nulls ()
  (let ((*omit-nulls* t))
    (&body)))

(def-suite gen-options
  :in core
  :description "gen option test")

(in-suite gen-options)

(test case0
  (let* ((options-str '("-K, --config <file> Read config from a file"
                        "-a, --append      Append to target file when uploading"))
         (opts (loop for line in options-str
                     collect (let ((opt (make-instance 'option1)))
                               (option-match-string opt line)
                               opt)))
         (comm (make-new-command "aa" opts)))
    (is (equal '("-K" "hello")
               (jkl:gen-options comm :k "hello")))
    (is (equal '("-K" "hello" "-a")
               (jkl:gen-options comm :k "hello" :a t)))
    (is (equal '("-K" "hello" "--append")
               (jkl:gen-options comm :K "hello" :append t)))
    (is (equal '("--append" "--config" "hello")
               (jkl:gen-options comm :append t :config "hello")))))

(test case1
  (let* ((options-str '("  -r,  --recursive                 specify recursive download"
                        " -l,  --level=NUMBER              maximum recursion depth (inf or 0 for infinite)"
                        "       --convert-file-only         convert the file part of the URLs only (usually known as the basename)"))
         (opts (loop for line in options-str
                     collect (let ((opt (make-instance 'option2)))
                               (option-match-string opt line)
                               opt)))
         (comm (make-new-command "aa" opts)))
    (is (equal '("-r")
               (jkl:gen-options comm :r t)))
    (is (equal '("-l" "12")
               (jkl:gen-options comm :l 12)))
    (is (equal '("--convert-file-only" "-r" "-l" "12")
               (jkl:gen-options comm :convert-file-only t :r t :L 12)))))

(test subcmd-case0
  (let ((cmd (jkl:make-new-command
               "top"
               (mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option1 line))
                       '("--capath <dir> CA directory to verify peer against"
                         "-E, --cert <certificate[:password]> Client certificate file and password"))
               :subcommand `(("a" ,(mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option1 line))
                                           '("--capath <dir> CA directory to verify peer against"
                                             "-E, --cert <certificate[:password]> Client certificate file and password")))))))
    (is (equal '("-E" "cert" "a" "--cert" "cert")
               (jkl:gen-options cmd :e "cert" "a" :cert "cert")))
    ))

(test subcmd-case1
  (let ((cmd (jkl:make-new-command
              "top"
              (mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option1 line))
                      '("--capath <dir> CA directory to verify peer against"
                        "-E, --cert <certificate[:password]> Client certificate file and password"))
              :subcommand `(("a" ,(mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option1 line))
                                          '("--capath <dir> CA directory to verify peer against"
                                            "-E, --cert <certificate[:password]> Client certificate file and password"))
                                 :subcommand (("b" ,(mapcar (lambda (line) (jkl:parse-option-from-help 'jkl-options:option2 line))
                                                            '(" -r,  --recursive                 specify recursive download")))))))))
    (is (equal '("-E" "cert" "a" "-E" "cert" "b" "-r")
               (jkl:gen-options cmd :e "cert" "a" :e "cert" "b" :r t)))
    (is (equal '("-E" "cert" "a" "--cert" "cert" "b" "--recursive")
               (jkl:gen-options cmd :e "cert" "a" :cert "cert" "b" :recursive t)))
    ))
