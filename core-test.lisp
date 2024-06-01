(defpackage :jkl-core-test
  (:use :cl :jkl-core :jkl-options)
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

(in-package :jkl-core-test)

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
         (opts (loop with table = (make-hash-table :test 'equal)
                     for s in options-str
                     for opt = (make-instance 'option1)
                     do (option-match-string opt s)
                     do (progn
                          (if (short-option opt) (setf (gethash (str:upcase (short-option opt)) table) opt))
                          (if (long-option opt) (setf (gethash (str:upcase (long-option opt)) table) opt)))
                     finally (return table)))
         (comm (make-instance 'command
                              :name "aa"
                              :options opts)))
    (is (equal '("--config" "hello")
               (jkl-core::gen-options comm :k "hello")))
    (is (equal '("--config" "hello" "--append")
               (jkl-core::gen-options comm :k "hello" :a t)))
    (is (equal '("--config" "hello" "--append")
               (jkl-core::gen-options comm :K "hello" :append t)))
    (is (equal '("--append" "--config" "hello")
               (jkl-core::gen-options comm :append t :config "hello")))))

(test case1
  (let* ((options-str '("  -r,  --recursive                 specify recursive download"
                        " -l,  --level=NUMBER              maximum recursion depth (inf or 0 for infinite)"
                        "       --convert-file-only         convert the file part of the URLs only (usually known as the basename)"))
         (opts (loop with table = (make-hash-table :test 'equal)
                     for s in options-str
                     for opt = (make-instance 'option2)
                     do (option-match-string opt s)
                     do (progn
                          (if (short-option opt) (setf (gethash (str:upcase (short-option opt)) table) opt))
                          (if (long-option opt) (setf (gethash (str:upcase (long-option opt)) table) opt)))
                     finally (return table)))
         (comm (make-instance 'command
                              :name "aa"
                              :options opts)))
    (is (equal '("--recursive")
               (jkl-core::gen-options comm :r t)))
    (is (equal '("--level=12")
               (jkl-core::gen-options comm :l 12)))
    (is (equal '("--convert-file-only" "--recursive" "--level=12")
               (jkl-core::gen-options comm :convert-file-only t :r t :L 12)))))

(test subcmd-case0
  (let ((cmd (jkl-core:make-new-command
               "top"
               (mapcar (lambda (line) (jkl-core:parse-option-from-help 'jkl-options:option1 line))
                       '("--capath <dir> CA directory to verify peer against"
                         "-E, --cert <certificate[:password]> Client certificate file and password"))
               :subcommand `(("a" ,(mapcar (lambda (line) (jkl-core:parse-option-from-help 'jkl-options:option1 line))
                                           '("--capath <dir> CA directory to verify peer against"
                                             "-E, --cert <certificate[:password]> Client certificate file and password")))))))
    (is (equal '("--cert" "cert" "a" "--cert" "cert")
               (jkl-core::gen-options cmd :e "cert" "a" :e "cert")))
    ))

(test subcmd-case1
  (let ((cmd (jkl-core:make-new-command
              "top"
              (mapcar (lambda (line) (jkl-core:parse-option-from-help 'jkl-options:option1 line))
                      '("--capath <dir> CA directory to verify peer against"
                        "-E, --cert <certificate[:password]> Client certificate file and password"))
              :subcommand `(("a" ,(mapcar (lambda (line) (jkl-core:parse-option-from-help 'jkl-options:option1 line))
                                          '("--capath <dir> CA directory to verify peer against"
                                            "-E, --cert <certificate[:password]> Client certificate file and password"))
                                 :subcommand (("b" ,(mapcar (lambda (line) (jkl-core:parse-option-from-help 'jkl-options:option2 line))
                                                            '(" -r,  --recursive                 specify recursive download")))))))))
    (is (equal '("--cert" "cert" "a" "--cert" "cert" "b" "--recursive")
               (jkl-core::gen-options cmd :e "cert" "a" :e "cert" "b" :r t)))
    ))
