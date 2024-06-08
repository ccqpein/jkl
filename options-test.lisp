(defpackage :jkl-options-test
  (:use :cl)
  (:import-from :fiveam
                :def-suite
                :in-suite
                :test
                :is
                :signals
                :def-fixture
                :with-fixture)
  
  (:import-from
   :jkl-options
   :option1
   :option2
   :option-match-string
   :restore-back-to-string
   :option1-match-string
   :option2-match-string)
  
  (:export
   :options
   :option1-match
   :option2-match)
  )

(in-package :jkl-options-test)

(def-suite options
  :description "test options")

(def-fixture ignore-case ()
  (let ((*ignore-case* t))
    (&body)))

(def-fixture omit-nulls ()
  (let ((*omit-nulls* t))
    (&body)))

(def-suite option1-match
  :in options
  :description "test string match for option1")

(in-suite option1-match)

(test option1-match-case0
  (is (equal (multiple-value-list (option1-match-string "-K, --config <file> Read config from a file"))
             '("K" "config" "file" "Read config from a file" t)))
  (is (equal (multiple-value-list (option1-match-string "-a, --append      Append to target file when uploading"))
             '("a" "append" "" "Append to target file when uploading" t)))
  (is (equal (multiple-value-list (option1-match-string "--capath <dir> CA directory to verify peer against"))
             '("" "capath" "dir" "CA directory to verify peer against" t)))
  (is (equal (multiple-value-list (option1-match-string "Usage: curl [options...] <url>"))
             '("" "" "" "" nil))))

(def-suite option2-match
  :in options
  :description "test string match for option2")

(in-suite option2-match)

(test option2-match-case0
  (is (equal (multiple-value-list (option2-match-string "  -r,  --recursive                 specify recursive download"))
             '("r" "recursive" "" "specify recursive download" t)))
  
  (is (equal (multiple-value-list (option2-match-string " -l,  --level=NUMBER              maximum recursion depth (inf or 0 for infinite)"))
             '("l" "level" "NUMBER" "maximum recursion depth (inf or 0 for infinite)" t)))
  
  (is (equal (multiple-value-list (option2-match-string "       --convert-file-only         convert the file part of the URLs only (usually known as the basename)"))
             '("" "convert-file-only" "" "convert the file part of the URLs only (usually known as the basename)" t)))

  (is (equal (multiple-value-list (option2-match-string "       --warc-max-size=NUMBER      set maximum size of WARC files to NUMBER
"))
             '("" "warc-max-size" "NUMBER" "set maximum size of WARC files to NUMBER" t))))

(test option2-restore-back-to-string
  (let ((x (make-instance 'option2)))
    (option-match-string x " -l,  --level=NUMBER              maximum recursion depth (inf or 0 for infinite)")
    (is (equal '("-l" "3") (restore-back-to-string x 3 :short-option)))
    (is (equal '("--level=3") (restore-back-to-string x 3))))

  (let ((x (make-instance 'option2)))
    (option-match-string x "       --convert-file-only         convert the file part of the URLs only (usually known as the basename)")
    (signals error (restore-back-to-string x t :short-option))
    (is (equal '("--convert-file-only") (restore-back-to-string x t)))))

(def-suite option3-match
  :in options
  :description "test string match for option3")

(in-suite option3-match)

(test option3-match-case0
  (is (equal (multiple-value-list (option1-match-string "  -r, --refresh         refresh the all quiz cache"))
             '("r" "refresh" "" "refresh the all quiz cache" t)))
  (is (equal (multiple-value-list (option1-match-string "  -o, --output <VALUE>  output file"))
             '("o" "output" "VALUE" "output file" t)))
  (is (equal (multiple-value-list (option1-match-string "  -n, --id <INT>        the id of quiz"))
             '("n" "id" "INT" "the id of quiz" t)))
  )

(def-suite option4-match
  :in options
  :description "test string match for option4")

(in-suite option4-match)

(test option4-match-case0
  (is (equal (multiple-value-list (option1-match-string "  -D, --del                            Delete the crumbs"))
             '("D" "del" "" "Delete the crumbs" t)))
  (is (equal (multiple-value-list (option1-match-string "  -R, --restore                        Restore the crumbs back to normal comment"))
             '("R" "restore" "" "Restore the crumbs back to normal comment" t)))
  (is (equal (multiple-value-list (option1-match-string "      --fmt <FMT_COMMAND>              Format command after delete crumbs"))
             '("" "fmt" "FMT_COMMAND" "Format command after delete crumbs" t)))
  (is (equal (multiple-value-list (option1-match-string "  -O, --output-format <OUTPUT_FORMAT>  Output format: json, list"))
             '("O" "output-format" "OUTPUT_FORMAT" "Output format: json, list" t)))
  )
