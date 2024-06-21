(in-package :jkl-cmd)

(defparameter *git-add*
  (read-file-lines
   (merge-pathnames
    "static/git/git-add.txt"
    (pathname-parent-directory-pathname
     (pathname-directory-pathname #.(or *compile-file-truename* *load-truename*))))))

;; (defparameter *git*)

;; (defun git (&rest args)
;;   (apply #'run *curl* args))

;; (export '(git *git*))
