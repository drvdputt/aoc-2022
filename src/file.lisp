(require "asdf")
(defun read-lines (fn)
  (uiop:read-file-lines fn))

(defun read-default-input (prefix)
  (read-lines (pathname (concatenate 'string prefix "-input.txt"))))
(provide "file")

;; assumes that all lines are of equal length
;; supported read-in types: numbers, chars
(load "string.lisp")
(defun read-array (fn type)
  (let* ((lines (read-lines fn))
	 (nrow (length lines))
	 (ncol (length (first lines)))
	 (a (make-array (list nrow ncol) :element-type type)))
    (loop for l in lines
	  for i from 0
	  with f = (cond ((eq type 'char)
		          (lambda (line) (loop for c across line collect c)))
                         ((eq type 'integer)
		          (function find-all-int)))
	  do (loop for x in (funcall f l)
		   for j from 0
		   do (setf (aref a i j) x)))
    a))
  
