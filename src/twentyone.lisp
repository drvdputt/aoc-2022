(load "file.lisp")
(defparameter lines-e (read-default-input "twentyone-example"))
(defparameter lines (read-default-input "twentyone"))

(defstruct monkey
  name
  value ;; nil by default. Will be set once the monkey is able to execute its operation.
  operation ;; operation and dependens will remain nil for constant monkeys
  dep1
  dep2)

(defun parse-monkey (line)
  ;; name is the first 4 characters
  (let* ((name (subseq line 0 4))
         (is-operation (= (length line) 17))
         (m (make-monkey :name name)))
    (if is-operation
        ;; operation monkey: initial value is nil
        (setf (monkey-dep1 m) (subseq line 6 10)
              (monkey-dep2 m) (subseq line 13)
              (monkey-operation m) (let ((c (char line 11)))
                                     (cond ((char= c #\*) #'*)
                                           ((char= c #\-) #'-)
                                           ((char= c #\+) #'+)
                                           ((char= c #\/) #'floor))))
        ;; constant monkey: just set the number
        (setf (monkey-value m) (parse-integer (subseq line 6))))
    m))

(defun parse-monkey-table (lines)
  (let ((table (make-hash-table :test 'equal)))
    (loop for l in lines
          for m = (parse-monkey l)
          do (setf (gethash (monkey-name m) table) m))
    table))

(defun process-monkeys (monkey-table start)
  (let* ((m (gethash start monkey-table))
         (value (monkey-value m)))
    (if value
        ;; if this is not nil, return it
        value
        ;; Else, recurse to get dependant values, and apply the operation to them.
        (funcall (monkey-operation m)
                 (process-monkeys monkey-table (monkey-dep1 m))
                 (process-monkeys monkey-table (monkey-dep2 m))))))
        
        
(defun do-puzzle (lines start)
  (let ((table (parse-monkey-table lines)))
    (process-monkeys table start)))
