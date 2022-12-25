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

(defun reset-monkey-values (monkey-table)
  (loop for m being each hash-value of monkey-table
        ;; if monkey has an operation, reset its value to nil
        if (monkey-operation m)
          do (setf (monkey-value m) nil)))

(defun do-part2 (lines)
  (let* ((table (parse-monkey-table lines))
         (root-dep1 (monkey-dep1 (gethash "root" table)))
         (root-dep2 (monkey-dep2 (gethash "root" table))))
    ;; (print (loop for k being each hash-key of table collect k))
    ;; (print root-dep1)
    ;; (print root-dep2)
    (loop for humn-value from 0 to 16
          do (reset-monkey-values table)
          if (progn
               (setf (monkey-value (gethash "humn" table)) humn-value)
               (let* ((v1 (process-monkeys table root-dep1))
                      (v2 (process-monkeys table root-dep2)))
                 (print (list v1 v2))
                 (= v1 v2)))
            return humn-value)))

;; Part two needs some human attention. v2 is always equal to 
  
