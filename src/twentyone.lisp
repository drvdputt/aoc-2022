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

(defun do-part2 (lines guess-min guess-max &key (method 'scan) (stijgend nil))
  (let* (;; we found two numbers. Guess min has the value too big,
         ;; and guess max has it to small. So do bisection between
         ;; those.
         (table (parse-monkey-table lines))
         (root-dep1 (monkey-dep1 (gethash "root" table)))
         (root-dep2 (monkey-dep2 (gethash "root" table)))
         (vmin guess-min)
         (vmax guess-max)
         (humn-value (cond ((eq method 'scan) vmin)
                           ((eq method 'bisect) (floor (+ guess-min guess-max) 2))))
         (v1 nil)
         (v2 nil))
    (flet ((refresh-v1-v2 ()
             ;; reset everything with new humn value
             (reset-monkey-values table)
             (setf (monkey-value (gethash "humn" table)) humn-value)
             (setq v1 (process-monkeys table root-dep1)
                   v2 (process-monkeys table root-dep2))
             (print (list "humn v1 v2" humn-value v1 v2))))
    ;; (print (loop for k being each hash-key of table collect k))
    ;; (print root-dep1)
    ;; (print root-dep2)
    (cond ((eq method 'bisect)
           (loop
             ;; move humn to middle of the bounds
             do (setq humn-value (floor (+ vmin vmax) 2))

                ;; calc v1 and v2
             do (refresh-v1-v2)
             do (print (list "vmin humn vmax" vmin humn-value vmax))

                ;; check return condition
             if (or (= v1 v2) (< (abs (- vmin vmax)) 2))
               return humn-value

             ;; adjust search bounds
             if (or (and stijgend (< v1 v2))
                    (and (not stijgend) (> v1 v2)))
               ;; if stijgend and too small, or dalend and too big, need bigger number
               do (setq vmin humn-value)
             else
               ;; else, need smaller number
               do (setq vmax humn-value)))
          
          ((eq method 'scan)
           (loop
             ;; calc v1 and v2
             do (refresh-v1-v2)

                ;; check return condition (actually, we want to scan everything
                ;; if (or (= v1 v2) (< (abs (- vmin vmax)) 2))
                ;;   return humn-value
             if (= v1 v2)
               do (print "^^^^^^^")
                
             if (> humn-value vmax)
               return nil
                
             ;; increment humn by 1
             do (incf humn-value)))))))

;; part 2 needs two parts

;; first: do bisection search to find the right range
;; (do-part2 lines 0 1000000000000000 :method 'bisect :stijgend nil)

;; then, look at the output, and do a scan to find the first humn value for which we are
;; successful. (there are 6)
;; (do-part2 lines 3059361893915 3059361893929 :method 'scan :stijgend nil)
