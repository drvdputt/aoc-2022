;; collect unique values
(defun list-tools/unique (input-list test)
  (let ((unique-entries nil))
    (loop for item in input-list
          if (not (member item unique-entries :test test))
            do (push item unique-entries))
    unique-entries))


(defun pprint-hashmap (h)
  (loop for k being each hash-key of h
        do (print
            (concatenate
             'string
             k
             " : "
             (write-to-string (gethash k h))))))
                               
