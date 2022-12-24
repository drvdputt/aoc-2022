(load "file.lisp")
(load "string.lisp")
(load "num.lisp")
(load "list-tools.lisp")
(defparameter numbers (mapcar #'parse-integer (read-default-input "twenty")))
(defparameter numbers-e '(1 2 -3 3 -2 0 4))

(defun shift-element-position (position-table i delta)
  "Shift element by skipping over <delta> elements (with rollover).

  set p_i to p_i + 1 + delta (skips over <delta> elements, so when delta = 1, skip over 1 ->
  position goes from 0 to 2. delta = 2

  Forward case
  ------------
 
  0 1 2 3 4 5
  delta 1
  x 1 0 2 3 4 5
  0 now at position 2
  be shifted left)

  0 1 2 3 4 5
  delta 5
  x 1 2 3 4 5 (0 skips over 5 back to position 0)
  so delta + 1 = 6 should result in p-new = 0

  backwards case (skip over the one at the other end!)
  --------------

  0 1 2 3 4 5
  delta -1 from 1 (skip over zero and end up in final position)
            1
  0 x 2 3 4 5

  0 1 2 3 4 5
  delta -1
  x 1 2 3 4 0 5
  0 now at position (n - 1) 

  0 1 2 3 4 5
  delta -2
  x 1 2 3 0 4 5
  0 now at position (n - 2)

  Finalizing
  ----------

  Shifts then need to happen to remove the position marked as x.

  "
  (let* ((n (length position-table))
         (p-old (aref position-table i))
         ;; shift = delta + 1 if positive, delta - 1 if negative, 0 if 0
         (p-new (mod (+ p-old 
                        (if (> delta 0)
                            (1+ delta)
                            (1- delta)))
                     n)))
    (print (list "p-old" p-old "p-new" p-new))
    ;; Do the shifts, except when delta == 0 or pnew == pold 
    (unless (or (= delta 0) (= p-new p-old))
      ;; carefully think about forward and backward moves
      (setf (aref position-table i) p-new)
      
      ;; forward (p-new > p-old)
      ;; 0 1 2 3 4 5
      ;;     .-d2>2   delta 2 (index goes up by 3). 2 and 5 now have the same position
      ;; 0 1 x 3 4 5
      ;; 5 stays in place, 3 and 4 and 2 (p-new) shift left
      (when (> p-new p-old)
        (loop for j from 0 below n
              for pj across position-table
              ;; shift left if between p-old and p-new
              if (and (> pj p-old)
                      (< pj p-new))
                do (decf (aref position-table j)))
        ;; shift i separately, because it temporarily had the same index as the element that was
        ;; previously at p-new.
        (decf (aref position-table i)))

      ;; backward (p-new < p-old)
      ;; 0 1 2 3 4 5
      ;;    4         4 moves by delta -2
      ;; 0 1 2 3 x 5
      ;; 1 stays in place, 2 and 3 and 4 (p-new) shift right
      (when (< p-new p-old)
        (loop for j from 0 below n
              for pj across position-table
              ;; shift right if between p-new and p-old
              if (and (> pj p-new)
                      (< pj p-old))
                do (incf (aref position-table j)))
        ;; Same here. There were temporarily two element at p-new. Now that there's extra space
        ;; on the right, move i to p-new + 1.
        (incf (aref position-table i))))))

(defun mix (numbers)
  ;; The numbers are not unique!
  (let ((h (int-histogram numbers)))
    (loop for k being each hash-key of h
          if (> (gethash k h) 1)
            do (print (list k (gethash k h)))))
  ;; instead of working with a list which we constantly modify, and searching the number in it,
  ;; make a table to keep track of the position of each number. This table is a vector, where
  ;; the position in the vector (index) refers to the original order, and the value refers to
  ;; the new order. At the start, value = index
  (let* ((nn (length numbers))
         (position-table
           (make-array
            nn
            :element-type 'integer
            :initial-contents (loop for i from 0 below nn collect i))))
    (loop for v in numbers
          for i from 0
          do (print v)
          do (shift-element-position position-table i v)
          do (print position-table)
          do (pprint-position-table numbers position-table))))

(defun pprint-position-table (numbers position-table)
  (print (loop for v in numbers
               for p across position-table
               with a = (make-array (length numbers) :element-type 'integer)
               do (setf (aref a p) v)
               finally (return a))))
                                      
