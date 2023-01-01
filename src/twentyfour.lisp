(load "file.lisp")
(load "dfss.lisp")
(ql:quickload :array-operations)
(ql:quickload :alexandria)

(defparameter +time-limit+ 20)

(defparameter chars-e (read-array "twentyfour-example-input.txt" 'char))
(defparameter chars-c (read-array "twentyfour-complex-input.txt" 'char))
(defparameter chars (read-array "twentyfour-input.txt" 'char))

(defstruct state
  ;; list of blizzards, will evolve
  blz
  ;; occupancy, derived from walls + blizzard position
  a)

(defstruct blizzard
  (direction #\> :type character)
  (x 0 :type integer)
  (y 0 :type integer))


(defun state/parse (char-array)
  (let ((a nil)
        (blz nil))
    ;; go over all chars, parsing the blizzards, and returning the occupancy array
    (setq a (aops:each-index (i j)
              (let ((c (aref char-array i j)))
                ;; side effect: add the blizzard to the list
                (when (or (char= c #\>)
                          (char= c #\v)
                          (char= c #\<)
                          (char= c #\^))
                  (push (make-blizzard
                         :direction c
                         :x j
                         :y i)
                        blz))
                ;; return occupancy value
                (if (char= c #\.) 0 1))))
    (make-state :blz blz :a a)))

(defun state/copy (s)
  "Need deep copy for state machinery"
  (make-state :blz (loop for b in (state-blz s)
                         collect (copy-blizzard b))
              :a (alexandria:copy-array (state-a s))))

(defun set-occupancy-from-blizzard-list (a blz)
  "set central (non-wall) values to zero, then go over blizzards and fill everything in"
  (aops:each-index! a (i j)
    (if (and (> i 0)
             (< i (1- (array-dimension a 0)))
             (> j 0)
             (< j (1- (array-dimension a 1))))
        ;; clear central values
        0
        ;; keep edge values
        (aref a i j)))

  (loop for b in blz
        for j = (blizzard-x b)
        for i = (blizzard-y b)
        do (incf (aref a i j))))

(defun state/step (s)
  "No arguments, as the evolution is fixed

   1. update blizzards

   2. update occupancy array"
  (let ((ny-2 (- (array-dimension (state-a s) 0) 2))
        (nx-2 (- (array-dimension (state-a s) 1) 2)))
    (loop for b in (state-blz s)
          for c = (blizzard-direction b)
          for y = (blizzard-y b)
          for x = (blizzard-x b)
          ;; update its position
          do (cond ((char= c #\>)
                    ;; x++, if x == n-2, go back to 0 + 1
                    (setf (blizzard-x b) 
                          (if (= x nx-2)
                              1
                              (1+ x))))
                   ((char= c #\v)
                    ;; y++
                    (setf (blizzard-y b)
                          (if (= y ny-2)
                              1
                              (1+ y))))
                   ((char= c #\<)
                    ;; x--
                    (setf (blizzard-x b)
                          (if (= x 1)
                              nx-2
                              (1- x))))
                   ((char= c #\^)
                    ;; y--
                    (setf (blizzard-y b)
                          (if (= y 1)
                              ny-2
                              (1- y)))))))
  (set-occupancy-from-blizzard-list (state-a s) (state-blz s)))

(defun dfss-end-p (s)
  (let ((x (getf s :x))
        (y (getf s :y))
        (occupancy (state-a (getf s :field))))
  "End criterion is a. killed by blizzard b. reached the goal"
  (or
   ;; final position
   (and (= x (- (array-dimension occupancy 1) 2))
        (= y (- (array-dimension occupancy 0) 1)))
   ;; killed (occupancy is not zero)
   (not (= (aref occupancy y x) 0))
   ;; timeout
   (> (getf s :time) +time-limit+))))
   
(defun dfss-score-f (s)
  (let ((x (getf s :x))
        (y (getf s :y))
        (occupancy (state-a (getf s :field))))
    ;; print every time the score gets evaluated. Good way to track end of branches.
    (print (getf s :time))
    (cond
      ;; if at the final square, score needs to be higher for shorter time
      ((and (= x (- (array-dimension occupancy 1) 2))
            (= y (- (array-dimension occupancy 0) 1)))
       (/ 1 (getf s :time)))
      ;; if anywhere else, score zero because we're either killed, stuck, or back at the start
      (t 0))))

(defun dfss-next-states-f (s)
  "Return states corresponding to steps that will not kill us (assuming simultaneous blizzard and
player movement)"
  (let* ((x (getf s :x))
         (y (getf s :y))
         (nx (array-dimension (state-a (getf s :field)) 1))
         (ny (array-dimension (state-a (getf s :field)) 0))
         ;; make sure to copy, so that we can rewind a dead branch
         (new-field (state/copy (getf s :field)))
         ;; possible new xs (right down left up wait)
         (new-xs (list (1+ x) x (1- x) x x))
         (new-ys (list y (1+ y) y (1- y) y)))
    ;; first, evolve the blizzards, so we know where we can go
    (state/step new-field)

    ;; if we're next to the goal, always return only that move
    (if (and (= x (- nx 2))
             (= y (- ny 2)))
        (list
         (list :time (1+ (getf s :time))
               :x (- nx 2)
               :y (- ny 1)
               :field new-field))
        ;; else, try options
        (loop for new-x in new-xs
              for new-y in new-ys
              ;; do not include in moveset if...
              unless (or
                      ;; ... out of bounds
                      (< new-x 0)
                      (>= new-x nx)
                      (< new-y 0)
                      (>= new-y ny)
                      ;; ... we go back to the starting position (waiting still allowed). In other
                      ;; words, (1 1) to (0 1) is not allowed.
                      (and (= y 1) (= x 1) (= new-y 0) (= new-x 1))
                      ;; ... we would collide
                      (> (aref (state-a new-field) new-y new-x) 0))
                collect (list :time (1+ (getf s :time))
                              :x new-x
                              :y new-y
                              :field new-field)))))

(defun do-puzzle (char-array)
  (let ((dfss-cur-state (list
                         :time 0
                         :x 1
                         :y 0
                         :field (state/parse char-array))))
    (print "initial state")
    (print dfss-cur-state)
    (dfss-score-f
     (dfss/recurse dfss-cur-state
                   #'dfss-end-p
                   #'dfss-score-f
                   #'dfss-next-states-f))))
