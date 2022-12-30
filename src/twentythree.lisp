(load "file.lisp")
(load "num.lisp")
(ql:quickload :array-operations)
(ql:quickload :alexandria)
;; reuse old function

(defparameter a-small (read-array "twentythree-small-input.txt" 'char))

;; putting everything on a grid would results in O(N) runtime for neighbor checking (go over
;; entire array, and for every position, check 9 squares if elf).

;; working without a grid would result in O(N**2) scaling (need to go over all points)

;; So having an array, and just eating the overhead of expanding it every now and then, would
;; definitely be preferable.
(defstruct elf-array
  a
  yoffset
  xoffset
  )

(defun elf-array/shrinkwrap (xys)
  "Make an elf-array instance that just fits around the list of elves

   Will be filled with 0 and 1. Higher integers when xys contains duplicates."
  (let* ((xymax ;; get maximum of each column
           (aops:each-index j
             (aops:reduce-index #'max i
               (aref xys i j))))
         (xymin ;; get minimum of each column
           (aops:each-index j
             (aops:reduce-index #'min i
               (aref xys i j))))
         ;; first y then x for array dimensions
         (dimensions (list (1+ (- (aref xymax 1) (aref xymin 1)))
                           (1+ (- (aref xymax 0) (aref xymin 0)))))
         (ea (make-elf-array :a (make-array dimensions :element-type 'integer :initial-element 0)
                             :xoffset (elt xymin 0)
                             :yoffset (elt xymin 1))))
    (loop for n from 0 below (array-dimension xys 0)
          do (incf (elf-array/get ea (aref xys n 0) (aref xys n 1))))
    ;; (print xymax)
    ;; (print xymin)
    ;; (print dimensions)
    ;; (print ea)
    ea))

(defun elf-array/get (ea x y)
  (let ((x0 (- x (elf-array-xoffset ea)))
        (y0 (- y (elf-array-yoffset ea))))
    (aref (elf-array-a ea) y0 x0)))

(defun elf-array/occupied (ea x y)
  "Returns nil if free or out of bounds"
  (let ((a (elf-array-a ea))
        (x0 (elf-array-xoffset ea))
        (y0 (elf-array-yoffset ea)))
    (if (or (< x x0) (>= x (+ x0 (array-dimension a 1)))
            (< y y0) (>= y (+ y0 (array-dimension a 0))))
        ;; out of bounds
        nil
        ;; check occupancy
        (> (elf-array/get ea x y) 0))))

(defun (setf elf-array/get) (new-value ea x y)
  "here, I learn how to use a setf expander"
  (let ((x0 (- x (elf-array-xoffset ea)))
        (y0 (- y (elf-array-yoffset ea))))
    (setf (aref (elf-array-a ea) y0 x0) new-value)))

(defparameter directions
  '('north 'south 'west 'east))

(defstruct elf-state
  xys ;; current position of each elf
  ;; rotates between each step (north south west east)
  (nswe 0))

(defun elf-state/init (file)
  (let ((char-a (read-array file 'char))
        (es (make-elf-state))
        (N 0))
    ;; first, count the number of elves
    (setq N (loop for c across (aops:flatten char-a)
                  count (char= c #\#)))

    ;; then, set up arrays for these positions, and fill them in
    (let ((xys (make-array (list N 2) :element-type 'integer))
          (n 0))
      (aops:each-index (i j)
        (when (char= (aref char-a i j) #\#)
          ;; x = j, y = i
          (setf (aref xys n 0) j
                (aref xys n 1) i)
          (incf n)))
      (setf (elf-state-xys es) xys))
    es))

(defun elf-array/nswe-free (ea x y nswe)
  "Return t if direction is free, based on the check described in the instructions"
  (cond
    ;; north: check N, NE, NW
    ((= nswe 0)
     (loop for xi from (1- x) to (1+ x)
           if (elf-array/occupied ea xi (1- y))
             return nil
           finally (return t)))
    ;; south
    ((= nswe 1)
     (loop for xi from (1- x) to (1+ x)
           if (elf-array/occupied ea xi (1+ y))
             return nil
           finally (return t)))
    ;; west
    ((= nswe 2)
     (loop for yi from (1- y) to (1+ y)
           if (elf-array/occupied ea (1- x) yi)
             return nil
           finally (return t)))
    ;; east
    ((= nswe 3)
     (loop for yi from (1- y) to (1+ y)
           if (elf-array/occupied ea (1+ x) yi)
             return nil
           finally (return t)))))

(defun move-x (nswe x)
  (cond
    ;; west
    ((= nswe 2) (1- x))
    ;; east
    ((= nswe 3) (1+ x))
    ;; north and south
    (t x)))

(defun move-y (nswe y)
  (cond
    ;; north
    ((= nswe 0) (1- y))
    ;; south
    ((= nswe 1) (1+ y))
    (t y)))

(defun elf-state/step (es)
  "part 1: fill in the proposed move array
     a. Set up position array
     b. Go over all elves, and check the position array for neighbors. This determines their move.

   part 2: check if proposed moves are possible
     a. count number of moves to each tile (store in array)
     b. go over all elves, and check count on their destination. Adjust xy[n] if elf n is free to move.

   part 3: advance pointers"
  (let* ((N (array-dimension (elf-state-xys es) 0))
         ;; set up position array (for neighbor checking)
         (xys (elf-state-xys es))
         (ea (elf-array/shrinkwrap xys))
         ;; set up move count array (for move conflict checking)
         (move-xys (alexandria:copy-array xys))
         (ea-move nil))

    ;; set move direction for all points
    (loop for n from 0 below N
          for x = (aref xys n 0)
          for y = (aref xys n 1)
          ;; try the 4 directions (with mod rollover)
          for free-directions = (loop for i from 0 below 4
                                      for nswe = (mod (+ (elf-state-nswe es) i) 4)
                                      if (elf-array/nswe-free ea x y nswe)
                                        collect nswe)
          ;; If at least one is free, move into the first available direction. But if all 4 are
          ;; free, do nothing.
          if (and free-directions
                  (not (= (length free-directions) 4)))
            ;; do (print (list x y "can move in direction" nswe))
            do (setf (aref move-xys n 0) (move-x (first free-directions) x)
                     (aref move-xys n 1) (move-y (first free-directions) y)))

    ;; (print "move goals")
    ;; (print move-xys)

    ;; count moves to each tile
    (setq ea-move (elf-array/shrinkwrap move-xys))

    ;; (print "positions")
    ;; (pprint-int-array (elf-array-a ea))

    ;; (print "move")
    ;; (pprint-int-array (elf-array-a ea-move))

    ;; check for conflicts and execute moves
    (loop for n from 0 below N
          for x-move = (aref move-xys n 0)
          for y-move = (aref move-xys n 1)
          ;; if only one elf is moving to this tile, we can do the move, otherwise do nothing
          if (= (elf-array/get ea-move x-move y-move) 1)
            do (setf (aref (elf-state-xys es) n 0) x-move
                     (aref (elf-state-xys es) n 1) y-move))

    ;; advance pointers
    (setf (elf-state-nswe es)
          (mod (1+ (elf-state-nswe es)) 4))
    es))



(defun do-puzzle (fn num-steps)
  (loop repeat num-steps
        with es = (elf-state/init fn)
        do (pprint-int-array (elf-array-a (elf-array/shrinkwrap (elf-state-xys es))))
        do (print "-------")
        do (elf-state/step es)
        finally (pprint-int-array (elf-array-a (elf-array/shrinkwrap (elf-state-xys es))))))
           
