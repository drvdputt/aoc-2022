(load "file.lisp")
(load "num.lisp")
(defparameter lines (read-default-input "twentytwo"))
(defparameter lines-e (read-default-input "twentytwo-example"))

(defun parse-map (lines)
  "Read map into char array from input lines."
  (let* ((h (- (length lines) 2))
         (w (apply #'max (mapcar #'length (subseq lines 0 (1+ h)))))
         (a (make-array (list h w) :element-type 'character :initial-element #\ )))
    (loop for l in lines
          for i from 0 below h
          do (loop for c across l
                   for j from 0
                   do (setf (aref a i j) c)))
    ;; (pprint-char-array a)
    a))

(defun parse-instructions (lines)
  "Get lists :moves and :turns

   There is always one more move than turns.

   These should be executed as move turn move turn move"
  (let* ((l (car (last lines)))
         (numbers (find-all-int l))
         (letters (loop for c across l
                        if (or (char= c #\L) (char= c #\R))
                          collect c)))
    (list :moves numbers
          :turns letters)))

(defstruct wrapmap
  char-a ;; the map data
  ;; start and end of wrap-around for each row
  row-start-v
  ;; end is exclusive! (easier for modulo etc)
  row-end-v
  ;; start and end of wrap-around for each column
  col-start-v
  col-end-v)

(defun wrapmap/init (a)
  (let* ((nrow (array-dimension a 0))
         (ncol (array-dimension a 1))
         (wm (make-wrapmap :char-a a
                           :row-start-v (make-array nrow :element-type 'integer)
                           :row-end-v (make-array nrow :element-type 'integer)
                           :col-start-v (make-array ncol :element-type 'integer)
                           :col-end-v (make-array ncol :element-type 'integer))))
    (loop for i from 0 below nrow
          with start = nil
          with end = nil
          do (loop for j from 0 below ncol
                   for space-p = (char= (aref a i j) #\ )
                   with previous-inside-p = nil
                   ;; if outside, and character is not space, we have now entered!
                   if (and (not previous-inside-p) (not space-p))
                     do (setq previous-inside-p t
                              start j)
                        ;; if inside, and we encounter a space, we are outside again, so we can return
                   if (and previous-inside-p space-p)
                     do (setq end j)
                     and return nil
                   ;; if no more spaces encountered
                   finally (setq end ncol))
          do (setf (aref (wrapmap-row-start-v wm) i) start
                   (aref (wrapmap-row-end-v wm) i) end))
    (loop for j from 0 below ncol
          with start = nil
          with end = nil
          do (loop for i from 0 below nrow
                   for space-p = (char= (aref a i j) #\ )
                   with previous-inside-p = nil
                   ;; if outside, and character is not space, we have now entered!
                   if (and (not previous-inside-p) (not space-p))
                     do (setq previous-inside-p t
                              start i)
                        ;; if inside, and we encounter a space, we are outside again, so we can return
                   if (and previous-inside-p space-p)
                     do (setq end i)
                     and return nil
                   ;; if no more spaces encountered
                   finally (setq end nrow))
          do (setf (aref (wrapmap-col-start-v wm) j) start
                   (aref (wrapmap-col-end-v wm) j) end))
    wm))


(defun wrap-generic (x start end)
  "Force x to be within [start, end[ by wrapping."
  (+ start (mod (- x start)
                (- end start))))

(defun wrapmap/wrap-i (wm i j)
  (let ((start (elt (wrapmap-col-start-v wm) j))
        (end (elt (wrapmap-col-end-v wm) j)))
    (wrap-generic i start end)))

(defun wrapmap/wrap-j (wm i j)
  (let ((start (elt (wrapmap-row-start-v wm) i))
        (end (elt (wrapmap-row-end-v wm) i)))
    (wrap-generic j start end)))

(defun wrapmap/get-value-wrapped (wm i j direction)
  "Wrap the given the given position if necessary, and return the map value."
  (let* ((horizontal (or (= direction 0)
                         (= direction 2)))
         (wi (if horizontal
                 i
                 (wrapmap/wrap-i wm i j)))
         (wj (if horizontal
                 (wrapmap/wrap-j wm i j)
                 j)))
    (aref (wrapmap-char-a wm) wi wj)))

;; direction is magic number 0, 1, 2, or 3 (with right = 0, and clockwise increase)
;; (defun do-turn (direction turn)
;;   (mod (cond ((char= turn #\R)
;;               (+ direction 1))
;;              ((char= turn #\L)
;;               (- direction 1)))
;;        4))

(defun do-move (wm i j direction distance)
  "a is the map. I and j indicate position (row and column). Direction is magic number 0, 1, 2,
   3 for r, d, l, u. Distance is int."
  (let ((horizontal (or (= direction 0) (= direction 2)))
        ;; direction 0 (right) and 1 (down) have delta + 1
        ;; the other two - 1
        (delta (if (< direction 2) 1 -1))
        (a (wrapmap-char-a wm)))
    (if horizontal
        (loop for counter from 0 below distance
              with j-cur = j
              for j-next = (wrapmap/wrap-j wm i (+ j-cur delta))
              do (print (list "moving j" j-cur j-next))
              if (char= (aref a i j-next) #\#)
                return (cons i j-cur)
              else
                do (setq j-cur j-next)
              finally (return (cons i j-cur)))
        ;; else vertical
        (loop for counter from 0 below distance
              with i-cur = i
              for i-next = (wrapmap/wrap-i wm (+ i-cur delta) j)
              ;; check for obstruction
              if (char= (aref a i-next j) #\#)
                return (cons i-cur j)
              else
                do (setq i-cur i-next)
              finally (return (cons i-cur j))))))


(defun do-puzzle (lines)
  (let* ((wm (wrapmap/init (parse-map lines)))
         (i 0)
         (j (aref (wrapmap-row-start-v wm) i))
         (ij (cons i j))
         (d 0)
         (instructions (parse-instructions lines)))

    (print wm)

    ;; do the first move
    (print ij)
    (setq ij (do-move wm (car ij) (cdr ij) d (first (getf instructions :moves))))
    (print ij)

    ;; then (turn, move), (turn, move), ...
    (loop for turn in (getf instructions :turns)
          for move in (cdr (getf instructions :moves))
          ;; apply turn
          do (setq d (do-turn d turn))
             ;; apply move
          do (setq ij (do-move wm (car ij) (cdr ij) d move))
          do (print (list turn move ij)))
    (+ (* 1000 (1+ (car ij)))
       (* 4 (1+ (cdr ij)))
       d)))

(defun which-face (i j N)
  "return which cube face we're in, depending on the coordinates. Only works for the big input
  (it has a different unfolding layout than the example...

  The returned numbers correspond to my drawing on paper.

  Returns -1 if off the 2d map (useful for detecting if we have to change face."
  (let ((ii (floor i N))
        (jj (floor j N)))
    (cond ((and (= ii 0) (= jj 1))
           1)
          ((and (= ii 0) (= jj 2))
           2)
          ((and (= ii 1) (= jj 1))
           3)
          ((and (= ii 2) (= jj 0))
           4)
          ((and (= ii 2) (= jj 1))
           5)
          ((and (= ii 3) (= jj 0))
           6)
          (t
           -1))))

(defun face-offset (f)
  "sort of inverse function. Returns the top left corner of each face."
  (cdr (assoc f '((1 . (0 . 1))
                  (2 . (0 . 2))
                  (3 . (1 . 1))
                  (4 . (2 . 0))
                  (5 . (2 . 1))
                  (6 . (3 . 0))))))

;; (defparameter cube-face-transform-type
;;   ;; maps pairs of cubes to a label (function maybe?) indicating the type of coordinate
;;   ;; transformation
;;   (list '(1 2) 'horizontal
;;         (cons '(1 3) 'vertical)
;;         (cons '(

;; how all 4 sides of each face relate to another face, with a certain rotation given. Rotation:
;; 0 = plain, 1 = 90 anticlockwise, 2 upside-down, 3 270 anticlockwise

(defun cube-face-relation (f)
  (cdr (assoc f
              '((1 . (:r (2 . 0)
                      :d (3 . 0)
                      :l (4 . 2)
                      :u (6 . 1)))
                (2 . (:r (5 . 2)
                      :d (3 . 1)
                      :l (1 . 0)
                      :u (6 . 0)))
                (3 . (:r (2 . 3)
                      :d (5 . 0)
                      :l (4 . 3)
                      :u (1 . 0)))
                (4 . (:r (5 . 0)
                      :d (6 . 0)
                      :l (1 . 2)
                      :u (3 . 1)))
                (5 . (:r (2 . 2)
                      :d (6 . 1)
                      :l (4 . 0)
                      :u (3 . 0)))
                (6 . (:r (5 . 3)
                      :d (2 . 0)
                      :l (1 . 3)
                      :u (4 . 0)))))))

(defun direction-legend (d)
  (cdr (assoc d '((0 . :r)
                  (1 . :d)
                  (2 . :l)
                  (3 . :u)))))

(defun neighboring-face-and-rotation (f d)
  "f = face integer
   d = direction in which to roll over (integer again)
   Returns (next-face . num-rotations)"
  (getf (cube-face-relation f) (direction-legend d)))

(defun transform-point-to-rotated-face (i j N times)
  "times = multiple of 90

   Rotation matrix won't work because we want to rotate around center.

   Straightforward on paper though.

   Rotating the coordinate axes 90 anti-clockwise (= rotating the face of the cube while the
   point stays the same), yields:

   x' = N - y
   y' = x

   or i-new = j
      j-new = N - 1 - i
  "
  (let ((i-new i)
        (j-new j))
    (loop repeat times
          for i-temp = i-new
          for j-temp = j-new
          do (setq i-new j-temp
                   j-new (- N 1 i-temp)))
    (cons i-new j-new)))

(defun transform-direction-to-rotated-face (d times)
  (mod (+ d times) 4)) 

(defun step-on-cube (f i j d N)
  "Return coordinates after making one step in the given direction.

     (works with cube coordinates (f i j), with f referring to the current face. For every face,
     the up direction (i = 0) is the same as in the 2d map. So every time we step over the
     border of a face, we rotate i j, and update f."
  (let* ((horizontal (or (= d 0) (= d 2)))
         (delta (if (< d 2) 1 -1))
         (next-i (if horizontal i (+ i delta)))
         (next-j (if horizontal (+ j delta) j))
         (next-f f)
         (next-d d)
         (change-f (or (< next-i 0)
                       (>= next-i N)
                       (< next-j 0)
                       (>= next-j N))))
    (when change-f
      ;; we change face --> figure out which face is next and what transformation to apply
      (let* ((face-and-rot (neighboring-face-and-rotation f d)))
        ;; change face and orientation
        (setq next-f (car face-and-rot)
              next-d (transform-direction-to-rotated-face d (cdr face-and-rot)))
        ;; roll over the coordinates
        (setq next-i (mod next-i N)
              next-j (mod next-j N))
        (print (list "before rot" (list next-i next-j)))
        (let ((rot-ij (transform-point-to-rotated-face next-i next-j N (cdr face-and-rot))))
          (setq next-i (car rot-ij)
                next-j (cdr rot-ij)))))

    (list next-f next-i next-j next-d)))

(defun cube-to-2d (f i j N)
  (let ((offset (face-offset f)))
    (cons (+ i (* N (car offset)))
          (+ j (* N (cdr offset))))))

(defun do-cube-move (a f i j N direction distance)
  "Step one by one, along a direction, checking for obstructions by converting to 2d coords at
   every step"
  (loop for counter from 0 below distance
        with f-cur = f
        with i-cur = i
        with j-cur = j
        with d-cur = direction
        for fijd-next = (step-on-cube f-cur i-cur j-cur d-cur N)
        for f-next = (first fijd-next)
        for i-next = (second fijd-next)
        for j-next = (third fijd-next)
        for d-next = (fourth fijd-next)
        for ij-next-2d = (cube-to-2d f-next i-next j-next N)
        do (print (list "moving-fijd" fijd-next ij-next-2d))
          
        ;; check for obstruction on the 2d map
        if (char= (aref a (car ij-next-2d) (cdr ij-next-2d)) #\#)
          return (list f-cur i-cur j-cur d-cur)
        else
          do (setq f-cur f-next
                   i-cur i-next
                   j-cur j-next
                   d-cur d-next)
        finally (return (list f-cur i-cur j-cur d-cur))))

(defun do-part2 ()
  (let* ((f 1)
         (i 0)
         (j 0)
         (d 0)
         (fijd (list f i j d))
         (N 50)
         (a (parse-map lines))
         (instructions (parse-instructions lines)))
    ;; start at f = 1, i = 0, j = 0 (top left corner of face 1, maps to 0, 50 on the 2D map)
    ;; check if the conversion works fine
    (print (cube-to-2d f i j N))

    ;; do the first move
    (print fijd)
    (setq fijd (do-cube-move a (first fijd) (second fijd) (third fijd) N (fourth fijd) (first (getf instructions :moves))))
    (print fijd)

    ;; then (turn, move), (turn, move), ...
    (loop for turn in (getf instructions :turns)
          for move in (cdr (getf instructions :moves))
          do (setq fijd (do-cube-move a
                          (first fijd) (second fijd) (third fijd) N (do-turn (fourth fijd) turn)
                          move))
          do (print (list turn move fijd)))

    ;; convert back to 2d, and get final score
    (let ((ij-2d (cube-to-2d (first fijd) (second fijd) (third fijd) N)))
      (+ (* 1000 (1+ (car ij-2d)))
         (* 4 (1+ (cdr ij-2d)))
         d))))

