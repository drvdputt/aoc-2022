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
  (it has a different unfolding layout than the example..."
  (let ((ifloor (floor i N))
        (jfloor (floor j N)))
    (cond (and (ifloor 
  
