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
  xoffset)



(defun elf-array/shrinkwrap (xys)
  "Make an elf-array instance that just fits around the list of elves"
  (let* ((xymax ;; get maximum of each column
           (aops:each-index j
             (aops:reduce-index #'max i
               (aref xys i j))))
         (xymin ;; get minimum of each column
           (aops:each-index j
             (aops:reduce-index #'min i
               (aref xys i j))))
         (dimensions (map 'list (lambda (a b) (1+ (- a b))) xymax xymin))
         (ea (make-elf-array :a (make-array dimensions :element-type 'character :initial-element #\.)
                             :xoffset (elt xymin 0)
                             :yoffset (elt xymin 1))))
    (loop for n from 0 below (array-dimension xys 1)
          do (elf-array/set ea (aref xys n 0) (aref xys n 1) #\#))
    (pprint-char-array (elf-array-a ea))
    ea))

(defun elf-array/set (ea x y v)
  (let ((x0 (- x (elf-array-xoffset ea)))
        (y0 (- y (elf-array-yoffset ea))))
    (setf (aref (elf-array-a ea) y0 x0) v)))

(defparameter directions
  '('north 'south 'west 'east))

(defstruct elf-state
  xys ;; current position of each elf
  move-xys ;; proposed next position of each elf
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
          (print (cons i j))
          (setf (aref xys n 0) j
                (aref xys n 1) i)
          (incf n)))
      (setf (elf-state-xys es) xys)
      (setf (elf-state-move-xys es) (make-array
                                     (array-dimensions xys)
                                     :element-type 'integer
                                     :initial-element 0)))
    es))
        

  
