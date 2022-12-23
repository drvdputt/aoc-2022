(load "file.lisp")
(load "string.lisp")
(ql:quickload :array-operations)

(defparameter lines (read-default-input "eighteen"))
(defparameter lines-e (read-default-input "eighteen-example"))

(defun integer-array (n)
  (make-array n :element-type 'integer :initial-element 0))

;; let's go the easy route. Just make a 3d array, since the maximum input is only 19 anyway.
(defun cube/init (lines)
  (let* ((n (length lines))
         (xs (integer-array n))
         (ys (integer-array n))
         (zs (integer-array n)))
    (loop for l in lines
          for i from 0
          for xyz = (find-all-int l)
          for x = (first xyz)
          for y = (second xyz)
          for z = (third xyz)
          do (setf (elt xs i) x 
                   (elt ys i) y
                   (elt zs i) z))
    (let* ((dimensions (mapcar (lambda (i) (1+ (reduce #'max i)))
                               (list xs ys zs)))
           (a (integer-array dimensions)))
      (print dimensions)
      (loop for x across xs
            for y across ys
            for z across zs
            do (setf (aref a x y z) 1))
      a)))
 
(defun cube/surface-area (cube)
  ;; go over all cells, and if a cell is 1, check the neighboring cell. Input is small enough
  ;; for this to be fast.
  (let ((num-sides 0))
    (array-operations:nested-loop (i j k) (array-dimensions cube)
      (when (> (aref cube i j k) 0)
        (incf num-sides (- 6 (cube/num-neighbors cube i j k)))))
    num-sides))

(defun cube/num-neighbors (cube i j k)
  (let ((num-neighbors
           (count t
                  (list 
                   (and (> i 0) (> (aref cube (1- i) j k) 0))
                   (and (< i (1- (array-dimension cube 0))) (> (aref cube (1+ i) j k) 0))
                   (and (> j 0) (> (aref cube i (1- j) k) 0))
                   (and (< j (1- (array-dimension cube 1))) (> (aref cube i (1+ j) k) 0))
                   (and (> k 0) (> (aref cube i j (1- k)) 0))
                   (and (< k (1- (array-dimension cube 2))) (> (aref cube i j (1+ k)) 0))))))
    ;; (print num-neighbors)
    num-neighbors))

;; (defun cube/

;; part 2

;; idea: set up a dijkstra type walker to find all outside points. Then sum the number of
;; neighbors of the outside points. Do not need distances here can be simpler than dijkstra.
;; Just need the walker, and it can stop exploring if the neighboring nodes have already been
;; visited.
;; first set up indexing system. Map i j k to single index
(defun flat-index (i j k dimensions)
  ;; i + j * ni + k * ni * nj
  (+ i
     (* j (elt dimensions 0))
     (* k (elt dimensions 0) (elt dimensions 1))))

(defun ijk-index (flat-index dimensions)
  (let* ((ni (elt dimensions 0))
         (nj (elt dimensions 1))
         (i (mod flat-index ni))
         (j (floor (mod flat-index (* ni nj)) ni))
         (k (floor flat-index (* ni nj))))
    (list i j k)))

;; returns all i j k for empty neighbors
(defun cube/empty-neighbors (cube i j k)
  (let ((neighbors nil))
    (when (and (> i 0) (= (aref cube (1- i) j k) 0))
      (push (list (1- i) j k) neighbors))

    (when (and (< i (1- (array-dimension cube 0))) (= (aref cube (1+ i) j k) 0))
      (push (list (1+ i) j k) neighbors))

    (when (and (> j 0) (= (aref cube i (1- j) k) 0))
      (push (list i (1- j) k) neighbors))

    (when (and (< j (1- (array-dimension cube 1))) (= (aref cube i (1+ j) k) 0))
      (push (list i (1+ j) k) neighbors))

    (when (and (> k 0) (= (aref cube i j (1- k)) 0))
      (push (list i j (1- k)) neighbors))

    (when (and (< k (1- (array-dimension cube 2))) (= (aref cube i j (1+ k)) 0))
      (push (list i j (1+ k)) neighbors))
    neighbors))

(load "dijkstra.lisp")

(defun do-puzzle (lines)
  (cube/surface-area (cube/init lines)))

;; (defun do-part2 (lines)
;;   (let* ((cube (cube/init lines))
;;          (ds (array-dimensions cube))
;;          (matrix (dijkstra/distance-matrix-init (apply #'* ds))))
;;     (dijkstra/recurse
;;      matrix
;;      (flat-index 0 0 0 ds)
;;      (lambda (flat-index) 
     
     
