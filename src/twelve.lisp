(defun char-math (integer-operation c1 c2)
  (funcall integer-operation
	   (char-int c1) (char-int c2)))

(defun char- (c1 c2)
  (char-math (function -) c1 c2))


(defun array/find-value (a char)
  (loop for i from 0 below (array-dimension a 0)
	for jfound = (loop for j from 0 below (array-dimension a 1)
			   for c = (aref a i j)
			   if (char= c char)
			     return j
			   finally (return nil))
	if jfound
	  return (list i jfound)
	finally (return nil)))

;; first is i (col), second is j (row)
(defun find-start (hmap)
  (array/find-value hmap #\S))

(defun find-end (hmap)
  (array/find-value hmap #\E))

(defun opposite-step (dir)
  (cond ((char= dir #\u) #\d)
	((char= dir #\d) #\u)
	((char= dir #\l) #\r)
	((char= dir #\r) #\l)))

(defun ij-step (i j dir)
  (cond ((char= #\u dir) (cons (- i 1) j))
	((char= #\d dir) (cons (+ i 1) j))
	((char= #\l dir) (cons i (- j 1)))
	((char= #\r dir) (cons i (+ j 1)))))

(defun hmap/peek (hmap i j dir)
  (let* ((newij (ij-step i j dir))
	 (newi (car newij))
	 (newj (cdr newij)))
    (if (or (< newi 0) (>= newi (array-dimension hmap 0))
	    (< newj 0) (>= newj (array-dimension hmap 1)))
	nil
	(aref hmap newi newj))))

(defun hmap/step-possible (hmap i j step)
  (let ((peek (hmap/peek hmap i j step))
	(c (aref hmap i j)))
    (and
     ;; destination can not be nil
     peek
     ;; height increase at most 1
     (<= (char- peek c) 1))))

(defun hmap/step-possible-reverse (hmap i j step)
  (let ((peek (hmap/peek hmap i j step))
	(c (aref hmap i j)))
    (and
     ;; destination can not be nil
     peek
     ;; height decrease at most 1
     (>= (char- peek c) -1))))

;; find all possible steps (not backwards)
;; returns list with up to three of UDLR
;; prev-step can also be nil
(defun hmap/step-options (hmap i j prev-step step-possible-f)
  (let ((bad-dir (if prev-step
		     (opposite-step prev-step)
		     nil)))
    (loop for dir in '(#\d #\r #\u #\l)
	  ;; we cannot do anything with the direction we came from
	  if (and
	      ;; direction needs to be allowed
	      (or (not bad-dir) (not (char= dir bad-dir)))
	      ;; and possible in terms of height
	      (funcall step-possible-f hmap i j dir))
	    collect dir)))

;; recursive explore until end is found
;; i j is starting position

(defun dmap/set-neighbour-distance (dmap i j ni nj)
  (let* ((olddist (aref dmap ni nj))
	 (newdist (+ (aref dmap i j) 1)))
    (if (= olddist -1)
	(setf (aref dmap ni nj) newdist)
	;; if old distance was already set, check if we need to shrink it
	(setf (aref dmap ni nj) (min newdist olddist)))))

;; if the d(neighbor) is unset (-1), or bigger than d(ij) + 1, we need to go
;; down that path
(defun dmap/needs-explore-p (dmap i j ni nj)
  (let* ((nd (aref dmap ni nj)))
    (or (= -1 nd)
	(> nd (+ 1 (aref dmap i j))))))

;; to be used recursively. Initiation of recursion uses #\r as initial
;; step (since entry point is always on the left edge)
(defun hmap/fill-dmap-entrypoint (hmap dmap i j
				  &key (step-possible-f (function hmap/step-possible)))
  (setf (aref dmap i j) 0)
  (hmap/fill-dmap-neighbors hmap dmap i j
			    :prev-dir nil
			    :step-possible-f step-possible-f))

(defun hmap/fill-dmap-neighbors (hmap dmap i j
				 &key
				   (prev-dir nil)
				   (step-possible-f (function hmap/step-possible)))
  (let ((dirs (hmap/step-options hmap i j prev-dir step-possible-f)))
    ;; first fill the map values, and remember which ones need to be explored
    (let ((dirs-to-explore
	    (loop for dir in dirs
		  for ij-neighbor = (ij-step i j dir)
		  for ni = (car ij-neighbor)
		  for nj = (cdr ij-neighbor)
		  if (dmap/needs-explore-p dmap i j ni nj)
		    collect dir
		  ;; needs to happen after the above check
		  do (dmap/set-neighbour-distance dmap i j ni nj))))
      ;; then recursively explore the directions that need exploring
      (loop for dir in dirs-to-explore
	    for ij-neighbor = (ij-step i j dir)
	    for ni = (car ij-neighbor)
	    for nj = (cdr ij-neighbor)
	    do (hmap/fill-dmap-neighbors hmap dmap ni nj
					 :prev-dir dir
					 :step-possible-f step-possible-f)))
    dmap))

(load "file.lisp")
(defvar hmap-big (read-array "twelve-input.txt" "chars"))
(defvar start-big (find-start hmap-big))
(defvar end-big (find-end hmap-big))

(defvar hmap-e (read-array "twelve-input-example.txt" "chars"))
(defvar start-e (find-start hmap-e))
(defvar end-e (find-end hmap-e))

(defun make-array-same-dim (a)
  (make-array (array-dimensions a) :initial-element -1 :element-type 'integer))

(defun do-puzzle (hmap istart jstart iend jend)
  (let ((dmap (make-array-same-dim hmap)))
    (setf (aref hmap istart jstart) #\a)
    (setf (aref hmap iend jend) #\z)
    (hmap/fill-dmap-entrypoint hmap dmap istart jstart)
    (print dmap)
    (print (aref dmap iend jend))
    dmap))

;; (defun print-progress (hmap dmap)
;;   0)
;; (defvar dmap-big (do-puzzle hmap-big
;; 		   (first start-big) (second start-big)
;; 		   (first end-big) (second end-big)))

;; part two
;; d(new, end) = d(start, end) - d(start, new)
;; Does not work if tile is not ON the shortest path!
;; (defun dmap/distance-to-end (dmap new-start end)
;;   (-
;;    ;; distance from start to end
;;    (aref dmap (first end) (second end))
;;    ;; distance from start to new point
;;    (aref dmap (first new-start) (second new-start))))

;; alternative way, set up a distance map with a different starting point
;; (defvar dmap-from-end (do-puzzle hmap-big
;; 			(first end-big) (second end-big)
;; 			(first start-big) (second start-big)))

;; go over all characters in the map. If character is at height a, check distance and remember the minimum.
(defun hmap/find-closest-a (hmap dmap)
  (loop for i from 0 below (array-dimension hmap 0)
	minimize (loop for j from 0 below (array-dimension hmap 1)
		       for d = (aref dmap i j)
		       if (and (char= (aref hmap i j) #\a) (not (= -1 d)))
			 minimize d)))

(defun do-part2 (hmap istart jstart iend jend)
  (let ((dmap (make-array-same-dim hmap)))
    (setf (aref hmap istart jstart) #\a)
    (setf (aref hmap iend jend) #\z)
    (hmap/fill-dmap-entrypoint hmap dmap iend jend
			       :step-possible-f (function hmap/step-possible-reverse))
    (print dmap)
    (print (hmap/find-closest-a hmap dmap))))

;; (do-part2 hmap-e (first start-e) (second start-e) (first end-e) (second end-e))
(do-part2 hmap-big (first start-big) (second start-big) (first end-big) (second end-big))
