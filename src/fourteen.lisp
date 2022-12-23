(load "file.lisp")
(load "string.lisp")
(load "num.lisp")

;; return list of x and y coordinates of all the wall segments, from a
;; wall specification (list of the x and y of the corners)
(defun make-wall-path (xs ys)
  (let* ((prev-x (car xs))
         (prev-y (car ys))
         (path (list (cons prev-x prev-y))))
    (loop for x in (cdr xs)
          for y in (cdr ys)
          ;; do (print (list x y prev-x prev-y))
          ;; do (print (list "delta x" (- x prev-x)))
          if (not (= 0 (- x prev-x))) ;; either xdiff or ydiff, diagonal not possible
            do (loop for xi in (range prev-x x :exclude-start t)
                     ;; do (print (list "traversing x" xi))
                     do (push (cons xi y) path))
          else
            do (loop for yi in (range prev-y y :exclude-start t)
                     ;; do (print (list "traversing y" yi))
                     do (push (cons x yi) path))
          end
          do (setq prev-x x prev-y y))
    path))


(defun path/parse (line)
  ;; loop in pairs. See https://lispcookbook.github.io/cl-cookbook/iteration.html
  (loop for rest on (find-all-int line) by #'cddr
        collect (car rest) into x
        collect (car (cdr rest)) into y
        finally (return (make-wall-path x y))))

(defun paths/parse (lines)
  (loop for l in lines
        collect (path/parse l)))

;; path does not need to be contigous, just collection of conses, each one has integers (x . y)
(defun path/extent (path)
  (loop for coord in path
        for x = (car coord)
        for y = (cdr coord)
        maximize x into xmax
        minimize x into xmin
        maximize y into ymax
        minimize y into ymin
        finally (return (list
                         :xoffset xmin
                         :yoffset ymin
                         :dimensions (list (+ 1 (- ymax ymin)) (+ 1 (- xmax xmin)))))))

;; turn collection of paths and source into collection of coords
(defun paths/flatten (paths source)
  (let* ((pathcoords (apply (function append) paths))
         (allcoords (cons source pathcoords)))
    allcoords))

;; paths = list of (x . y) conses
;; source = one cons, indicating where the sand source is
(defun paths/map (paths source)
  ;; make one big path and calculate the extent of it
  (let* ((allcoords (paths/flatten paths source))
         ;; also need source position in extent calculation
         (extent (path/extent allcoords))
         ;; use the extent to initialize this array of size (ny nx)
         (map-array (make-array (getf extent :dimensions)
                                :element-type 'character
                                :initial-element #\.)))
    ;; fill in all the coordinates with #
    (loop for c in allcoords
          ;; i = y - yoffset
          for i = (- (cdr c) (getf extent :yoffset))
          ;; j = x - xoffset
          for j = (- (car c) (getf extent :xoffset))
          do (setf (aref map-array i j) #\#))
    ;; mark the source
    (setf (aref map-array
                (- (cdr source) (getf extent :yoffset))
                (- (car source) (getf extent :xoffset)))
                #\+)
    (list :map map-array
          :extent extent)))

(defun pprint-char-array (a)
  (loop for i from 0 below (array-dimension a 0)
        do (loop for j from 0 below (array-dimension a 1)
                 do (write-char (aref a i j))
                    finally (terpri))))

;; (pprint-char-array (paths/map (paths/parse lines-e) (cons 500 0)))
;; (pprint-char-array (paths-map (paths/parse lines)))

;; function that returns nil if out of bounds
;; 0 if free
;; 1 if obstructed
(defun sandmap/try-move (sandmap i j)
  (let ((nj (array-dimension sandmap 1))
        (ni (array-dimension sandmap 0)))
    ;; (print (list "trying move to" i j ni nj))
    (cond ((or (< i 0)
               (>= i ni)
               (< j 0)
               (>= j nj))
           nil)
          ((char= (aref sandmap i j) #\.)
           0)
          (t
           1))))

;; return nil if off screen, return current if settled
;; position is (x . y) (after applying offset)
(defun sandmap/next-position (sandmap position)
  (let* ((x (car position))
         (y (cdr position))
         ;; first handle y one down
         (ydown (+ 1 y)))
    ;; way to loop in pairs
    (loop for (i j) on (list ydown x
                             ydown (- x 1)
                             ydown (+ x 1))
          by #'cddr while j
          for try-result = (sandmap/try-move sandmap i j)
          ;; do (print try-result)
          ;; out of bounds
          if (not try-result)
            return nil
          ;; free -> return new position
          else if (= 0 try-result)
                 return (cons j i)
          end
          ;; everything blocked -> return current position.
          finally (return position))))

;; returns final position of sand
(defun sandmap/drop (sandmap position)
  (let ((cur-pos position))
    (loop for i from 0 below (array-dimension sandmap 0) ;; just a practical limit to the number of drops
          for new-pos = (sandmap/next-position sandmap cur-pos)
          ;; out of bounds
          if (not new-pos)
            return nil
          ;; settled
          if (equal new-pos cur-pos)
            return new-pos
          ;; moving
          else
            do (setq cur-pos new-pos))))

;; update map with new sand. Returns new sand position, which is nil if sand flows off map
;; (meaning that more steps won't do anything new)
(defun sandmap/step (sandmap source)
  (let ((final-sand-pos (sandmap/drop sandmap source)))
    (if final-sand-pos
        (setf (aref sandmap (cdr final-sand-pos) (car final-sand-pos)) #\o))
    ;; (pprint-char-array sandmap)    
    final-sand-pos))

(defun do-puzzle (lines source)
  (let* ((paths (paths/parse lines))
         (the-map (paths/map paths source))
         (sandmap (getf the-map :map))
         (extent (getf the-map :extent))
         (source-offset (cons (- (car source) (getf extent :xoffset))
                              (- (cdr source) (getf extent :yoffset)))))
    (loop for counter from 0
          ;; step and check if nil
          if (not (sandmap/step sandmap source-offset))
            return counter)))

(defvar source (cons 500 0))

(defvar lines-e (list
                 "498,4 -> 498,6 -> 496,6"
                 "503,4 -> 502,4 -> 502,9 -> 494,9"))

(defvar lines (read-default-input "fourteen"))

;; (do-puzzle lines-e source)
;; (do-puzzle lines source)

;; for part 2, build in a custom path representing the floor
(defun paths/add-floor (paths source)
  (let* ((allcoords (paths/flatten paths source))
         (ymax (loop for c in allcoords maximize (cdr c)))
         (yfloor (+ ymax 2))
         (xmin (loop for c in allcoords minimize (car c)))
         (xmax (loop for c in allcoords maximize (car c)))
         ;; set x range big enough to accomodate both triangle with width 2 * height, and the
         ;; original x range
         (xminfloor (min xmin (- (car source) yfloor)))
         (xmaxfloor (max xmax (+ (car source) yfloor)))
         ;; now, generate a big list of points representing the floor
         (floorpoints (loop for x from xminfloor to xmaxfloor
                            collect (cons x yfloor))))
  ;; add these points as an extra path
    (cons floorpoints paths)))
    
(defun do-part2 (lines source)
  (let* ((paths (paths/parse lines))
         (paths-with-floor (paths/add-floor paths source))
         (the-map (paths/map paths-with-floor source))
         (sandmap (getf the-map :map))
         (extent (getf the-map :extent))
         (source-offset (cons (- (car source) (getf extent :xoffset))
                              (- (cdr source) (getf extent :yoffset)))))
    ;; (pprint-char-array sandmap)))
    (loop for counter from 1
          for sand-pos = (sandmap/step sandmap source-offset)
          if (or (not sand-pos)
                 (equal sand-pos source-offset))
            do (pprint-char-array sandmap)
            and do (print sand-pos)
            and return counter)))


