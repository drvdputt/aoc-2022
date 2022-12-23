(load "string.lisp")

;; manhattan distance = delta x + delta y
(defun distance (x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defun sensor/parse (line)
  (let* ((numbers (find-all-int line))
        (sx (first numbers))
        (sy (second numbers))
        (bx (nth 2 numbers))
        (by (nth 3 numbers)))
    (list :sx sx :sy sy :bx bx :by by :r (distance sx sy bx by))))

(defun sensor/in-exclusion-zone (sensor x y)
  (<= (distance x y (getf sensor :sx) (getf sensor :sy))
      (getf sensor :r)))

;; excluded x-range for sensor, given a y coordinate. Nil if no points. return value is (cons
;; xmin xmax)
(defun sensor/exclusion-x (sensor y)
  (let ((ydist (abs (- y (getf sensor :sy))))
        (r (getf sensor :r)))
    (if (> ydist r)
        nil
        (let ((x (getf sensor :sx))
              (dx (- r ydist)))
          (cons (- x dx) (+ x dx))))))

;; try innefficient, simplest approach first
(defun scan-row (sensors y)
  (let ((start 0)
        (stop 0))
    ;; first, need to determine scan range
    (loop for s in sensors
          for xmin-xmax = (sensor/exclusion-x s y)
          ;; do (print (list "ran exclusion" xmin-xmax))
          if xmin-xmax
            minimize (car xmin-xmax) into xmin
            and maximize (cdr xmin-xmax) into xmax
          finally (setq start xmin stop xmax))
    ;; then do scan
    (print (list "running scan from" start "to" stop))
    (loop for x from start to stop
          if (sensors/in-any-exclusion-zone sensors x y)
            collect x)))

(load "file.lisp")
(defvar lines-e (read-lines "fifteen-example-input.txt"))
(defvar sensors-e (loop for l in lines-e collect (sensor/parse l)))

(defvar lines (read-default-input "fifteen"))

(load "list-tools.lisp")
(defun do-puzzle (lines y)
  (let* ((sensors (loop for l in lines collect (sensor/parse l)))
         (excluded-points (scan-row sensors y))
         (beacons-in-line (list-tools/unique
                           (loop for s in sensors
                                 if (= (getf s :by) y)
                                   collect (cons (getf s :bx) (getf s :by)))
                           (function equal))))
    ;; we still need to count the number of beacons on the row itself. they don't count for the
    ;; answer. Some sensors share a beacon! Collect only unique ones.
    ;; (print excluded-points)
    (print beacons-in-line)
    (- (length excluded-points) (length beacons-in-line))))

(defun sensors/in-any-exclusion-zone (sensors x y)
  (loop for s in sensors
        if (sensor/in-exclusion-zone s x y)
          return t
        finally
           (return nil)))

;; brute force is too slow...
(defun do-part2 (lines xmax ymax)
  (let* ((sensors (loop for l in lines collect (sensor/parse l)))
         (x-found 0)
         (y-found 0))
    (loop for y from 0 to ymax
          if (or (< ymax 101)
                 (= 0 (mod y (floor ymax 100))))
            do (print (list "progress" y))
          if (loop with x = 0
                   ;; no need to do x+1! We go forward according to the sensor ranges. And if
                   ;; we're out of range, then we've found our point!
                   if (loop for s in sensors
                            for zone = (sensor/exclusion-x s y)
                            ;; if in exclusion zone
                            if (and zone (>= x (car zone)) (<= x (cdr zone)))
                              ;; skip forward and stop checking sensors
                              ;; do (print (list "in exclusion zone" zone "skipping forward to" (+ 1 (cdr zone))))
                              do (setq x (+ 1 (cdr zone)))
                              and return nil ;; this will restart the loop with the updated x
                            ;; if all sensors were passed, then we have our result
                            finally (return t))
                     ;; if result was found, save it and return
                     do (setq x-found x y-found y)
                     and return t
                   ;; if result was not found (we're past the maximum), move to the next row
                   if (> x xmax)
                     return nil)
            return t)
    (print (cons x-found y-found))
    ;; "tuning frequency"
    (+ (* x-found xmax) y-found)))

(print (do-part2 lines 4000000 4000000))
