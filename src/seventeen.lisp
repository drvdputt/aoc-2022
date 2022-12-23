(load "file.lisp")
(defparameter gas (car (read-default-input "seventeen")))
(defparameter gas-e ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(defparameter the-gas gas)
;; (setq the-gas gas-e)

;; width of the chamber
(defparameter width 7)

;; lowest and highest possible x
(defparameter x-lo 0)
(defparameter x-hi (1- width))

(defun xy/xs (points)
  (mapcar (function car) points))

(defun xy/ys (points)
  (mapcar (function cdr) points))

(defun xy/width (points)
  (let ((xs (xy/xs points)))
    (+ 1 (- (apply (function max) xs) (apply (function min) xs)))))

(defun xy/height (points)
  (let ((ys (xy/ys points)))
    (+ 1 (- (apply (function max) ys) (apply (function min) ys)))))

(defun xy/shift (points dx dy)
  (mapcar (lambda (p)
            (cons (+ dx (car p)) (+ dy (cdr p))))
          points))

(load "num.lisp")
(defun xy/pprint (points)
  (let* ((x0 (apply (function min) (xy/xs points)))
         (y0 (apply (function min) (xy/ys points)))
         (w (xy/width points))
         (h (xy/height points))
         (a (make-array (list h w) :element-type 'character :initial-element #\.)))
    (loop for x in (xy/xs points)
          for y in (xy/ys points)
          do (setf (aref a
                         ;; flip the y direction
                         (- (- h 1) (- y y0))
                         ;; keep the x direction
                         (- x x0)) #\#))
    (pprint-char-array a)))
    
;; each rock is a list of coordinates, relative to its bottom left point (since the left and
;; bottom edge are most important for the spawning)
;; points is list of x . y conses
;; precalculates some stuff too
(defun init-rock (points)
  (list :xy points
        :w (xy/width points)
        :h (xy/height points)))

(defparameter rock-types
  (make-array
   5
   :element-type 'list
   :initial-contents
   (mapcar (function init-rock)
           (list
            ;;----
            '((0 . 0) (1 . 0) (2 . 0) (3 . 0))
            ;; |
            ;;-+-
            ;; |
            '((1 . 2)
              (0 . 1) (1 . 1) (2 . 1)
              (1 . 0))
            ;;  |
            ;;  |
            ;;__|
            '((2 . 2)
              (2 . 1)
              (0 . 0) (1 . 0) (2 . 0))
            ;;|
            ;;|
            ;;|
            ;;|
            '((0 . 3)
              (0 . 2)
              (0 . 1)
              (0 . 0))
            ;;++
            ;;++
            '((0 . 1) (1 . 1)
              (0 . 0) (1 . 0))))))
   
(defparameter state-ini
  (list
   :rock-count 0
   ;; refers to an entry of the rock type array
   :rock-index 0
   ;; rocks spawn at x = 2 and y = highest-occupied + 4 (with y = 0 being the lowest most
   ;; position a rock can take, and y = -1 the position of the floor)
   :rock-x 2
   :rock-y 3
   ;; list of all coordinates occupied by rock pieces. Starts with just the floor at -1.
   :occupied-xy (loop for x from 0 to x-hi collect (cons x -1))
   ;; highest rock, to determine spawn point. Updated every time a rock stops.
   :occupied-ymax -1
   ;; refers to an entry in the gas pattern
   :gas-index 0))

(defun state/rock-type (state)
  (elt rock-types (getf state :rock-index)))

(defun state/rock-points (state)
  (let ((rock-layout (getf (state/rock-type state) :xy))
        (dx (getf state :rock-x))
        (dy (getf state :rock-y)))
    (xy/shift rock-layout dx dy)))

;; evolve the state by one step = one wind move + one rock fall. Returns t if a rock has
;; stopped, nil if it is still falling.
(defun state/step (state &key (print-after nil) (discard-threshold nil))
  (let ((rock-stopped nil))
    ;; apply wind move and advance wind pointer by one
    (let ((wind-char (elt the-gas (getf state :gas-index))))
      ;; (print wind-char)
      (cond
        ;; if wind goes left and rock can move left (TODO check for obstruction by other rocks)
        ((and (char= wind-char #\<)
              ;; check left wall
              (> (getf state :rock-x) x-lo)
              ;; check left collision
              (not (state/move-will-cause-collision-p state -1 0)))
         (decf (getf state :rock-x)))
        ;; if wind goes right and rock can move right
        ((and (char= wind-char #\>)
              ;; check right wall. One past rightmost block = x + w
              ;; ####
              ;; 01234
              (<= (+ (getf state :rock-x) (getf (state/rock-type state) :w)) x-hi)
              ;; check right collision
              (not (state/move-will-cause-collision-p state 1 0)))
         (incf (getf state :rock-x)))))
    ;; advance gas index (with modulo rollover)
    (setf (getf state :gas-index)
          (mod (1+ (getf state :gas-index)) (length the-gas)))

    (if (and
         ;; check floor
         (> (getf state :rock-y) 0)
         ;; check down collision
         (not (state/move-will-cause-collision-p state 0 -1)))
        ;; drop the rock by 1
        (decf (getf state :rock-y))
        ;; else, add the rock's points to the occupied points, advance the rock pointer, and
        ;; reset the rock x y trackers.
        (progn (setq rock-stopped t)
               (state/finalize-rock state)))
    ;; optional optimization
    (when (and discard-threshold
               (> (xy/height (getf state :occupied-xy)) discard-threshold))
      (state/discard-points state))
    ;; optional print
    (when print-after (state/pprint state))
    rock-stopped))
  
(defun state/finalize-rock (state)
  (let ((rock-points (state/rock-points state))
        (rock-type (state/rock-type state)))
    ;; store the rock points
    (setf (getf state :occupied-xy)
          (append rock-points (getf state :occupied-xy)))
    ;; update the highest point if one of the new rocks is higher
    (setf (getf state :occupied-ymax)
          (max (getf state :occupied-ymax) (1- (+ (getf state :rock-y)
                                                  (getf rock-type :h)))))
    ;; advance the rock index and set the spawn coordinates of the new rock (a gap of 3 needs to
    ;; exist)
    (setf (getf state :rock-index) (mod (1+ (getf state :rock-index)) (length rock-types))
          (getf state :rock-y) (+ 4 (getf state :occupied-ymax))
          (getf state :rock-x) 2)
    (incf (getf state :rock-count))))

(defun state/pprint (state)
  (terpri)
  (xy/pprint (getf state :occupied-xy))
  (terpri))

(defun state/move-will-cause-collision-p (state dx dy)
  ;; no need to check if we're still above the other blocks
  (if (> (getf state :rock-y) (1+ (getf state :occupied-ymax)))
      nil
      ;; "some of the shifted rock points are member of the occupied list"
      (some (lambda (p)
              (member p (getf state :occupied-xy) :test #'equal))
            (xy/shift (state/rock-points state) dx dy))))

;; optimization that only keeps points above a corridor "jam". Discard everything below.
(defun state/discard-points (state)
  ;; sort by y value. Bigger y should come first.
  (sort (getf state :occupied-xy) (lambda (p1 p2) (> (cdr p1) (cdr p2))))
  ;; (print "sorted")
  ;; (print (getf state :occupied-xy))
  (setf (getf state :occupied-xy)
        (subseq (getf state :occupied-xy)
                0
                (floor (length (getf state :occupied-xy)) 2))))
  ;; (print "after cut")
  ;; (print (getf state :occupied-xy)))
    
    
(defun do-puzzle ()
  (let ((nustate (copy-list state-ini)))
    (loop with counter = 0
          ;; if a rock has stopped, increase the counter
          if (state/step nustate :discard-threshold 400)
            do (incf counter)
          when (= counter 2022)
            return (getf nustate :occupied-ymax))))
          
        
(defun do-part2 (num-rocks discard-threshold)
  (let ((nustate (copy-list state-ini)))
    (loop with counter = 0
          ;; optimization needed: switch from alist to vector to find periodicity, store states
          ;; per gas cycle, while keeping rock cycle fixed. Indexed on gas-index.
          with rock0-gasi = (make-array (length the-gas) :element-type 'list :initial-element nil)
          ;; also store the second occurrence, to check for consistency. If delta h between
          ;; current and second, and second and first are the same, then print out what we
          ;; found.
          with second-rock0-gasi = (make-array (length the-gas) :element-type 'list :initial-element nil)
          ;; if a rock has stopped, check a bunch of things
          if (state/step nustate :discard-threshold discard-threshold :print-after nil)
            do (incf counter)
            and do (when (= (getf nustate :rock-index) 0)
                     ;; IDEA: look for periodicity. Find if there's any number of rocks after which the original
                     ;; configuration is recovered, specifically:
                     ;; - flat floor at y
                     ;; - rock type index at 0
                     ;; - rock position at (2 . y + 4) (always true when a new rock spawns)
                     ;; - wind index at 0
                     ;; after having a rock cycle, store the state at a certain gas index. Once
                     ;; we re-encounter this gas index,
                     ;; (print rock0-gasi-alist)
                     (let* ((gas-i (getf nustate :gas-index))
                            (entry (elt rock0-gasi gas-i))
                            (second-entry (elt second-rock0-gasi gas-i)))

                       ;; add to list if not there
                       (when (not entry)
                         (setf (elt rock0-gasi gas-i) (copy-list nustate)))

                       ;; add to second list if already in first list but not second
                       (when (and (not second-entry) entry)
                         (setf (elt second-rock0-gasi gas-i) (copy-list nustate)))

                       ;; when we have two recorded entries, check consistency
                       (when (and entry second-entry)
                         (let* ((first-state entry)
                                (second-state second-entry)
                                (delta-r1 (- (getf nustate :rock-count) (getf second-state :rock-count)))
                                (delta-y1 (- (getf nustate :occupied-ymax) (getf second-state :occupied-ymax)))
                                (delta-r2 (- (getf second-state :rock-count) (getf first-state :rock-count)))
                                (delta-y2 (- (getf second-state :occupied-ymax) (getf first-state :occupied-ymax))))
                           ;; check delta r and delta y consistency
                           (when (and (= delta-r1 delta-r2) (= delta-y1 delta-y2))
                             (print (concatenate 'string
                                                 "periodicity found! after rock nr"
                                                 (write-to-string counter)))
                             (print "state at start of period")
                             (print first-state)
                             (state/pprint first-state)
                             (print "second state")
                             (print second-state)
                             (state/pprint second-state)
                             (print "current state")
                             (print nustate)
                             (state/pprint nustate)
                             (print (concatenate
                                     'string
                                     "rock start . delta rock . delta H"
                                     (write-to-string
                                      (list
                                       (getf first-state :rock-count)
                                       delta-r1
                                       delta-y1)))))))))
            and when (= counter num-rocks)
                  return (getf nustate :occupied-ymax))))

(defparameter n 1000000000000)
;; instructions: run (do-part2), and note down the offset, delta and deltaH from the output.
;; for example, this works
(defparameter offset 15)
(defparameter delta 35)
(defparameter delta-h 53)
;; for big input, try this 105 1715 2616
(setq offset 105
      delta 1715
      delta-h 2616)

(let*
    ;; the number of rocks to simulate once the period has started
    ((num-periods (floor (- n offset) delta))
     (remainder (mod (- n offset) delta))
     (numrocks-non-periodic (+ offset remainder))
     (height-periodic (* num-periods delta-h))
     (height-non-periodic (do-part2 numrocks-non-periodic (* delta-h 4))))
  (print (+ 1 height-periodic height-non-periodic)))

