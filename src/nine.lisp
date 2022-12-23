(load "file.lisp")
(defvar lines)
(setq lines (read-default-input "nine"))
;; (setq lines (read-lines "nine-example-input.txt"))

(defun rope/default ()
  (list :xh 0 :yh 0 :xt 0 :yt 0))

;; rope: see definition above, direction: character D, U, R, or L
(defun rope/move-once (rope direction)
  (let ((newrope (copy-list rope)))
    (cond ((char= direction #\D)
	   (decf (getf newrope :yh)))
	  ((char= direction #\U)
	   (incf (getf newrope :yh)))
	  ((char= direction #\R)
	   (incf (getf newrope :xh)))
	  ((char= direction #\L)
	   (decf (getf newrope :xh))))
    (rope/update-tail newrope)))

(load "string.lisp")

;; returns all new configurations the rope went through during this move
(defun rope/apply-move-instruction (rope line)
  (let ((newrope (copy-list rope))
	(direction (char line 0))
	(n (first (find-all-int line))))
    (loop repeat n
	  do (setq newrope (rope/move-once newrope direction))
	  collect newrope)))

(defun multi-rope/apply-move-instruction (multi-rope line)
  (let ((direction (char line 0))
	(n (first (find-all-int line))))
    (loop repeat n
	  with new-multi-rope = multi-rope
	  do (setq new-multi-rope (multi-rope/move-once new-multi-rope direction))
	  collect new-multi-rope)))
  

(defun rope/update-tail (rope)
  (let* ((newrope rope)
	 (dx (- (getf rope :xh) (getf rope :xt)))
	 (dy (- (getf rope :yh) (getf rope :yt)))
	 (bigdx (>= (abs dx) 2))
	 (bigdy (>= (abs dy) 2))
	 (bigdiag (or (and bigdx (>= (abs dy) 1))
		      (and bigdy (>= (abs dx) 1)))))
    ;; all conditions where x needs to move
    (if (or bigdx bigdiag)
	(incf (getf newrope :xt) (signum dx)))
    (if (or bigdy bigdiag)
	(incf (getf newrope :yt) (signum dy)))
    newrope))

;; now we need to collect all unique positions of the tail
(defun rope/tail (rope)
  (cons (getf rope :xt) (getf rope :yt)))

(let* ((r (rope/default))
       (unique-tails (list (rope/tail r))))
  (loop for l in lines
	do (let* ((ropes (rope/apply-move-instruction r l)))
	     ;; update rope configuration to the last of the series we
	     ;; just computed
	     (setq r (car (last ropes)))
	     ;; go over the new tail positions
	     (loop for newrope in ropes
		   for tail = (rope/tail newrope)
		   if (not (member tail unique-tails :test (function equal)))
		     do (push tail unique-tails))))
  (print unique-tails)
  (print (length unique-tails)))


;; for the second part, we compose the long rope of multiple segments
;; like the above. The head of the (n+1)th segment is always equal to
;; the tail of the nth one.

(defun multi-rope/default (length)
  (loop repeat length
	collect (rope/default)))

(defun rope/match-preceding-tail (rope preceding-rope)
  (let ((newrope (copy-list rope)))
    (setf (getf newrope :xh) (getf preceding-rope :xt))
    (setf (getf newrope :yh) (getf preceding-rope :yt))
    (rope/update-tail newrope)))

(defun multi-rope/move-once (multi-rope direction)
  (loop for segment in multi-rope
	with preceding-rope = nil
	with updated-rope = nil
	;; for the head, do a regular move
	if (not preceding-rope)
	  do (setq updated-rope
		   (rope/move-once segment direction))
	;; for the rest, set the head equal to the tail of the
	;; preceding segment, then update the tail
	else
	  do (setq updated-rope
		   (rope/match-preceding-tail segment preceding-rope))
	end
	do (setq preceding-rope updated-rope)
	collect updated-rope))

(defun multi-rope/tail (multi-rope)
    (rope/tail (car (last multi-rope))))

;; with the new tail definition in place, we can use the same code to
;; collect the unique tails



(let* ((multi-rope (multi-rope/default 9))
       (unique-tails (list (multi-rope/tail multi-rope))))
  (loop for l in lines
	do (let* ((multi-rope-steps (multi-rope/apply-move-instruction multi-rope l)))
	     ;; go over the new tail positions
	     (loop for new-multi-rope in multi-rope-steps
		   for tail = (multi-rope/tail new-multi-rope)
		   if (not (member tail unique-tails :test (function equal)))
		     do (push tail unique-tails))
	     ;; update rope configuration to the last of the series we
	     ;; just computed
	     (setq multi-rope (car (last multi-rope-steps)))))
  (print unique-tails)
  (print (length unique-tails)))

