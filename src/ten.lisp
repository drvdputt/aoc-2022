(load "file.lisp")
(defvar lines (read-default-input "ten"))

(defun cpu-state/default ()
  (list :cycle 1 :x 1))

(defun cpu-state/noop (cpu-state)
  (let ((new-state (copy-list cpu-state)))
    (incf (getf new-state :cycle))
    new-state))

(defun cpu-state/addx (cpu-state i)
  (let ((new-state (copy-list cpu-state)))
    (incf (getf new-state :cycle) 2)
    (incf (getf new-state :x) i)
    new-state))

(load "string.lisp")
(defun cpu-state/instruction (cpu-state line)
  (if (starts-with line "addx")
      (cpu-state/addx cpu-state (parse-integer (subseq line 5)))
      (cpu-state/noop cpu-state)))

;; will only work for when distance between the elements of at-cycles is large enough...
(defun find-signal-strengths (at-cycles lines)
  (let ((cpu-state (cpu-state/default))
	(previous-state (cpu-state/default))
	(probe-cycles at-cycles))
    (loop for l in lines
	  do (print l)
	  do (setq cpu-state (cpu-state/instruction cpu-state l))
	  ;; get the signal strength of the previous cycles, once we
	     ;; exceed one of the requested ones caution: the thing
	     ;; below works because the steps of 20 and 40 are big
	     ;; enough. if the probe cycles are closely spaced, it
	     ;; won't work anymore, because the cycles can jump by 2
	     ;; and the probe index only by 1
	  if (> (getf cpu-state :cycle) (car probe-cycles))
	    do (print (list (getf previous-state :x) (car probe-cycles)))
	    and collect (* (getf previous-state :x) (car probe-cycles)) into signal-strengths
	    and do (setq probe-cycles (cdr probe-cycles))
	  ;; this means we don't need to go any further
	  when (not probe-cycles)
	    return signal-strengths
	  do (setq previous-state cpu-state)
	  finally (return signal-strengths))))

(defun all-cpu-states (lines)
    (cons (cpu-state/default)
	  (loop for l in lines
		with cpu-state = (cpu-state/default)
		do (setq cpu-state (cpu-state/instruction cpu-state l))
		collect cpu-state)))

(defun pixel-value (pixel cpu-state)
  (if (<= (abs (- (mod pixel 40) (getf cpu-state :x))) 1)
      #\#
      #\.))

(defun compute-pixels (states)
  ;; pixels are numbered from 0 to 239, cycles from 1 to 240
  (loop for pixel from 0 to 239
	for cycle = (+ pixel 1)
	with rest-of-states = states
       ;; if our cycle goes past that of the next state, move to that
       ;; state. Asserts that we have enough states to do this (will
       ;; crash otherwise because cdr returns nil)
	if (>= cycle (getf (second rest-of-states) :cycle))
	  do (setq rest-of-states (cdr rest-of-states))
	;; collect the pixel values. If pixel i is within 1 distance
	     ;; of sprite x, the pixel turns on
	do (print (list pixel
			(car rest-of-states)
			(pixel-value pixel (car rest-of-states))))
	collect (pixel-value pixel (car rest-of-states))))

(defun draw-image (lines)
  (let* ((pixels (compute-pixels (all-cpu-states lines))))
    (print pixels)
    (terpri)
    (loop for p in pixels
	  for i from 0
	  do (write-char p)
	  if (= (mod i 40) 39)
	     do (terpri))))


(defvar example-lines (read-lines "ten-example-input.txt"))
(print (apply (function +) (find-signal-strengths at-cycles lines)))
						  
