#!/usr/bin/sbcl --script
(load "file.lisp")
(defvar input (read-lines  #p"one-input.txt"))
;; (print input)
;; 

;; (print (cl-ppcre:split "0" input))
(defvar sums (list))
(defvar indices (list))

(let ((currentsum 0)
      (elf 1))
      (loop for x in input
	    when (string= x "")
	    collect currentsum into maxes
	    and collect elf into elves
	    and do (setq currentsum 0 elf (+ 1 elf))
	    else
	    do (setq currentsum (+ currentsum (parse-integer x)))
	    finally (setq sums maxes indices elves)))

;; sort in reverse order
(defvar sortedmaxes (sort sums #'>))
;; print first three
(print (subseq sortedmaxes 0 3))
(print (apply (function +) (subseq sortedmaxes 0 3)))
