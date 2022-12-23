#!/usr/bin/sbcl --script
(load "file.lisp")
(defvar lines (read-lines  #p"two-input.txt"))

(defun score (line hands-function)
  (let ((hands (funcall hands-function line)))
    ;; remember that hands is (opponent me)
    (+ (choice-score (nth 1 hands))
       (apply (function win-score) hands))))
	  
(defun choice-score (hand)
  (cond ((char= hand #\A) 1)
	((char= hand #\B) 2)
	((char= hand #\C) 3)))

(defun win-score (opponent me)
  (cond ((draw opponent me) 3)
	((win opponent me) 6)
	(t 0)))

(defun draw (opponent me)
  (char= opponent me))
  
(defun win (opponent me)
  (or (and (char= opponent #\A) (char= me #\B))
      (and (char= opponent #\B) (char= me #\C))
      (and (char= opponent #\C) (char= me #\A))))

;; return a pair of ABC
(defun original-rps-hands (line)
  (let ((opponent (elt line 0))
	(me (elt line 2)))
    (list opponent (cond ((char= me #\X) #\A)
			 ((char= me #\Y) #\B)
			 ((char= me #\Z) #\C)))))

(defun draw-hand (opponent)
  (identity opponent))

(defun win-hand (opponent)
  (cond ((char= opponent #\A) #\B)
	((char= opponent #\B) #\C)
        ((char= opponent #\C) #\A)))

(defun lose-hand (opponent)
  (cond ((char= opponent #\A) #\C)
	((char= opponent #\B) #\A)
        ((char= opponent #\C) #\B)))


;; part two of the problem, with strategy that depends on other hand
(defun updated-rps-hands (line)
  (let ((opponent (elt line 0))
	(me (elt line 2)))
    (list opponent
	  (cond ((char= me #\X) (lose-hand opponent))
		((char= me #\Y) (draw-hand opponent))
		((char= me #\Z) (win-hand opponent))))))
  	  
(print (loop for l in lines
	     sum (score l (function original-rps-hands))))

(print (loop for l in lines
	     sum (score l (function updated-rps-hands))))

;; (print (loop for l in lines
	     ;; sum (score l)))
