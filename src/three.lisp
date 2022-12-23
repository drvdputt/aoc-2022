#!/usr/bin/sbcl --script
(load "file.lisp")
(load "string.lisp")

(defvar lines (read-lines  #p"three-example-input.txt"))
(defvar halves-example (string-halves (nth 0 lines)))
(print (nth 0 halves-example))
(print (nth 1 halves-example))

(print (apply (function common-character) (string-halves (nth 0 lines))))

(defun priority (c)
  (let ((code (char-code c)))
    (cond
      ;; A should map to 27. char code of A is 65
      ((and (<= 65 code) (< code 97))
       (+ (- code 64) 26))
      ;; a should map to 1. char code of a is 97.
      ((<= 97 code)
       (- code 96)))))

(defun rucksack-common-item (line)
  (apply (function common-character) (string-halves line)))

(defun three-elves-common-item (line1 line2 line3)
  (common-character-multi (list line1 line2 line3)))

(print (priority #\A))
(print (priority #\z))

(print (loop for l in (read-lines  #p"three-input.txt")
	     ;; do (print (list l (string-halves l) (rucksack-common-item l)))
	     sum (priority (rucksack-common-item l))))

(print (common-character-multi (subseq lines 0 3)))

(defvar all-lines (read-lines #p"three-input.txt"))
(print (loop for (line1 line2 line3) on all-lines
	     for i from 0 to (length all-lines)
	     when (= 0 (mod i 3))
	     ;; collect (three-elves-common-item line1 line2 line3)))
	     sum (priority (three-elves-common-item line1 line2 line3))))


