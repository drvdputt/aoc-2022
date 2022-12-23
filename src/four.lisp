(load "file.lisp")

(defvar lines (read-lines #p"four-input.txt"))
;; (defvar lines (read-lines #p"four-example-input.txt"))

(defun is-contained (min1 max1 min2 max2)
  (or
   ;; 2 contained in 1
   (and (<= min1 min2) (<= max2 max1))
   ;; 1 contained in 2
   (and (<= min2 min1) (<= max1 max2))))

(defun is-partial-overlap (min1 max1 min2 max2)
  (not (or
   ;; all of 1 smaller than 2
   (< max1 min2)
   ;; all of 1 bigger than 2
   (< max2 min1))))

(load "num.lisp")

(load "string.lisp")
(defun elves-have-complete-overlap (line)
  (let ((result (apply (function is-contained) (find-all-int line))))
    result))

(defun elves-have-partial-overlap (line)
  (apply (function is-partial-overlap) (find-all-int line)))

(print (count T (mapcar
		 (function elves-have-complete-overlap) lines)))

(print (count-predicate lines (function elves-have-partial-overlap)))
