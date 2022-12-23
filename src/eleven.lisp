(defvar all-divisors-used nil)

(load "file.lisp")
(defvar lines nil)
(setq lines (read-default-input "eleven"))

(defun monkey/init (items ;; list of int
		    operation ;; function(old) -> new
		    divisor ;; int for divisible test
		    next-true ;; index of next monkey if True
		    next-false) ;; index of next monkey if False
  (list :items items
	:operation operation
	:divisor divisor
	:next-true next-true
	:next-false next-false))

;; tip: I can test this using (funcall (monkey/parse-operation "
;; Operation: new = old + 2") 10) etc.
(defun monkey/parse-operation
 (operation-line)
  ;; second line has three forms
  ;; Operation: new = old + 8
  ;; Operation: new = old * 3
  ;; Operation: new = old * old
  ;; character 23 is either + or *
  ;; then there's a number or "old"
  (let ((operator (if (char= (char operation-line 23) #\+)
		      (function +)
		      (function *)))
	(second-operand (subseq operation-line 25)))
    (if (string= second-operand "old")
     ;; if this is "old" return square function
     (lambda (old) (* old old))
     ;; else we need to parse the number
     (let ((number (parse-integer second-operand)))
       (lambda (old) (funcall operator old number))))))


(load "string.lisp")
(defun monkey/parse (lines) ;; 5 lines from the init file
  (monkey/init
   ;; first line contains items
   (find-all-int (first lines))
   ;; second line is one of these functions
   (monkey/parse-operation (nth 1 lines))
   ;; third line contains the divisor at character 21
   (parse-integer (subseq (nth 2 lines) 21))
   ;; fourth line contains next monkey at character 29
   (parse-integer (subseq (nth 3 lines) 29))
   ;; fifth lines contains next monkey at character 30
   (parse-integer (subseq (nth 4 lines) 30))))

;; n is number of monkeys to parse from lines
;; 7 lines per monkey
;; data is at lines 1 - 5
(defun monkeys/parse-all (lines n)
  (loop for i from 0 to n
	for offset = (* 7 i)
	collect (monkey/parse (subseq lines (+ offset 1) (+ offset 6)))))

;; process all items held by monkey. Deletes items from monkey, and
;; returns thrown items as conses (value target). hopefully edits in
;; place? If not figure out a return mechanism.

;; returns resulting (cons value target)
(defun monkey/process-item (monkey item)
  (let* ((item-inspected ;; apply function
	   (funcall (getf monkey :operation) item))
	 (item-final ;; divide by 3 (floor to get integer, not ratio)
	   (floor (/ item-inspected 3)))
	 (target ;; divisibility test
	   (if (= (mod item-final (getf monkey :divisor)) 0)
	       (getf monkey :next-true)
	       (getf monkey :next-false))))
    (cons item-final target)))

;; same but doesn't divide by 3 this causes the number to inflate way
;; to big BUT all the divisors in the puzzle are prime numbers! What
;; we need, is a way to reduce the size of the number, while keeping
;; its divisibility properties. I'm thinking either doing some kind of
;; modulus operation, or something with prime factors. (just leave out
;; all prime factors that are not in the list of divisors. Maybe they
;; don't matter)

;; in fact the multipliers and divisers in the problem are (almost?)
;; consecutive primes
(defun monkey/process-item-alt (monkey item)
  (let* ((item-final ;; apply function
	   (mod (funcall (getf monkey :operation) item)
		all-divisors-used))
	 ;; don't divide by 3 anymore
	 (target ;; divisibility test
	   (if (= (mod item-final (getf monkey :divisor)) 0)
	       (getf monkey :next-true)
	       (getf monkey :next-false))))
    (cons item-final target)))

;; this variable chooses which processing function we are using (one
;; for part 1 the other for part 2)
;; (defvar monkey-process (function monkey/process-item))
(defvar monkey-process (function monkey/process-item-alt))

(defun monkey/do-turn (monkey)
  (let ((thrown-items
	  (loop for item in (getf monkey :items)
		;; do (print (list "processing" item))
		collect (funcall monkey-process monkey item))))
    (setf (getf monkey :items) (list))
    thrown-items))

;; takes list of monkey structs and index of monkey doing the turn.
;; Edits everything in-place
;; returns list of thrown items too, for later reference
(defun monkeys/do-turn (monkeys i)
  (let ((thrown-items (monkey/do-turn (nth i monkeys))))
    ;; distribute the thrown items over the other monkeys
    (loop for thrown-item in thrown-items
	  for value = (car thrown-item)
	  for target = (cdr thrown-item)
	  ;; do (print (list "monkey" i "trows at" target))
	  do (setf (getf (nth target monkeys) :items)
		   (append (getf (nth target monkeys) :items)
			   (list value))))
    thrown-items))
    

;; do round and count how many items were thrown by each monkey
(defun monkeys/do-round (monkeys)
  (loop for i from 0 below (length monkeys)
	collect (length (monkeys/do-turn monkeys i))))

(defun monkeys/count-activity-for-rounds (monkeys num-rounds)
  (let* ((monkey-counters (make-list (length monkeys) :initial-element 0))
	 (print-frequency (/ num-rounds 40)))
    (loop for n from 0 below num-rounds
	  if (= (mod n print-frequency) 0)
	    do (print (list "round" n))
	  ;; do round and sum up number of items for each monkey
	  do (let ((round-counts (monkeys/do-round monkeys)))
	       ;; (print round-counts)
	       (map-into monkey-counters
			 (function +)
			 monkey-counters
			 round-counts)))
    monkey-counters))

;; need multiple test monkeys to test this

;; (defvar test-monkey (monkey/parse
;;  (list
;;   "  Starting items: 79, 98"
;;   "  Operation: new = old * 19"
;;   "  Test: divisible by 23"
;;   "    If true: throw to monkey 2"
;;   "    If false: throw to monkey 3")))

;; (print (list "before turn" test-monkey))
;; (print (monkey/do-turn test-monkey))
;; (print (list "after turn" test-monkey))

;; EXAMPLE TEST HERE
(defvar example-lines (read-lines "eleven-example-input.txt"))
(defvar example-monkeys nil)
(setq example-monkeys (monkeys/parse-all example-lines 3))

;; round 1 test
;; (monkeys/do-round example-monkeys)
;; (print (monkeys/count-activity-for-rounds example-monkeys 20))
;; works now

;; 10000 test for example monkeys
(setq all-divisors-used  (* 23 19 13 17))
(let* ((ms (monkeys/parse-all example-lines 3))
       (result (sort (monkeys/count-activity-for-rounds ms 10000)
		     (function >))))
  (print (list result (* (first result) (second result)))))

;; TAKING THE MODULUS ACTUALLY WORKED! modulus using product of all divisors

;; FINAL CODE HERE
;; (let* ((test-monkeys (monkeys/parse-all lines 7))
;;        (result (monkeys/count-activity-for-rounds test-monkeys 20))
;;        (srtresult (sort (copy-list result) (function >))))
;;   (print (list result srtresult (* (first srtresult) (second srtresult)))))

;; 10000 run
(setq all-divisors-used (* 13 3 7 2 19 5 11 17))
(let* ((test-monkeys (monkeys/parse-all lines 7))
       (result (sort (monkeys/count-activity-for-rounds test-monkeys 10000)
		     (function >))))
  (print (list result (* (first result) (second result)))))
