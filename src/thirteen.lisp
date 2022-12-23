(defun ensure-list (i)
  (if (numberp i)
      (list i)
      i))

;; trinary: -1 in order, 1 out of order, 0 undecided
(defun compare-any (v1 v2)
  (cond
    ;; both integer. this is the exit of the recursion
    ((and (numberp v1) (numberp v2))
     (compare-int v1 v2))
    ;; at least one list --> recurse
    (t
     (comp-lists (ensure-list v1) (ensure-list v2)))))

(defun compare-int (v1 v2)
  (cond ((< v1 v2) -1)
	((= v1 v2) 0)
	(t 1)))
	 
(defun comp-lists (v1 v2)
  (loop for x1 in v1
	for x2 in v2
	with c = nil
	do (setq c (compare-any x1 x2))
	if (not (= c 0))
	  return c
	finally
	   (return (compare-int (length v1) (length v2)))))

(load "file.lisp")
(defvar lines (read-default-input "thirteen"))

(load "string.lisp")
;; first character needs to be [, and last character needs to be ].
;; this will also be recursive: if another [ is encountered, we do
;; parse-packet on this [ ... ] sub-sequence. The exit for this
;; recursion, is when there's no more [. Then we can just collect all
;; integers.
(defun parse-packet (l)
  (let ((num-brackets (length (index-of-all-matches l "\\["))))
    (cond
      ;; single integer
      ((= num-brackets 0)
       (parse-integer l))
      ;; list of integers 
      ((= num-brackets 1)
       (find-all-int l))
      ;; nested, use recursion
      (t
       (loop for sub in (split-packet l)
	     collect (parse-packet sub))))))


(defun split-packet (l)
  ;; ignore first and last character ([ and ]), so we have only the
  ;; comma separated values
  (let ((csv (subseq l 1 (- (length l) 1))))
    (loop for c across csv
	  for i from 0
	  with fragment-start = 0
	  with bracket-balance = 0
	  ;; if bracket balance > 0, we are inside a sublist and we need
	  ;; to keep accumulating characters for this substring
	  if (char= c #\[)
	    do (incf bracket-balance)
	  if (char= c #\])
	    do (decf bracket-balance)
	       ;; if bracket balance = 0 and we encounter a comma, then
	       ;; we collect the substring and set up for the next one
	  if (and (= bracket-balance 0) (char= c #\,))
	    collect (subseq csv fragment-start i) into fragments
	    and do (setq fragment-start (+ 1 i))
	  finally
	     ;; at the end, there's no comma anymore, so we need to add
	     ;; this part separately
	     (return (append fragments (list (subseq csv fragment-start)))))))
	   
(defun do-puzzle (lines)
  (let ((num-pairs (/ (+ 1 (length lines)) 3)))
    (loop for i from 1 to num-pairs
	  for j = (* 3 (- i 1))
	  for v1 = (parse-packet (nth j lines))
	  for v2 = (parse-packet (nth (+ 1 j) lines))
	  if (= -1 (compare-any v1 v2))
	    collect i into ivals
	    and sum i into isum
	  finally (return (list ivals isum)))))

(defun do-part2 (lines)
  (let* ((dividers (list (list (list 2))
			 (list (list 6))))
	 (packets (concatenate 'list
			       dividers
			       (loop for l in lines
				     if (not (string= l ""))
				       collect (parse-packet l))))
	 (sorted-packets (sort packets (lambda (v1 v2)
					 (= -1 (compare-any v1 v2))))))
    (print sorted-packets)
    (loop for p in sorted-packets
	  for i from 1 
	  if (= 0 (compare-any p (first dividers)))
	    do (print (list "first divider at" i))
	  if (= 0 (compare-any p (second dividers)))
	    do (print (list "second divider at" i)))))
	       

(defvar lines-e (read-lines "thirteen-example-input.txt"))
