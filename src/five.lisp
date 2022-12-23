(load "file.lisp")
(defvar lines nil)
(setq lines (read-default-input "five"))
;; find line that starts with " 1" to figure out number of stacks
(load "string.lisp")
(defvar stack_definition_line nil)

(defvar stack_definition_index nil)
(loop for l in lines
      for i = 0 then (+ 1 i)
      when (starts-with l " 1")
	do (setq stack_definition_line l stack_definition_index i))

(defvar num_stacks (first (last (find-all-int stack_definition_line))))
(defvar initialization_lines (reverse (subseq lines 0 stack_definition_index)))
(defvar move_lines (subseq lines (+ 2 stack_definition_index)))

;; make list of empty lists
(defun stacks/initialize (num-stacks)
    (loop repeat num-stacks
	  collect (list)))

;; the positions of the characters in the line
(defun input-position (stacknr)
  (+ 1 (* 4 stacknr)))

;; we'll need to call this from bottom to top
;; line looks like this
;; "    [R] [D] [L] [C] [N] [Q]     [R]"
(defun stacks/add-input-line (stacks line)
  (loop for stack in stacks
	for n from 0 to (length stacks)
	for c = (char line (input-position n))
	;; do (print (list n c))
	when (alpha-char-p c)
	  do (push c stack)
	collect stack))

(defvar startstacks (stacks/initialize num_stacks))
(print (list "initializing using" initialization_lines))
(loop for line in initialization_lines
      ;; do (print startstacks)
      do (setq startstacks (stacks/add-input-line startstacks line)))

(defun stacks/pretty-print (stacks)
  (loop for stack in stacks
	do (print (reverse stack))))

(stacks/pretty-print startstacks)

;; move crate from src to dst (zero based index)
(defun stacks/apply-move (stacks src dst)
  (let ((crate (first (nth src stacks))))
    (loop for s in stacks
	  for i = 0 then (+ 1 i)
	  collect (cond
		    ;; remove first from source
		    ((= i src) (cdr s))
		    ;; crate on stack
		    ((= i dst) (cons crate s))
		    ;; else return unchanged
		    (t s)))))

(defun stacks/apply-move-alt (stacks num src dst)
  (let
      ;; crates to be moved = first N of src
      ((crates (subseq (nth src stacks) 0 num)))
    (loop for s in stacks
	  for i = 0 then (+ 1 i)
	  collect (cond
		    ;; remove first N from source
		    ((= i src) (subseq s num))
		    ;; put crates on stack in order
		    ((= i dst) (concatenate 'list crates s))
		    ;; else return unchanged
		    (t s)))))

(defun stacks/apply-move-instruction-line (stacks line)
  (let ((numbers (find-all-int line))
	(newstacks stacks))
    ;; first number indicates repetitions
    (dotimes (i (first numbers))
      ;; second is src, third is dst
      (setq newstacks (stacks/apply-move
		       newstacks
		       (- (nth 1 numbers) 1)
		       (- (nth 2 numbers) 1))))
    newstacks))

(defun stacks/apply-move-instruction-line-alt (stacks line)
  (let ((numbers (find-all-int line))
	(newstacks stacks))
    ;; second is src, third is dst
    (setq newstacks (stacks/apply-move-alt
		     newstacks
		     (first numbers)
		     (- (nth 1 numbers) 1)
		     (- (nth 2 numbers) 1)))
    newstacks))

(print "final configuration")
(let ((stacks startstacks))
  (loop for l in move_lines
	do (setq stacks (stacks/apply-move-instruction-line stacks l)))
  (stacks/pretty-print stacks))

(print "alt configuration")
(let ((stacks startstacks))
  (loop for l in move_lines
	do (setq stacks (stacks/apply-move-instruction-line-alt stacks l)))
  (stacks/pretty-print stacks))
