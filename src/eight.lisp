(load "file.lisp")
(defvar lines nil)
(defvar nrows nil)
(defvar ncols nil)
(setq lines (read-default-input "eight"))
(setq nrows (length lines))
(setq ncols (length (first lines)))

(defun make-tree-array-zeros ()
  (make-array (list nrows ncols) :element-type 'integer))

(defun make-array-same-shape (array)
  (make-array (array-dimensions array) :element-type 'integer))

(defvar tree_array (make-tree-array-zeros))
(defvar visible_tree_array (make-tree-array-zeros))

;; fill the tree map as an array of ints
(loop for irow below nrows
      for l in lines
      do (loop for icol below ncols
	       for c across l
	       do (setf (aref tree_array irow icol) (digit-char-p c))))

;; now determine for each position, if it is visible

;; we do four actions:
;; row wise
;; row wise reverse
;; col wise
;; col wise reverse

(defun array/row (array irow)
  (make-array (array-dimension array 1)
	      :displaced-to array
	      :displaced-index-offset (* irow (array-dimension array 1))))

(defun array/col (array icol)
  (let ((col-array (make-array (array-dimension array 0))))
    (loop for irow from 0 below (array-dimension array 0)
	  do (setf (aref col-array irow) (aref array irow icol)))
    col-array))
    
;; return indices of visible trees given a certain ordening of trees
(defun forward-visible-indices (trees)
  (let ((tmax -1))
    (loop for tree across trees
	  for i = 0 then (+ 1 i)
	  if (> tree tmax)
	    collect i
	    and do (setq tmax (max tree tmax)))))

;; same but looking from the other side   
(defun backward-visible-indices (trees)
  (let ((backward-result (forward-visible-indices (reverse trees))))
    (mapcar (lambda (i)
	      (- (- (length trees) 1) i))
	    backward-result)))

;; check visibility for all rows
(loop for irow below nrows
      do (let ((rowtrees (array/row tree_array irow)))
	   (loop for icol in (concatenate
			      'list
			      (forward-visible-indices rowtrees)
			      (backward-visible-indices rowtrees))
		 do (incf (aref visible_tree_array irow icol)))))

;; check visibility for all cols
(loop for icol below ncols
      do (let ((coltrees (array/col tree_array icol)))
	   (loop for irow in (concatenate
			      'list
			      (forward-visible-indices coltrees)
			      (backward-visible-indices coltrees))
		 do (incf (aref visible_tree_array irow icol)))))

;; count total number of trees with visibility > 0
(loop for irow below nrows
      sum (loop for icol below ncols
		count (> (aref visible_tree_array irow icol) 0)))

;; calculate scenic scores for a vector of trees (in one direction)
(defun forward-scenic-scores (trees)
  (concatenate
   'list
   ;; first two are always zero and one
   '(0 1)
   ;; the rest depend on the trees to the left
   (loop for i from 2 below (length trees)
	 for current-tree = (elt trees i)
	;; find distance of previous tree bigger than this one
	 for back-dist = (loop for i-back from 1 to i
			       ;; when current tree <= previous tree
			       for previous-tree = (elt trees (- i i-back))
			       if (<= current-tree previous-tree)
				 return i-back
			       ;; at finally, we're one too far
			       ;; (yes, e.g. from 1 to 5 actually goes to 6 at "finally")
			       finally (return (- i-back 1)))
	 collect (if (> back-dist 0) back-dist 1))))

(defun backward-scenic-scores (trees)
  (reverse (forward-scenic-scores (reverse trees))))

;; now apply this in 4 directions
(defun tree-array/scenic-scores (tree-array)
  (let ((l_scores (make-array-same-shape tree-array))
	(r_scores (make-array-same-shape tree-array))
	(d_scores (make-array-same-shape tree-array))
	(u_scores (make-array-same-shape tree-array))
	(nrows (array-dimension tree-array 0))
	(ncols (array-dimension tree-array 1)))
    (loop for irow below nrows
	  do (let ((trees (array/row tree-array irow)))
	       (loop for icol below ncols
		     for l_score in (forward-scenic-scores trees)
		     for r_score in (backward-scenic-scores trees)
		     do (setf (aref l_scores irow icol) l_score)
		     do (setf (aref r_scores irow icol) r_score))))
    (loop for icol below ncols
	  do (let ((trees (array/col tree-array icol)))
	       (loop for irow below nrows
		     for u_score in (forward-scenic-scores trees)
		     for d_score in (backward-scenic-scores trees)
		     do (setf (aref d_scores irow icol) d_score)
		     do (setf (aref u_scores irow icol) u_score))))
    ;; set the edges to zero
    (loop for irow below nrows
	  do (setf (aref l_scores irow (- ncols 1)) 0)
	  do (setf (aref r_scores irow 0) 0))
    (loop for icol below ncols
	  do (setf (aref u_scores (- nrows 1) icol) 0)
	  do (setf (aref d_scores 0 icol) 0))
    (list :left l_scores :right r_scores :down d_scores :up u_scores)))

(ql:quickload :array-operations)

(defun tree-array/scenic-multiply (tree-array)
  (let ((scores (tree-array/scenic-scores tree-array)))
    (reduce (lambda (a b)
	      (aops:each (function *) a b))
	    (list (getf scores :up)
		  (getf scores :down)
		  (getf scores :left)
		  (getf scores :right)))))
	
(defvar small_test (make-array '(5 5) :element-type 'integer
				      :initial-contents '((3 0 3 7 3)
							  (2 5 5 1 2)
							  (6 5 3 3 2)
							  (3 3 5 4 9)
							  (3 5 3 9 0))))

(print (tree-array/scenic-scores small_test))
(print (tree-array/scenic-multiply small_test))

(print (tree-array/scenic-multiply tree_array))
(print (loop for i across (aops:flatten (tree-array/scenic-multiply tree_array))
	     maximize i))
