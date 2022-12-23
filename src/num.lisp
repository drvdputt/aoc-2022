(defun at-max (valuelist otherlist)
  (let ((maxval 0)
	(maxother 0))
    (loop for v in valuelist
	  for x in otherlist
	  do (if (> v maxval)
		 (setq maxval v maxother x)
		 nil))
    (list maxval maxother)))

(defun count-predicate (value_list predicate_function)
  (count T (mapcar predicate_function value_list)))

(defun range (imin imax &key (exclude-start nil) (exclude-end nil))
  (let ((ascending (< imin imax))
	(start imin)
	(end imax))
    (if exclude-start
	(if ascending (incf start) (decf start)))
    (if exclude-end
	(if ascending (decf end) (incf end)))
    (if ascending
	(loop for i from start to end collect i)
	(loop for i from start downto end collect i))))

(defun pprint-char-array (a)
  (loop for i from 0 below (array-dimension a 0)
        do (loop for j from 0 below (array-dimension a 1)
                 do (write-char (aref a i j))
                    finally (terpri))))
