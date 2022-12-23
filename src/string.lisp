;; find the first character of string 1, that also appears in string 2
(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre)

(defun starts-with (s1 s2)
  (and (>= (length s1) (length s2))
       (string= (subseq s1 0 (length s2)) s2)))

(defun string-contains-char (s c)
  (not (null (find c s))))

(defun count-character-dict (s)
  (let ((hist nil))
    (loop for c across s
	  for entry = (assoc c hist)
	  ;; do (print hist)
	  if (not entry)
	    do (setq hist (acons c 1 hist))
	  else
	    do (rplacd entry (incf (cdr entry))))
    hist))

(defun has-duplicate-character (s)
  (let ((hist nil))
    (loop for c across s
	  for entry = (assoc c hist)
	  ;; do (print hist)
	  if (not entry)
	    do (setq hist (acons c 1 hist))
	  else
	    return t
	  finally (return nil))))

(defun alltrue (values)
  (print values)
  (every #'identity values))

(defun strings-contain-char (strings c)
  (alltrue (mapcar
	    (lambda (s) (string-contains-char s c))
	    strings)))

(defun common-character (s1 s2)
  (loop for c across s1
	when (string-contains-char s2 c)
	return c))

(defun common-character-multi (strings)
  (loop for c across (first strings)
	when (strings-contain-char (subseq strings 1) c)
	return c))

(defun string-halves (s)
  (let ((halfway (/ (length s) 2)))
    (list (subseq s 0 halfway)
	  (subseq s halfway))))

(defun s_to_int (s)
  (parse-integer s))

(defun find-all-int (s)
  (mapcar (function parse-integer)
	  (cl-ppcre:all-matches-as-strings "[0-9]+" s)))

(defun index-of-all-matches (s expr)
  ;; collect only even element (start of match, don't need end of match)
  (loop for i in (cl-ppcre:all-matches expr s)
	for j = 0 then (+ 1 j)
	if (evenp j)
	  collect i))

(defun index-of-last-match (s expr)
  (let ((matches (cl-ppcre:all-matches expr s)))
    (nth (- (length matches) 2) matches)))
