(load "file.lisp")
(load "string.lisp")
(defvar lines (read-default-input "seven"))

(defvar path "/")
;; use hash table compatible with strings as keys (need equal as the compare function)
(defvar dutable (make-hash-table :test 'equal))
(defun dutable/print (dutable)
  (loop for k being each hash-key of dutable
	do (print (concatenate
		   'string
		   k
		   " : "
		   (write-to-string (gethash k dutable))))))

(defun path/add (current-path name)
  (if (string= name "/")
      "/"
      (let ((ends-with-slash (char= (char current-path (- (length current-path) 1)) #\/)))
	(concatenate
	 'string
	 (if ends-with-slash
	     current-path
	     (concatenate 'string current-path "/"))
	 name
	 "/"))))

;; needs trailing slash for directories!
(defun path/up (path)
  (let ((result (subseq path 0 (+ 1 (car (last (index-of-all-matches path "/") 2))))))
    (if (string= result "")
	"/"
	result)))

;; list of all parents of the path. I should add file size to each of these
(defun path/all-parents (path)
  (loop for c across path
	for i = 0 then (+ 1 i)
	do (print c)
	if (char= c #\/)
	  collect (subseq path 0 (+ 1 i))))
       

;; add dir relative to given path and set dir size entry to 0 in the table
(defun dutable/dir (dutable current-path dir)
  (setf (gethash (path/add current-path dir) dutable) 0))

;; register file size in the current dir
(defun dutable/file (dutable current-path fsize)
  ;; (print (list dutable current-path fsize))
  (loop for p in (path/all-parents current-path)
	do (setf (gethash p dutable)
		 (+ (gethash p dutable) fsize))))

(defun process-line (l)
  (cond
    ;; dir
    ((starts-with l "dir")
     (dutable/dir dutable path (subseq l 4)))
    ;; cd ..
    ((starts-with l "$ cd ..")
     (setq path (path/up path)))
    ;; cd other
    ((starts-with l "$ cd")
     (setq path (path/add path (subseq l 5))))
    ;; ls (do nothing)
    ((starts-with l "$ ls")
     nil)
    ;; all the rest is files (should start with number)
    (t
     (dutable/file dutable path (first (find-all-int l))))))

;; initialize manually with "/"!
(dutable/dir dutable "/" "/")
(loop for l in lines
      do (print (list l " -> " path))
      do (process-line l))
      ;; do (dutable/print dutable))
	 
(dutable/print dutable)

(print (loop for k being each hash-key of dutable
	     for size = (gethash k dutable)
	     when (<= size  100000)
	       do (print (list k size))
	       and sum size))

;; part 2

(defvar total 70000000)
(defvar required 30000000)
(defvar used (gethash "/" dutable))
(defvar free (- total used))
(defvar to_delete (- required free))

(print (loop for k being each hash-key of dutable
	     for size = (gethash k dutable)
	     when (> size to_delete)
	       minimize size))
