;; build up a distance matrix using dijktra's algorithm
(defun dijkstra/distance-matrix-init (num-nodes)
  (make-array (list num-nodes num-nodes)
              :element-type 'integer
              :initial-element -1))

(defun dijkstra/set-distance (matrix i j d)
  (setf (aref matrix i j) d)
  (setf (aref matrix j i) d))

(defun dijkstra/update-distance (matrix i j d)
  (let ((old-d (aref matrix i j)))
    (when (or (= -1 old-d) (< d old-d))
      (dijkstra/set-distance matrix i j d))))
          
;; trying to make this generic, so i can use it in the future
;; name-to-index-f: function that takes a string (node name) and translates it to an index in
;; the distance matrix
;; node-name: string
;; prev-node-names: list of strings, to backpropagate the distance info. nil at the entry point
;; of the recursion.
;; next-node-names-f: function that returns the nodes to explore next, given the current node
;; name and the previous node name
(defun dijkstra/recurse (matrix
                         i ;; node to explore
                         neighbor-indices-f ;; function that returns the indices of the neighbors 
                         &key (j-prev nil)) ;; nodes previously explored in this path
  (let ((neighbors (funcall neighbor-indices-f i))
        (new-j-prev (cons i j-prev)))
    ;; (print (list "exploring from" i "which has neighbours" neighbors))

    ;; first, do the backpropagation to all previous nodes. Also takes care of setting neighbor
    ;; distance to 1, since the neighbor is always at index 1 in the (updated) history.
    (loop for j in new-j-prev
          for d from 0
          do (dijkstra/update-distance matrix i j d))

    ;; then, figure out where to go next, and recurse
    (loop for j in neighbors
          if (dijkstra/needs-exploration matrix new-j-prev j)
            do (dijkstra/recurse matrix
                                 j
                                 neighbor-indices-f
                                 :j-prev new-j-prev))))
          ;; else
            ;; do (print (list "do not explore from" i "to" j)))))
            
;; does node j need exploration, given i-history
(defun dijkstra/needs-exploration (matrix j-prev j-next)
  ;; (print (list "needs-exploration" j-prev j-next))
  (if (member j-next j-prev)
      ;; if number is already in history, stop exploring, we don't want loops
      nil
      ;; ;; else, we keep exploring as long as the backpropagation keeps finding new distances
      ;; ;; (detours matter! we want the distance ALL pairs of points). Same deal if a shorter
      ;; ;; path between one of the points in the history and the next point would be found.
      ;; (loop for jp in j-prev
      ;;       for d from 1
      ;;       for old = (aref matrix j-next jp)
      ;;       if (or (= -1 old) (< d old))
      ;;         do (print (list "re-exploring would improve distance" j-next jp "from" old "to" d))
      ;;         and return t
      ;;       ;; if no improvement at all, no need to explore this path further
      ;;       finally (return nil))))

      ;; scratch that. This is an optimization that only works for partial distance matrices
      ;; (those from a single starting point). If we want to fill the full distance matrix, then
      ;; we NEED to explore all paths (except loops). Otherwise, some connections can be missed.
      ;; There might be an optimization possible (e.g. if we already know the ideal distance
      ;; from C to F, then to know the distance from A to F we only need to traverse A, B and C,
      ;; and then add C to F (and fill in B to F, A to F).

      ;; let's just do everything. Seems the right thing to do if there's much branching.
      t))

;; alternate method, where we don't need to now the distance. We only want to flag the points in
;; terms of being reachable from the given starting point. Will fill an array of length
;; num-nodes with 1 and -1, with 1 at position j meaning that a node j was reachable from node
;; i.
(defun dijkstra/visitor (num-nodes
                         ;; starting node
                         i
                         ;; function that returns the neighboring indices, given an index
                         neighbor-indices-f)
  (let ((visited (make-array num-nodes
                             :element-type 'integer
                             :initial-element -1)))
    (dijkstra/visitor-recurse visited i neighbor-indices-f)
    visited))

(defun dijkstra/visitor-recurse (visit-vector
                                 i
                                 neighbor-indices-f)
  ;; register that we visited the current node
  (setf (elt visit-vector i) 1)
  (let ((neighbors (funcall neighbor-indices-f i)))
    ;; then visit the neighbors that still need a visit (recurse)
    (loop for j in neighbors
          if (= -1 (elt visit-vector j))
            do (dijkstra/visitor-recurse visit-vector j neighbor-indices-f))))

    
                         
                         
                         
                         
