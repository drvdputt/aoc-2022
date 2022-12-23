(load "file.lisp")
(load "string.lisp")
(load "list-tools.lisp")
(defparameter lines-e (read-default-input "sixteen-example"))
(defparameter lines (read-default-input "sixteen"))

(defun valve/parse (line)
  (list
   ;; fixed position
   :name (subseq line 6 8)
   ;; the only number in the string
   :rate (first (find-all-int line))
   ;; value to the left of every comma, plus the last two characters
   :tunnels (loop for i in (index-of-all-matches line ",")
                  collect (subseq line (- i 2) i) into tunnels
                  finally
                     (return (append tunnels
                                     (list (subseq line (- (length line) 2))))))
   ))

;; opt for hash table instead of association list, because I want to be able to loop over the
;; (unique) keys. Includes all CONSTANTS for the valves. The actual state is in a separate
;; vector.
(defun valvedef/initialize (lines)
  ;; (loop for l in lines
  ;;       for v = (valve/parse l)
  ;;       collect (cons (getf v :name) v)))
  (let ((state (make-hash-table :test #'equal)))
    (loop for l in lines
          for v = (valve/parse l)
          for i from 0
          ;; add an index to each valve. We will use this index to set up a vector tracking
          ;; which valves are open
          do (setf (getf v :index) i)
          do (setf (gethash (getf v :name) state) v))
    state))

(defun valvedef/print (valvedef)
  (pprint-hashmap valvedef))

(defun valvestate/open (valvedef state name)
  (let ((i (getf (gethash name valvedef) :index))
        (new-state (copy-seq state)))
    (setf (elt new-state i) t)
    new-state))

;; true if all non-zero valves are open
(defun valvestate/allopen (valvedef state)
  (loop for v being each hash-value of valvedef
        if (and
            ;; if a non-zero valve...
            (> (getf v :rate) 0)
            ;; ...is closed
            (not (elt state (getf v :index))))
          return nil
        finally
           (return t)))

(defun valvestate/closed (valvedef state name)
  (not (elt state (getf (gethash name valvedef) :index))))

;; score that a valve will deliver, given that you start opening it at the current time. Opening
;; at minute 1 -> flows from minutes 2 - 30 --> total volume = 29 * rate.
(defun valve/score (valve time-remaining)
  (* (getf valve :rate) time-remaining))

;; trivial optimization: if we are at a certain node, at minute 29, the optimal move is to just
;; open the valve. Or wait if all non-zero valves are already open.

;; returns alist of possible moves and the resulting score
;; pos is a valve name referring to the current position
(defun max-score-move (valvedef state pos time-remaining &key (prev-pos nil))
  (cond ((= time-remaining 0)
         (list :best (cons 'wait 0) :history (list (cons 'wait 0))))
        ((= time-remaining 1)
         ;; if still closed, open it. Else wait.
         (let ((best (if (valvestate/closed valvedef state pos)
                         (let ((score (valve/score (gethash pos valvedef) 1)))
                           ;; (print (list time-remaining "opening last valve for" score))
                           (if (> score 0)
                               (cons 'open score)
                               (cons 'wait 0)))
                         (cons 'wait 0))))
           (list :best best :history (list best))))
        (t
         (try-score-move-recursions
          valvedef
          state
          pos
          time-remaining
          :prev-pos prev-pos))))


;; returns (:best best :history (list moves))
(defun try-score-move-recursions (valvedef state pos time-remaining &key (prev-pos nil))
  ;; (print (list time-remaining pos (action-suggest valvedef state pos :prev-pos prev-pos)))
  (loop for action in (action-suggest valvedef state pos :prev-pos prev-pos)
        with best-result = nil
        with this-result = nil
        with best-action = nil
        ;; if the action is wait, then we quit
        if (equal 'wait (car action))
          return (list :best (cons 'wait 0) :history (list (cons 'wait 0)))

        ;; else, calculate the score for this action. The score is the points we gain by
        ;; doing the action (= 0 for any moves), plus whatever is scored recursively.

        if (equal 'move (car action))
          ;; Move --> recurse from a different position. Pass current position as prev-pos, to
          ;; make sure we don't immediately move back
          do (setq this-result (max-score-move
                                valvedef
                                state
                                (cdr action)
                                (- time-remaining 1)
                                :prev-pos pos))

        if (equal 'open-valve (car action))
          ;; Open valve --> recurse from same position with the state changed                    
          do (progn
               (setq this-result (max-score-move
                                  valvedef
                                  (valvestate/open valvedef state pos)
                                  pos
                                  (- time-remaining 1)))
               ;; do not forget to add the score of the valve we just opened!
               (incf (cdr (getf this-result :best))
                     (valve/score
                      (gethash pos valvedef)
                      time-remaining)))
             ;; do (print (list this-result best-result))
        if (or
            (not best-result)
            (> (cdr (getf this-result :best))
               (cdr (getf best-result :best))))
          ;; If first move or higher score remember it.
          do (setq best-result this-result
                   best-action action)
             
        finally
           ;; for the best result, we return the move-score and add the new step to history
           (return (list :best (getf best-result :best)
                         :history (append (getf best-result :history)
                                          (list best-action))))))

;; Smart way to reduce the number of possible moves. Score calculation does not happen yet here,
;; just a list of things to try

;; returns list of actions ('move tunnelname) ('wait nil) or ('open-valve nil)
(defun action-suggest (valvedef state pos &key (prev-pos nil))
  (let ((action-list nil))
    (if (valvestate/allopen valvedef state)
        ;; if all relevant valves are open, wait
        (push (cons 'wait pos) action-list)
        ;; else, look at the valve and... 
        (let ((v (gethash pos valvedef)))
          ;; add all the moves
          (loop for tunnel in (getf v :tunnels)
                if (or (not prev-pos) (not (string= tunnel prev-pos)))
                  do (push (cons 'move tunnel) action-list))
          ;; add open-valve if flow rate nonzero and valve closed
          (when (and (> (getf v :rate) 0)
                     (valvestate/closed valvedef state pos))
            (push (cons 'open-valve pos) action-list))))
    (if action-list
        action-list
        ;; if after all this, still no actions are available, there's no use in further pursuing
        ;; this path, since it's not the most efficient (e.g. we end up in a dead end for the
        ;; second time, after already opening the valve). signal wait to end the search
        (list (cons 'wait pos)))))

;; -------- alternate method --------

;; (load "~/quicklisp/setup.lisp")
(ql:quickload :alexandria)
(load "dijkstra.lisp")

(defun best-score-all-permutations (valvedef matrix valves-to-permute time
                                    &key (size nil) (valves-fixed nil))
  (let* ((max-score nil)
         (best-permutation nil)
         (counter 0)
         (the-size (if size size (length valves-to-permute)))
         (num-permutations
           (apply (function *)
                  (loop for i from 1 to the-size collect i))))
    ;; (print (list num-permutations "permutations"))
    (alexandria:map-permutations
     (lambda (permutation)
       (let ((score (open-valves-in-order valvedef
                                          matrix
                                          ;; prefix the valve order with the fixed valves if so
                                          ;; requested
                                          (if valves-fixed
                                              (append valves-fixed permutation)
                                              permutation)
                                          time)))
         (when (or (not max-score) (> score max-score))
           (setq max-score score
                 best-permutation permutation))))
         ;; (incf counter)
         ;; (when (= 0 (mod counter 1000000))
         ;;   (print (list counter score max-score)))))
     valves-to-permute
     ;; optionally, only try out all subset permutations
     :length the-size)
    ;; (print (open-valves-heuristic valvedef matrix non-zero-names 30))
    (list :score max-score :order best-permutation)))

;; go over all valves in valve-order, given the distance matrix between those valves. For each
;; valve, move to it, then open it. The travel time and opening time are subtracted from time
;; remaining, and the resulting valve score is added to the total.

;; we will frequently have repeated orders (e.g. a b c . with . running over all others). The
;; keys are (cons time valves).
(defparameter open-valves-in-order-memo (make-hash-table :test #'equal))

(defun open-valves-in-order-memo-set (time valve-order time-remaining score)
  (let ((memo-key (cons time valve-order)))
    ;; (print (list "saving memo" memo-key :time-remaining time-remaining :score score))
    (unless (gethash memo-key open-valves-in-order-memo)
      (setf (gethash memo-key open-valves-in-order-memo)
            (list :time-remaining time-remaining :score score)))))

(defun open-valves-in-order (valvedef matrix valve-order time &key (use-memo nil))
  (let ((valves-todo valve-order)
        (time-remaining time)
        (start-score 0)
        (v (gethash "AA" valvedef)))

    ;; find the best memo
    (when use-memo
      (loop for end from (- (length valve-order) 1) downto 1
            ;; if we have a memo for the first n
            for skipped-valves = (subseq valve-order 0 end)
            for memo-key = (cons time skipped-valves)
            for memo-value = (gethash memo-key open-valves-in-order-memo)
            if memo-value
              do (setq valves-todo (subseq valve-order end)
                       ;; score is remembered
                       start-score (getf memo-value :score)
                       ;; time skips forward
                       time-remaining (getf memo-value :time-remaining))
                 ;; starting valve is no longer "AA"
              and do (setq v
                           (gethash (car (last skipped-valves)) valvedef))
                     ;; and do (print (list valve-order "memo'd" skipped-valves "continuing with" valves-todo))
              and return nil))

    (loop for next-name in valves-todo
          for next-v = (gethash next-name valvedef)
          with score = start-score
          ;; travel to the valve and open it (by adding score). Time decreases with travel time
          ;; + 1
          do (setq time-remaining (- time-remaining
                                     (+ (aref matrix (getf v :index) (getf next-v :index))
                                        1))
                   v next-v)
          if (<= time-remaining 0)
            ;; make memo
            do (when use-memo (open-valves-in-order-memo-set time valve-order time-remaining score))
               ;; if time is up, do not count the score! We do not want negative numbers
               ;; muddying the waters.
            and return score
          else
            ;; only increase score if positive time
            do (incf score (valve/score v time-remaining))

          finally (progn
                    ;; also make memo here
                    (when use-memo (open-valves-in-order-memo-set time valve-order time-remaining score))
                    ;; (print score)
                    (return score)))))

(defun open-valves-heuristic (valvedef matrix nonzero-valves time)
  ;; (print valve-order)
  (loop for i from 0
        with time-remaining = time
        with v = (gethash "AA" valvedef)
        ;; find best scoring move
        for best-name = (loop for name in nonzero-valves
                              for next-v = (gethash name valvedef)
                              for d = (aref matrix (getf v :index) (getf next-v :index))
                              for score = (valve/score next-v (- time-remaining (+ 1 d)))
                              with best-score = 0
                              with best-name = nil
                              ;; do (print (list name next-v d score))
                              if (> score best-score)
                                do (setq best-score score
                                         best-name name)
                              finally (return best-name))

        for next-v = (gethash best-name valvedef)

        do (print (list best-name time-remaining nonzero-valves score))
           ;; if all scores are negative, the loop above will return nil
        if (not best-name)
          return score

        ;; travel to the valve and open it. Time decreases with travel time + 1
        do (setq time-remaining (- time-remaining (+ (aref matrix (getf v :index) (getf next-v :index)) 1))
                 v next-v)
        summing (valve/score v time-remaining) into score

        ;; remove from the todo list
        do (setq nonzero-valves (remove best-name nonzero-valves :test #'equal))

           
           ;; time is up or valves are all open
        if (or (not nonzero-valves)
               (<= time-remaining 0))
          return score
        finally (progn
                  ;; (print score)
                  (return score))))


(defun make-distance-matrix (valvedef)
  (let* ((index-names-alist (loop for v being each hash-value of valvedef
                                  collect (cons (getf v :index) (getf v :name))))
         (matrix (dijkstra/distance-matrix-init (length index-names-alist))))
    ;; first, fill in the matrix
    ;; (print index-names-alist)
    ;; we go over all array rows. 
    (loop for i from 0 below (array-dimension matrix 0)
          ;; If we find a -1, start a dijkstra explorer from there.
          if (loop for j from 0 below (array-dimension matrix 1)
                   when (= -1 (aref matrix i j)) return t)
            do (dijkstra/recurse
                matrix
                i
                ;; function that gets the index of the neighbors
                (lambda (i)
                  ;; find name based on index
                  (let* ((name (cdr (assoc i index-names-alist)))
                         ;; find valve based on name
                         (v (gethash name valvedef))
                         ;; get tunnels from valve
                         (tunnels (getf v :tunnels)))
                    ;; convert every tunnel in the list to index
                    ;; (print tunnels)
                    (mapcar (lambda (name)
                              (car (rassoc name index-names-alist :test 'equal)))
                            tunnels)))))
    ;; and do (print matrix))
    matrix))

;; get (list :score and :order)
(defun smart-find-best-permutation (valvedef matrix valves time permutation-depth)
  ;; try combinations (e.g. 7 out of all) and remember with which valve the best result was.
  ;; Then continue with "best start" + combinations (7 out of remainig vales).
  (let ((valves-fixed nil)
        (valves-to-permute (copy-list valves)))
    (loop for num-fixed from 0 below (length valves)
          for result = (best-score-all-permutations
                        valvedef matrix valves-to-permute time
                        :size (min permutation-depth (length valves-to-permute))
                        :valves-fixed valves-fixed)
          ;; the partial permutations tell us that this might be the best order to continue
          for best-next = (car (getf result :order))
          ;; do (print result)
          do (setq
              ;; use the first valve of this order as the next "fixed opening" step 
              valves-fixed (append valves-fixed (list best-next))
              ;; and stop including it in the permutation search
              valves-to-permute (remove best-next valves-to-permute))
          ;; do (print (list :fixed valves-fixed :permute valves-to-permute))
          finally
             (progn
               ;; (print (list "best order" valves-fixed))
               (return (list :score (getf result :score)
                             :order valves-fixed))))))

;; go over all ways to divide the valves between the two actors (give all combinations to left,
;; and the rest to right). Calculate the best permutation and score individually.
(defun do-part2 (valvedef matrix valves time permutation-depth)
  (flet ((score-f (actor1-combination)
           ;; valves for actor2 are all valves except those for actor1
           (let* ((actor2-combination (remove-if
                                       (lambda (v) (member v actor1-combination))
                                       valves))
                  (result1 (smart-find-best-permutation valvedef
                                                        matrix
                                                        actor1-combination
                                                        time
                                                        permutation-depth))
                  (result2 (smart-find-best-permutation valvedef
                                                        matrix
                                                        actor2-combination
                                                        time
                                                        permutation-depth)))
             ;; (print (list "divide" actor1-combination actor2-combination))
             (+ (getf result1 :score) (getf result2 :score)))))
    ;; go over all sizes of actor1 (up to half size), and keep track of the best combo while
    ;; trying all combinations of that size.
    (loop for actor1-size from (floor (length valves) 3) to (floor (length valves) 2)
          maximize (let ((best-combo nil)
                         (best-score 0))
                     (alexandria:map-combinations
                      (lambda (combo)
                        (let ((combo-score (funcall #'score-f combo)))
                          (when (> combo-score best-score)
                            (setq best-score combo-score
                                  best-combo combo))))
                      valves
                      :length actor1-size)
                     (print (list "best combo of size" actor1-size
                                  "is" best-combo
                                  "with score" best-score))
                     best-score))))

;; (print (do-puzzle lines-e))
;; (print (do-puzzle lines))
(defun do-puzzle (lines &key (permutation-depth 4))
  (clrhash open-valves-in-order-memo)
  (let* ((valvedef (valvedef/initialize lines))
         (matrix (make-distance-matrix valvedef))
         (non-zero-names (loop for v being each hash-value of valvedef
                               if (> (getf v :rate) 0)
                                 collect (getf v :name))))
    (print (list
            "part 1"
            (smart-find-best-permutation valvedef
                                         matrix
                                         non-zero-names
                                         30
                                         permutation-depth)))
    (do-part2 valvedef matrix non-zero-names 26 permutation-depth)))

