(defun dfss/recurse (cur-state
                     end-p
                     state-score-f
                     next-states-f)
  "Depth first state search.

   STATE can be anything. This state (and evolutions of this state) will be passed to the
   functions below. The details depend on these functions.

   END-P should take a STATE as an argument, and return t when the state is considered final. It
   is recommended that the STATE tracks the depth in some way, and cuts off the calculation at a
   certain depth.

   STATE-SCORE-F should return the score of a STATE. Will be called once END-PREDICATE-F returns
   t, and we start going back up in the recursion. Branches will be chosen to maximize the
   score. Best state is passed all the way upward.

   NEXT-STATES-F should return a list of STATE objects, each one typically has a different type
   of evolution step applied to them. Try to make this function as restrictive as possible, so
   that the number of possible paths does not explode. If no options are returned, the branch
   also ends and the score is returned."
  (if (funcall end-p cur-state)
      ;; If the current state is past the end, just return the score. This breaks off the
      ;; recursion.
      ;; (funcall state-score-f cur-state)
      cur-state
      ;; Else, get the possible next states, and recursively continue the state evolution. Once
      ;; the end of the branch returns the score, it will be passed up and returned here. The
      ;; maximum score of all t he sub-branches is passed up.
      (let ((next-states (funcall next-states-f cur-state)))
        (if next-states
            (loop for next-state in next-states
                  with best-state = nil
                  with best-score = nil
                  do (let* ((return-state (dfss/recurse next-state
                                                        end-p
                                                        state-score-f
                                                        next-states-f))
                            (return-score (funcall state-score-f return-state)))
                       (when (or (not best-score) (> return-score best-score))
                         (setq best-score return-score
                               best-state return-state)))
                  finally (return best-state))

            ;; if no next states were possible, end the calculation by returning the current
            ;; state
            cur-state))))
