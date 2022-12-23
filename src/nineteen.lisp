;; try to write a generic state search thing

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
            
            ;; if no states are returned, end the calculation
            (funcall state-score-f cur-state)))))

;; CONSTANTS FOR PUZZLE

(defparameter maxtime 24)

;; example cost blueprint
(defparameter bp1-e (list :orebot-ore 4
                         :claybot-ore 2
                         :obsbot-ore 3
                         :obsbot-clay 14
                         :geobot-ore 2
                         :geobot-obs 7
                         ;; utility, so we don't have to take max later
                         :max-ore 4))

;; state. Let's learn something new and use defstruct. This is a macro that automatically
;; defines a bunch of functions, to make something that almost looks object-oriented.
(defstruct state
  (time 0)
  ;; resources
  (ore 0)
  (clay 0)
  (obsidian 0)
  (geode 0)
  ;; bots
  (orebots 1)
  (claybots 0)
  (obsbots 0)
  (geobots 0))

(defun state/end-p (s)
  (> (state-time s) maxtime))

(defun state/score-f (s)
  (state-geode s))

(defun state/advance (s bp &key
                             (buy-orebot nil)
                             (buy-claybot nil)
                             (buy-obsbot nil)
                             (buy-geobot nil))
  "Important detail. Check cost first, then collect ore, then update number of bots. If we
  collect first, then the cost check is wrong. If we collect last, then the income is too high;
  the new bot should not yet be included in the income multiplier."
  (let ((ns (copy-state s)))
    ;; advance time
    (incf (state-time ns))
    ;; increment resources (1 per turn per bot)
    (incf (state-ore ns) (state-orebots ns))
    (incf (state-clay ns) (state-claybots ns))
    (incf (state-obsidian ns) (state-obsbots ns))
    (incf (state-geode ns) (state-geobots ns))
    ;; spending and update number of bots
    (when buy-orebot
      (decf (state-ore ns) (getf bp :orebot-ore))
      (incf (state-orebots ns)))
    (when buy-claybot
      (decf (state-ore ns) (getf bp :claybot-ore))
      (incf (state-claybots ns)))
    (when buy-obsbot
      (decf (state-ore ns) (getf bp :obsbot-ore))
      (decf (state-clay ns) (getf bp :obsbot-clay))
      (incf (state-obsbots ns)))
    (when buy-geobot
      (decf (state-ore ns) (getf bp :geobot-ore))
      (decf (state-obsidian ns) (getf bp :geobot-obs))
      (incf (state-geobots ns)))
    ns))
  

;; we can make this dumb or smart. Will drastically affect runtime.
(defun state/next-f (s bp)
  (let ((next-list nil)
        ;; the basic checks of what's possible + optimization to not buy too many
        (can-buy-orebot (and (< (state-orebots s) (getf bp :max-ore))
                             (>= (state-ore s) (getf bp :orebot-ore))))

        (can-buy-claybot (and (< (state-claybots s) (getf bp :obsbot-clay))
                              (>= (state-ore s) (getf bp :claybot-ore))))

        (can-buy-obsbot (and (< (state-obsbots s) (getf bp :geobot-obs))
                             (>= (state-ore s) (getf bp :obsbot-ore))
                             (>= (state-clay s) (getf bp :obsbot-clay))))
        ;; can buy as many as we want here
        (can-buy-geobot (and (>= (state-ore s) (getf bp :geobot-ore))
                             (>= (state-obsidian s) (getf bp :geobot-obs)))))

    ;; Now, we can invent all kinds of logic.
    (cond
      ;; First optimization: if we have enough income to buy a geobot at every turn, always buy
      ;; a geobot, and never wait.
      ((and can-buy-geobot
            (>= (state-orebots s) (getf bp :geobot-ore))
            (>= (state-obsbots s) (getf bp :geobot-obs)))
       (push (state/advance s bp :buy-geobot t) next-list))

      ;; Another optimization idea:
      ;; - if (orebots >= max ore cost), stop making orebots
      ;; - if (claybots >= max clay cost), stop making claybots
      ;; - if (obsbots >= max obsidian cost), stop making obsbots
      ;; This info is included in the can-buy-bot booleans above

      ;; If we can't buy anything, waiting is the only option
      ((not (or can-buy-orebot can-buy-claybot can-buy-obsbot can-buy-geobot))
       (push (state/advance s bp) next-list))

      ;; if none of the above shortcuts were triggered, try different options. 
      (t
       (let ((waiting-useless
               (or
                ;; no clay income -> always buy if have enough ore for both
                (and (= 0 (state-claybots s)) can-buy-orebot can-buy-claybot)
                ;; no obsidian income -> always buy if have enough resources for all three
                (and (= 0 (state-obsbots s)) can-buy-orebot can-buy-claybot can-buy-obsbot))))
       
         ;; Add an option to WAIT, unless we flagged waiting as useless
         (unless waiting-useless
           (push (state/advance s bp) next-list))
         ;; Add available purchase options (except when not possible or not needed, as
         ;; calculated in the let block above
         (when can-buy-orebot
           (push (state/advance s bp :buy-orebot t) next-list))
         (when can-buy-claybot
           (push (state/advance s bp :buy-claybot t) next-list))
         (when can-buy-obsbot
           (push (state/advance s bp :buy-obsbot t) next-list))
         (when can-buy-geobot
           (push (state/advance s bp :buy-geobot t) next-list)))))
    ;; (print (list can-buy-orebot can-buy-claybot can-buy-obsbot can-buy-geobot))
    ;; (print next-list)
    next-list))

(defun do-puzzle (time)
  (setq maxtime time)
  (let ((s (make-state)))
    (dfss/recurse s
                  #'state/end-p
                  #'state/score-f
                  ;; blueprint is chosen here by pickling next-f
                  (lambda (s) (state/next-f s bp1-e)))))
                  