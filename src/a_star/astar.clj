(require '[clojure.set :as set])
(use 'clojure.stacktrace)	 

(defn priority-queue []
  [(sorted-map) #{}])

(defn get-best [queue]
  (let [[d [h & r]] (first queue)]
    h))

(defn remove-best [queue]
  (let [[d entries] (first queue)] 
    (if (= (count entries) 1)
      (dissoc queue d)			
      (conj queue [d (rest entries)]))))

(defn enqueue [[queue queued-set] state cost-fn]
  (if (not (queued-set (:state state)))
    [(merge-with #(concat %2 %1) queue (sorted-map (cost-fn state) [state]))
     queued-set]
    [queue queued-set]))

(defn enqueue-states [pq-and-av states cost-fn]
  (reduce #(enqueue %1 %2 cost-fn) pq-and-av states))

;;; from user's functions that operate on gamestate
;;; create functions that operate on our state nodes

(defn wrap-equals [eq-func]
  (fn [a b] (eq-func (:state a)(:state b))))

(defn wrap-successors [succ-fn]
  (fn [a] (map (fn [gamestate] {:path (conj (:path a) (:state a)) :state gamestate})
               (succ-fn (:state a)))))

(defn wrap-h [h goal]
  #(h (:state goal) (:state %)))

(defn encapsulate [initial-state goal-state successors eq heuristic]
  (let [start {:path [] :state initial-state}
        goal {:path [] :state goal-state}
        eq (wrap-equals eq)
        succ (wrap-successors successors)
        h (wrap-h heuristic goal-state)]
    [start goal succ eq h]))

(defn g [state]
  (count (:path state)))

(defn expand [successors h [priority-queue already-visited]]
  ;; returns [priority-queue already-visited]
  (let [state (get-best priority-queue)
        av (conj already-visited (:state state))
        children (filter #(not (av %)) (successors state))]
    (enqueue-states [(remove-best priority-queue) av] children #(+ (g %) (h %)))))

(defn A* [start goal succ eq h]
  (let [[start goal succ eq h] (encapsulate start goal succ eq h)
        [pq av] [(sorted-map (h start) [start]) #{}]
        not-found? (fn [[p a]] (and (not (nil? (get-best p))) ;; there are more nodes
                                    (not (eq (get-best p) goal)))) ;; next node not goal
        [pq-final a] (first (drop-while not-found?
                                        (iterate (partial expand succ h) [pq av])))] ;; This is the search loop
    (get-best pq-final))) ;; goal node is at front of priority queue


;;;;;;;;;;;;;;;;
;; String game ;
;;;;;;;;;;;;;;;;

(defn string-game [s] {:path nil :state s})

(defn str-swap [s i j]
  (let [v (into [] s)]
    (apply str (assoc (assoc v i (v j)) j (v i)))))

(defn next-states [str]
  (map #(str-swap str % (inc %)) (range (dec (count str)))))


(defn distance [a b]
  (apply + (map #(if (= %1 %2) 0 1) a b)))

(defn h [goal s]
  (int (/ (distance goal s) 2)))

(A* "abcd" "dcab"  next-states = h)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 16 puzzle
(def puzzle [[3 3]			;position of zero
             [[1 2 3 4]
              [5 6 7 8]
              [9 10 11 12]
              [13 14 15 0]]])

(def moves [[-1 0][1 0][0 -1][0 1]])

(defn legal [[dx dy]]
  (and (>= dx 0)(<= dx 3)(>= dy 0)(<= dy 3)))

(defn vec-swap [m p1 p2]
  (assoc-in 
   (assoc-in m p2 (get-in m p1))
   p1 (get-in m p2)))

(defn move [puzzle [dx dy]]
     (let [[zx zy] (puzzle 0)
	   move-to (map + [dx dy] [zx zy])]
       (when (legal move-to)
         [ move-to (vec-swap (puzzle 1) [zx zy] move-to)])))

(defn generate-moves [puzzle]
  (set (remove nil? (map #(move puzzle %1) moves))))

(defn randomize [puzzle move-generator n]
  (nth (iterate (fn [p] (rand-nth (into [] (move-generator p)))) puzzle) n))

(defn simple-distance [state goal]
  (cond
   (not (and (vector? state)(vector? goal))) (if (= state goal) 0 1)
   true  (apply + (map simple-distance state goal))))

(defn simple-distance-heuristic [state goal]
  (/ (simple-distance (second state)(second goal))
     2))

(A* (randomize puzzle generate-moves 10) puzzle generate-moves = simple-distance-heuristic)









  

	
       
       
	   