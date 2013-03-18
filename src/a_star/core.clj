(ns a-star.core)
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

