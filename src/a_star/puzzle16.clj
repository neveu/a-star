(ns a-star.puzzle16)


;;;;;;;;;;;;;;;
;; 16 puzzle ;;
;;;;;;;;;;;;;;;
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

;; (A* (randomize puzzle generate-moves 10) puzzle generate-moves = simple-distance-heuristic)

  