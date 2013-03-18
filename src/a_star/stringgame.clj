(ns a-star.stringgame)
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

;;(A* "abcd" "dcab"  next-states = h)
