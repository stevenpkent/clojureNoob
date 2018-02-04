(ns clojure-noob.core
  (:gen-class))

(require '[clojure.string :as str])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example of rest parameter &
;; call (codger "person1" "person2" "personN")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn codger-communication
  "What a codger says"
  [name]
  (format "Get off my lawn %s!!!" name))

(defn codger [& whippersnappers]
  (map codger-communication whippersnappers))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructure examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn destructure-map [{:keys [lat lng miles]}]
  (println (format "latitude: %s " lat))
  (println (format "longitude: %s " lng))
  (println miles))

(defn destructure-vector-or-list [[first-arg second-arg & others]]
  (println (format "1: %s " first-arg))
  (println (format "2: %s " second-arg))
  (println (str (str/join "," others))))

(defn ellipsize [text]
  (let [[x y z] (str/split text #"\W+")]
    (str/join " " [x y z "..."])))

;; use _ for items that are not pertinent
(defn line-end2
  "Takes 2 arguments, destructures them, returns the 2nd one"
  [[_ end]]
  end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anonymous function examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #(body)
(defn indexable-words? [text length]
  (filter #(> (count %1) length) (str/split text #"\W+")))
  
;; (fn [parms] (body))
(defn indexable-words2? [text length]
  (filter (fn [w] (> (count w) length)) (str/split text #"\W+")))

;; use let to name function within your function
(defn indexable-words3? [text length]
  (let [ndx (fn [w] (> (count w) length))]
    (filter ndx (str/split text #"\W+"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loop recur - tail optimized recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn loop-example [num-iterations]
  (loop [result [] iterator num-iterations]
    (if (zero? iterator)
    result
    (recur (conj result iterator) (dec iterator)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multimethod example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti area :type)

;; call (area {:type :circle :radius 5})
(defmethod area :circle
  [{radius :radius}]
  (* 3.14159 radius radius))

;; call (area {:type :square :side 5})
(defmethod area :square
  [{side :side}]
  (* side side))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol Shape
  (area2 [shape]))

(defrecord Circle [radius])
(defrecord Square [side])

;; call (area2 (->Square 5))
;; call (area2 (->Circle 5))
(extend-protocol Shape
  Circle
  (area2 [{radius :radius}]
    (* 3.14159 radius radius))
  Square
  (area2 [{side :side}]
    (* side side)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state management examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atom
(def visitors (atom #{}))

(defn hello [name]
  (swap! visitors conj name)
  @visitors) ;; dereference to see current value

;; agent
(def error-log (agent []))

(defn log [msg]
  (send-off error-log conj msg)
  @error-log) ;; deref. note that agent state change is asynch. change may not have occurred yet

;; ref
(def r1 (ref ["steven"]))
(def r2 (ref []))

(defn yoyo []
  (dosync
   (let [v1 @r1 v2 @r2]
     (ref-set r1 v2)
     (ref-set r2 v1)
     [@r1 @r2])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn collection-type
  "Returns a keyword indicating what type of collection was passed in"
  [coll]
  (cond
    (= (type coll) clojure.lang.PersistentList) :list
    (= (type coll) clojure.lang.PersistentVector) :vector
    (= (type coll) clojure.lang.PersistentArrayMap) :map
    (= (type coll) clojure.lang.PersistentHashSet) :set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; retrieve values associated with a keyword from a collection of map data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def the-crew [{:first "steven" :last "kent"}
               {:first "babe" :last "ruth"}
               {:first "werner" :last "hertzog"}
               {:first "sam" :last "kinison"}])

(defn extract-map-value [key coll]
  (map key coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adds an index to a collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-index [coll]
  (map-indexed vector coll))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example of using ->>
;; passes result of function as the input of next function in the chain
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn euler-1
  "this function creates a range, filters it to those items that are
  evenly divisible by 3 or 5, returns the sum of those values"
  [rng]
  (->>
   (range rng)
   (filter #(or (= (mod % 3) 0) (= (mod % 5) 0)))
   (reduce +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples from Clojure for the Brave and True
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def parts [{:name "head" :size 3}
            {:name "mouth" :size 1}
            {:name "nose" :size 1}
            {:name "neck" :size 2}
            {:name "chest" :size 10}
            {:name "back" :size 10}
            {:name "abdomen" :size 6}
            {:name "left-eye" :size 1}
            {:name "left-ear" :size 1}
            {:name "left-arm" :size 5}
            {:name "left-hand" :size 2}
            {:name "left-leg" :size 6}
            {:name "left-foot" :size 2}])

(defn do-match
  "if passed in part contains 'left' this function will return its 'right' counterpart.
  otherwise will just return the passed in part"
  [part]
  {:name (str/replace (:name part) #"^left-" "right-") :size (:size part)})

(defn do-symmetry
  "will returned a finished body, meaning all left parts will have a right counterpart"
  [parts]
  (loop [p parts, result []]
    (if (empty? p) ;; base case
      result
      (let [[f & r] p] ;; destructure p into first, rest
        (recur r (into result (set [f (do-match f)])))))))

(defn which-part-hit?
  "make body symmetrical,
  make a random number that is up to the sum of sizes for the entire body,
  recurse until the accumulated size of the body parts iterated through
  is > than the random number. The body part at which the accumlator exceeds the random
  is the body part that is hit. Print out the name, size of that body part"
  [b]
  (let [body (do-symmetry b), rnd (rand (reduce + (map :size body)))]
    (loop [[f & r] body, accumulator (:size f)]
      (str "accum " accumulator)
      (if (> accumulator rnd) ;; base case
        (str "Size " (f :size) " " (f :name) " was hit.")
        (recur r (+ accumulator (:size (first r))))))))

;; pass multiple collections to map
;; call via (map unify human animal)
(def human [8.1 7.6 4.1 12.9])
(def animal [0.3 1.2 6.3 2.0])

(defn unify [h a]
  {:human h :animal a})


(def food-journal [{:month 3 :day 1 :human 4.2 :critter 3.3}
                   {:month 3 :day 2 :human 4.0 :critter 3.8}
                   {:month 4 :day 1 :human 3.7 :critter 3.9}
                   {:month 4 :day 2 :human 3.7 :critter 3.6}])




