(ns clojure-basics.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Exercises from https://exercises.clojure.camp/
;; First Steps
(defn largest [x] (reduce max x))

(largest [1 2 3 4])

(def letter-scores {\a 1 \b 3 \c 3 \d 2 \e 1 \f 4 \g 2 \h 4})

(defn score [word]
  (->> word
      (seq)
      (map letter-scores)
      (apply +))
)

(score "abcd")

(defn count-if [predicate?, x]
  (->> x
       (filter predicate?)
       (count)))

(count-if even? [1 2 3 4 5])

(def planet2planet {:earth {:saturn 1/30} :mars {:mercury 78/10}})

(defn convert-space-age [age from to]
  (* age ( (planet2planet from) to )))

(convert-space-age 10 :mars :mercury)

;; (def lookup
;;   {:mercury 0.2408467
;;    :venus 0.61519726
;;    :earth 1
;;    :mars 1.8808158
;;    :jupiter 11.862615
;;    :saturn 29.447498
;;    :uranus 84.016846
;;    :neptune 164.79132})

;; (defn convert-space-age
;;   [age source-planet target-planet]
;;   (int (* age (/ (lookup source-planet) (lookup target-planet)))))

;; tic tac toe game state, first variable represent whose turn went first, should just use a map honestly
(def board [true :2 :3 :6 :7 :9])
(defn moves-played [board] (- (count board) 1))
(defn whose-turn? [board] (case (board 0)
                            true (if (even? (moves-played board)) :x :o)
                            false (if (even? (moves-played board)) :o :x)))
(whose-turn? board)

;;Exploring Functions

(defn get-and-set [key value coll]
  (vector (get coll key :missing) (assoc coll key val)))

(get-and-set 1 2 [1 2 3 4])
(get-and-set 1 2 {1 2 3 4})

(def path "./src/clojure_basics/toy.txt")

(defn get-filename [path]
  (last (clojure.string/split path #"/")))

(get-filename path)

(defn create-revfilename [name]
  (str "rev-" name))

(defn modify-path [path new-name] (clojure.string/join "/" (conj (pop (clojure.string/split path #"/")) new-name )))

( modify-path "hello/world/file" "newasdlkfj-file" )

(defn slurp-and-spit [path]
  ( let [filename (get-filename path)
          revfilename (create-revfilename filename)
          newpath (modify-path path revfilename)]
        ( ->> (slurp path)
              (clojure.string/reverse)
              (spit newpath)))
)
(slurp-and-spit "./src/clojure_basics/toy.txt")

(def people {:john {:hair "long" :height "tall" :age 10}})
(defn add-info [coll id keyword val] (assoc-in coll [id keyword] val))
(add-info people :john :tummy "large")

(defn update-info [info id number]
  (update-in info [id]
             (fn [{:keys [name points]}]
               {:name (clojure.string/upper-case name) :points (+ points number)})))
;; (defn update-info
;;   [info id points]
;;   (-> info
;;       (update-in [:people id :name] string/upper-case)
;;       (update-in [:people id :points] + points)))

(def people {:john {:name "james" :points 1}})
(update-info people :john 4)

(def strings ["beep" "abc" "apple" "things" "toffee" "camp" "data" "clojure"])
(defn categorize [coll] (group-by count coll))
(def categorized ( categorize strings ))
(categorized 5)

(defn activate [info]
  (-> info
      (dissoc :disabled?)
      (assoc :activated "now")
      (update :points inc)))

(def info {:name "jamse" :disabled? true :points 0})
(activate info)

(defn partial-map [fn1 fn2 values]
  (map (fn [x] (if (fn2 x) (fn1 x) x)) values))

(partial-map (fn [x] (* x 10))
             even?
             [1 2 3 4])


;; this returns a transducer, you can't use transducers like any other function
(defn partial-map
  [fn1 fn2]
  (map (fn [x] (if (fn2 x) (fn1 x) x))))

(def inner (fn [x] (if (even? x) (* x 10) x)))
(inner 1)

(def partial ( partial-map (fn [x] (* x 10)) even? ))
(type partial)
;; this retusns something strange
(type (  partial [1 2 3 4]  ))
;; this returns the actual sequence
( sequence partial [1 2 3 4 5 6] )

(defn sanitize
  [phone-number]
  (->> phone-number
       (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
       (take-last 10)
       (apply str)
       ))
( sanitize "123,lj3214lkj1324")

(doseq [x [-1 0 1]
        y [1 2 3]]
  (prn (* x y)))

(defn sum [values] (apply + values))
(sum [1 2 3 4])

(defn sum
  [values]
  (let [total (atom 0)]
    (doseq [x values]
      (swap! total + x))
    (deref total)))
(sum [1 2 3 4])

(defn sum [values] (reduce + values))
(sum [1 2 3 4])

(defn interests-info
  [person1 person2]
  {:all (clojure.set/union person1 person2)
   :shared (clojure.set/intersection person1 person2)
   :unique-a (clojure.set/difference person1 person2)
   :unique-b (clojure.set/difference person2 person1)}
  )

(interests-info #{"reading" "lifting" "programming" "boardgames"}
                #{"sculpting" "programming" "movies" "boardgames"})

(defn insert-at
  [coll index val]
  (into (conj ( subvec coll 0 index ) val) (subvec coll index)))
(insert-at [1 3 4] 1 2)

(defn extract-info
  [info-str]
  (let [[_ lastn firstn title] (re-matches #"([^,]+), ([^(]+) \(([^)]+)\)"
                                           info-str)]
    {:first firstn
     :last lastn
     :title title}))

(extract-info "World, Hello (Cool Person)")

(defn find-phone-numbers
  [text]
  (->> text
       (re-seq #"(?:\+?\d[- ])?(?:\(\d{3}\)|\d{3})[- ]\d{3}[- ]\d{4}")))

(find-phone-numbers
 "Here's a bunch of text call (416) 333 4444 and +1 647 123 4578 gurf 1- 416)123-4567 some more garbage 123_123_4567 but don't call from 1-800-888-9999 and why not? here's 905 777-1111")

(defn dissoc-at
  [coll index]
  (into ( subvec coll 0 index ) (subvec coll (inc index))))

(dissoc-at [1 2 3 4] 1)

;;version 1 with inner function
(defn sum [values]
  ((fn [acc vals]
    (if (empty? vals)
      acc
      (recur (+ acc (first vals)) (rest vals)))) 0 values))
(  sum [1 2 3 4]  )

;;version 2 with loop
(defn sum
  [values]
  (loop [acc 0 vals values]
    (if (empty? vals)
      acc
      (recur (+ acc (first vals)) (rest vals))))
  )
(  sum [1 2 3 4]  )

;;version 3 with multiple function calls
(defn sum
  ([values] (sum 0 values))
  ([acc values]
   (if (empty? values)
     acc
     (recur (+ acc (first values)) (rest values))))
  )
(  sum [1 2 3 4]  )

(defn fake-pmap [fn coll]
  (doall ( map deref (map #( future (fn %) ) coll) )))

(time (  fake-pmap #(do (Thread/sleep 5000)(inc %)) [1 2 3 4]  ))
(time (doall (   map #(do (Thread/sleep 5000)(inc %)) [1 2 3 4]   )))
;;without doall, time will evaluate the time it takes to create the lazy sequence.

(defn my-pmap
  [f coll]
  (->> coll
       (map (fn [x]
              (future (f x))))
       (map deref)))

;; this is evaluated very quickly because we are only creating a lazy sequence
(time (  my-pmap #(do (Thread/sleep 5000)(inc %)) [1 2 3 4]  ))


(defn sanitize
  [phone-number]
  (clojure.string/replace phone-number #"[^0-9]" ""))

( sanitize "123oiu123ljkj" )

(defn add-element [x coll] (conj coll x))
(add-element :a '(2 3 4))

(defn increasing-repeat
  [coll]
  (->> coll
       (map-indexed (fn [i v] (repeat (inc i) v)))
       (flatten))
  )
(increasing-repeat [:a :b :c :d :e])

;; woah, what the hell, this is really cool
(defn join-maps [list1 list2] (map merge list1 list2))
(join-maps [{:a 1} {:a 2} {:a 3}] [{:b 2} {:b 3} {:b 4}])

;;that's cool!
(map + [1 2 3 20] [10 15 16 20] [4 10 12])

;;Okay, i suppose I must learn a little java now
(import '(java.security MessageDigest))
(defn sha256
  [s]
  (-> (doto (MessageDigest/getInstance "SHA-256")
            (.update (.getBytes s "UTF-8")))
      (.digest)
      (->> (map (fn [b]
                  (format "%02x" b)))
           (apply str))))
(sha256 "hello world")
;;nevermind, fuck it. I don't wanna. But it's cool that it's there.

(defn long-lengths
  [strings]
  (map count
       (filter (fn [string]
                 (< 5 (count string)))
               strings)))

(defn long-lengths-refactored
  [strings]
  (reduce (fn [a b]
            (if (< 5 (count b))
              (conj a (count b))
              a))
          [] strings))
(long-lengths-refactored ["foo" "aoeusnth" "q" "aoeus" "abcdef" "1234567"] )

;;More Practice

;; What is this? Woah. Iterate is so cool:
;; Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects
(mapv + [1 2 3] (iterate inc 1))

(defn transpose
  [matrix]
  (apply map vector matrix))

(transpose [[1 4] [2 5] [3 6]])

(defn count-nucleotides
  [tide]
  (-> tide
      (clojure.string/split #"")
      (frequencies)))
(count-nucleotides
 "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

(defn process-person
  [arg]
  (let [name (first arg)
        info (second arg)
        age (:age info)
        interests (:interests info)
        interest-1 (first interests)
        other-interests (rest interests)]
    {:description (str name
                       " is "
                       age
                       " years old.\n"
                       "They like "
                       interest-1
                       " and "
                       (count other-interests)
                       " other things.")
     :name name
     :info info}))

(defn process-person1
  [[name
    {:keys [age]
     [interest-1 & other-interests] :interests
     :as info}]]
  {:description (str name
                     " is "
                     age
                     " years old.\n"
                     "They like "
                     interest-1
                     " and "
                     (count other-interests)
                     " other things.")
   :name name
   :info info})

(defn process-person2
  "Alternate solution that split things up a bit more"
  [[name
    {:keys [age interests]
     :as info}]]
  (let [[interest-1 & other-interests] interests]
    {:description (str name
                       " is "
                       age
                       " years old.\n"
                       "They like "
                       interest-1
                       " and "
                       (count other-interests)
                       " other things.")
     :name name
     :info info}))
(process-person2 ["James"
                 {:age 30
                  :interests ["highland games" "code" "reading"]}])

(defn dot
  [vec1 vec2]
  (->> (map * vec1 vec2)
       (reduce + 0)))

(dot [1 0] [3 4])

(defn fizzbuzz
  []
  (doseq [x (range 1 100)]
    (cond
      (and (= 0 (mod x 5)) (= 0 (mod x 5))) (println "fizzbuzz")
      (= 0 (mod x 5)) (println "buzz")
      (= 0 (mod x 3)) (println "fizz")
      :else (println x))))
(fizzbuzz)

;;alternatively, it would have been smarter to implement div? like this:
(defn div? [n factor] (= 0 (mod n factor)))

(defn sum-to
  [N]
  (apply + (range 1 (inc N))))

(sum-to 10)

(sum-to
 [N])
