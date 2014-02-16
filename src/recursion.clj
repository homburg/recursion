(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (let [head (first coll)
        tail (rest coll)]
    (if (empty? tail)
      head
      (my-last tail))))

(defn max-element [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
     (empty? a-seq) nil
     (singleton? a-seq) head
     :else (max head
                (max-element tail)))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (<= count-1 count-2)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? head)
        (cons head (my-filter pred? tail))
        (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (and (not (empty? a-seq))
         (or (= elem head)
             (sequence-contains? elem tail)))))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? head) (cons head (my-take-while pred? tail))
     :else '())))

(defn my-drop-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? head) (my-drop-while pred? tail)
     :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [seqs [a-seq b-seq]]
    (if (some empty? seqs)
      (apply = (map empty? seqs))
      (and (apply = (map first seqs))
           (apply seq= (map rest seqs))))))

(defn my-map [f seq-1 seq-2]
  (let [seqs [seq-1 seq-2]]
    (if (some empty? seqs)
      '()
      (cons (apply f (map first seqs))
            (apply (partial my-map f) (map rest seqs))))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n))
            (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to) '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate [a-seq]
  (let [shifted (concat (rest a-seq) (take 1 a-seq))]
    (cons shifted (lazy-seq (rotate shifted)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (take (count a-seq) (rotate a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

