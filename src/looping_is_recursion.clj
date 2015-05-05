(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [prod x]
                 (if (zero? x)
                   prod
                   (recur (* prod base) (dec x))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [lst sq]
                 (if (empty? sq)
                   lst
                   (recur (first sq) (rest sq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (and (empty? seq1) (not (empty? seq2))) false
    (and (not (empty? seq1)) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         s a-seq]
    (if (empty? s)
      nil
      (let [fst (first s)]
        (if (pred fst)
          idx
          (recur (inc idx) (rest s)))))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         s a-seq]
    (if (empty? s)
      (if (not (zero? count))
        (/ sum count)
        0)
      (recur (+ sum (first s))
             (inc count)
             (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [p #{}
         s a-seq]
    (if (empty? s)
      p
      (recur (toggle p (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [fn1 0 fn 1 n n]
    (if (zero? n)
      fn1
      (recur fn (+ fn1 fn) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [r []
         t #{}
         s a-seq]
    (if (empty? s)
      r
      (let [fst (first s)]
        (if (contains? t fst)
          r
          (recur (conj r fst) (conj t fst) (rest s)))))))

