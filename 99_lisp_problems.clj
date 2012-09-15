;from http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
(defn last [l]
  "Finds the last box of a list"
  (if (empty? (rest l))
        l
      (recur (rest l))))

(defn last-but-one [l]
  "Finds the last but one box of a list"
  (if (empty? (-> l rest rest))
       l
      (recur (rest l))))

(defn element-at [l pos]
  "Finds the element at the k-th position of a list"
  (loop [lst l index pos current-index 0]
    (if (= current-index index)
      (first lst)
      (recur (rest lst) index (inc current-index)))))

(defmacro unless [condition alternative consequence]
  `(if ~condition ~consequence ~alternative))

(defn len [l]
  "Finds the length of a list"
  (loop [lst l cnt 0]
    (unless (empty? lst)
       (recur (rest lst) (inc cnt))
       cnt)))

(defn reverse [l]
  "Reverses a list"
  (loop [lst l acum []]
    (if (empty? lst)
      acum
      (recur (rest lst) (cons (first lst) acum)))))

;determines if a list is a palindrome
(def is-palindrome? #(= % (reverse %)))

(defn flatten [l]
  "Flattens a nested list"
  (if (empty? l) []
    (let
      [[head & tail] l]
      (if (coll? head)
        (concat (flatten head) (flatten tail))
        ;else
        (concat [ head ]       (flatten tail))))))

(defn compress [l]
  "Eliminate consecutive duplicates in a list"
  (loop [lst l current-elem (first l) acum [(first l)]]
    (let [[head & tail] lst]
      (cond
        (empty? lst)
          acum
        (= head current-elem)
          (recur tail current-elem acum)
        :else ; it's a new symbol
          (recur tail head (conj acum head))))))
