;from http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html

;the following functions look weird because they include tests
;instead of
;(defn name [args]
;   body)
;
;I do
;
;(defn 
;  METADATA 
;  name [args] 
;  body)
;
;to load the file, just do
;(load-file path/to/here)
;
;to test any of the functions, do
;
;(test #'<function-name>)
;
;or
;
;(test (var <function-name>))
;
;to test any of the functions, just 

;for some reason, in tests, we can't do
;#((assert :assertion))
;because it gives a nullPointerException
;but I hate writing
;(fn [] (assert :assertion))
;so I created this macro
(defmacro go [body]
  `(fn [] ~body))

(defn
  ^{:doc "Finds the last box of a list"
    :test (go (assert (= '(4) (last [1 2 3 4]))))}
  last [l]
  (if (empty? (rest l))
        l
      (recur (rest l))))

(defn
  ^{:doc "Finds the last-but-one box of a list"
    :test (go 
            (assert (= 
                      '(3 4) 
                      (last-but-one [1 2 3 4]))))}
  last-but-one [l]
  (if (empty? (-> l rest rest))
       l
      (recur (rest l))))

(defn 
  ^{:doc "Finds the element at the k-th position of a list"
    :test (go (assert (= 3 (element-at [1 2 3 4] 2))))}

  element-at [l pos]
  (loop [lst l index pos current-index 0]
    (if (= current-index index)
      (first lst)
      (recur (rest lst) index (inc current-index)))))

(defmacro unless [condition alternative consequence]
  `(if ~condition ~consequence ~alternative))

(defn
  ^{:doc "Finds the length of a list"
    :test (go (assert (= 3 (len [1 2 3]))))}
  len [l]
  (loop [lst l cnt 0]
    (unless (empty? lst)
       (recur (rest lst) (inc cnt))
       cnt)))

(defn
  ^{:doc "Reverses a list"
    :test (go (assert (=
                        '(4 3 2 1)
                        (reverse [1 2 3 4]))))}
  reverse [l]
  (loop [lst l acum []]
    (if (empty? lst)
      acum
      (recur (rest lst) (cons (first lst) acum)))))

;determines if a list is a palindrome
;doesn't deserve a test!
(def is-palindrome? #(= % (reverse %)))

(defn
  ^{:doc "Flattens a nested list"
    :test (go (assert (= '(:a :b :c :d :e)
                         (flatten '(:a (:b (:c :d) :e))))))}
  flatten [l]
  (if (empty? l) []
    (let
      [[head & tail] l]
      (if (coll? head)
        (concat (flatten head) (flatten tail))
        ;else
        (concat [ head ]       (flatten tail))))))

(defn
  ^{:doc "Eliminate consecutive duplicates in a list"
    :test (go (assert (= '(:a :b :c)
                         (compress [:a :a :b :c :c :c]))))}
  compress [l]
  (loop [lst l current-elem (first l) acum [(first l)]]
    (let [[head & tail] lst]
      (cond
        (empty? lst)
          acum
        (= head current-elem)
          (recur tail current-elem acum)
        :else ; it's a new symbol
          (recur tail head (conj acum head))))))
