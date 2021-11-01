(ns clj-nock.nock5
  (:refer-clojure :exclude [atom])
  (:import
   (clj_nock.nock Atom Cell Util)))

(set! *warn-on-reflection* true)

;;; Nock 4K interpreter

(def slot 0)
(def const 1)
(def compose 2)
(def cell? 3)
(def succ 4)
(def eq? 5)
(def branch 6)
(def then=> 7)
(def push+= 8)
(def invoke 9)
(def edit 10)
(def hint 11)
(def scry 12)

(definline atom? [c] `(Util/isAtom ~c))
(definline -cell? [c] `(Util/isCell ~c))

(definline left [cell] `(.left ~(with-meta cell {:tag "Cell"})))
(definline right [cell] `(.right ~(with-meta cell {:tag "Cell"})))
(definline cell [l r] `(new Cell ~l ~r))
(defn atom [v] (new Atom v))

(defn nock-nock
  "Read improper Urbit vector to proper pair"
  [v]
  (if (sequential? v)
    (if (== 2 (count v))
      (let [[a b] v]
        (cell (nock-nock a) (nock-nock b)))
      (cell (nock-nock (first v)) (nock-nock (rest v))))
    (atom v)))

(definline loob [b] `(Util/loob ~b))

(definline ? [n] `(Util/isCellP ~n))

(definline equ? [a b] `(Util/eq ~a ~b))

(defn -slot
  "Formally correct slot implementation, stack based."
  [slot tree]
  (condp == slot
    1 tree
    2 (left tree)
    3 (right tree)
    (if (even? slot)
      (recur 2 (-slot (/ slot 2) tree))
      (recur 3 (-slot (/ (dec slot) 2) tree)))))

#_
(defn axis
  "Optimized axis implementation, path from axis MSB is path, 0 is left."
  [^long a ^Cell t]
  (if (== 1 a)
    t
    (loop [n (unsigned-bit-shift-right (Long/highestOneBit a) 1)
           t t]
      (if (zero? n)
        t
        (recur (unchecked-dec n)
               (if (zero? (bit-and n a))
                 (left t)
                 (right t)))))))
(definline axis
  [a c]
  `(Util/axis ~a ~c))

(defn- -edit-in
  [tree ^long where with ^long n]
  (if (zero? n)
    with
    (let [k (bit-and where n)
          n (unchecked-dec n)]
      (if (zero? k)
        (cell (-edit-in (left tree) where with n) (right tree))
        (cell (left tree) (-edit-in (right tree) where with n))))))

(defn edit-in
  [^long where with tree]
  (let [n (unsigned-bit-shift-right (Long/highestOneBit where) 1)]
    (-edit-in tree where with n)))

(declare nock)

(definline nock-cell?
  "*[a 3 b]  ?*[a b]"
  [subject formula]
  `(? (nock ~subject ~formula)))

(definline nock-inc
  "*[a 4 b]  +*[a b]"
  [subject formula]
  `(let [~'a (nock ~subject ~formula)]
     (.inc ~(with-meta 'a {:tag "Atom"}))))

(definline nock-eq?
  "*[a 5 b c]  =[*[a b] *[a c]]"
  [subject formula]
  `(let [s# ~subject
         f# ~formula]
     (equ? (nock s# (left f#))
           (nock s# (right f#)))))

(defn nock-edit
  "*[a 10 [b c] d]     #[b *[a c] *[a d]]"
  [subject formula]
  (let [a subject
        bc (left formula)
        b (left bc) c (right bc)
        d (right formula)]
    (edit-in b (nock a c) (nock a d))))

(defn nock
  [subject formula]
  (let [op (left formula)
        formula (right formula)]
    (if (-cell? op)
      (cell (nock subject op) (nock subject formula))
      (case (.value ^Atom op)
        0 (axis ^Atom formula subject)
        1 formula
        2 (recur (nock subject (left formula))
                 (nock subject (right formula)))
        3 (nock-cell? subject formula)
        4 (nock-inc subject formula)
        5 (nock-eq? subject formula)
        6 (let [cd (right formula)]
            (recur subject
                   (if (.equals Atom/YES (nock subject (left formula)))
                     (left cd)
                     (right cd))))
        7 (recur (nock subject (left formula)) (right formula))
        8 (recur (cell (nock subject (left formula)) subject) (right formula))
        9 (let [core (nock subject (right formula))]
            (recur core (axis ^Atom (left formula) core)))
        10 (nock-edit subject formula)
        11 (let [hint (left formula)]
             (if (-cell? hint)
               ;; *[a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
               (recur
                (cell
                 (nock subject (right hint))
                 (nock subject (right formula)))
                ;; *[a 11 b c]  *[a c]
                (cell slot cell?))
               (recur subject (right formula))))))))

(comment
  ;; ~zod:dojo> .*(77 [2 [1 42] [1 1 153 218]])
  (nock
   77
   [2 [[1 42] [1 [1 [153 218]]]] ])
  (nock 57 [0 1])
  (nock [132 19] [0 3]) ;; 19
  (nock [132 19] [4 [0 3]]) ;; 20
  ;;; .*(42 [[4 0 1] [3 0 1]])
  (nock 42 [[4 [0 1]] [3 [0 1]]]) ;; [43 1]

  (nock 42 [4 [0 1]])

  (nock 42 [5 [0 7] [4 0 6]])


  ;;; .*([132 19] [11 37 [4 0 3]])
  (nock [132 19]
        [11 [37 [4 [0 3]]]])
  (def v' [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1])
  (def v [8
          [1 0]
          [8
           [1
            [6
             [5 [0 7] [4 0 6]]
             [0 6]
             [9 2 [[0 2] [4 0 6] [0 7]]]]]
           [9 2 0 1]]])
  (= (nock-nock v) (nock-nock v'))
  (def decr (nock-nock v))
  (nock 42 decr)
  (nock 42 [[1 0] [0 1]])
  (def q (nock-nock [7 [1 7 1 [0 2] [[1 1] 0 1] 0 7] [0 2] [[1 1] 0 1] 0 7]))
  (= (nock 123 q) q)
  )

(def decr
  (nock-nock
   [8
    [1 0]
    [8
     [1
      [6
       [5 [0 7] [4 0 6]]
       [0 6]
       [9 2 [[0 2] [4 0 6] [0 7]]]]]
     [9 2 0 1]]]))
