(ns clj-nock.nock1
  (:refer-clojure :exclude [atom]))

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

(def atom? int?)
(def -cell? vector?)

(defn left [cell] (nth cell 0))
(defn right [cell] (nth cell 1))
(defn cell [l r] [l r])
(def atom identity)
(def c cell)

(defn nock-nock
  "Read improper Urbit vector to proper pair"
  [v]
  (if (sequential? v)
    (if (== 2 (count v))
      (let [[a b] v]
        (cell (nock-nock a) (nock-nock b)))
      (cell (nock-nock (first v)) (nock-nock (rest v))))
    (atom v)))

(def yes (atom 0))
(def no (atom 1))
(defn loob [b] (if b yes no))

(defn ? [n] (if (-cell? n) yes no))
(defn equ? [a b] (loob (= a b)))

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

(defn axis
  "Optimized axis implementation, path from axis MSB is path, 0 is left."
  [^long a t]
  (if (== 1 a)
    t
    (loop [n (unsigned-bit-shift-right (Long/highestOneBit a) 1)
           t t]
      (if (zero? n)
        t
        (let [k (bit-and n a)
              n (unchecked-dec n)]
          (if (zero? k)
            (recur n (left t))
            (recur n (right t))))))))

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

(defn nock-cell?
  "*[a 3 b]  ?*[a b]"
  [subject formula]
  (? (nock subject formula)))

(defn nock-inc
  "*[a 4 b]  +*[a b]"
  [subject formula]
  (inc (nock subject formula)))

(defn nock-eq?
  "*[a 5 b c]  =[*[a b] *[a c]]"
  [subject formula]
  (let [f (left formula)
        g (right formula)]
    (equ? (nock subject f) (nock subject g))))

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
      (condp == op
        slot (axis formula subject) ;            0
        const formula ;                          1
        compose (let [sf (left formula)
                      ff (right formula)
                      s (nock subject sf)
                      f (nock subject ff)]
                  (recur s f)) ; 2
        cell? (nock-cell? subject formula) ;     3
        succ (nock-inc subject formula) ;        4
        eq? (nock-eq? subject formula) ;         5
        branch (let [b (left formula) ;          6
                     cd (right formula)
                     c (left cd) d (right cd)
                     p (nock subject b)]
                 (condp == p
                   yes (recur subject c)
                   no (recur subject d)))
        then=> (let [b (left formula) ;     7
                     c (right formula)]
                 (recur (nock subject b) c))
        push+= (let [a subject b (left formula) c (right formula)
                     s (nock a b)
                     s (cell s a)]
                 (recur s c)) ;     8
        invoke (let [a (left formula)
                     f (right formula)
                     core (nock subject f)
                     formula' (axis a core)]
                 (recur core formula')) ;   9
        edit (nock-edit subject formula) ;       10
        hint (let [hint (left formula)]
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
