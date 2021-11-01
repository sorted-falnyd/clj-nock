(ns clj-nock.nock2-test
  (:require
   [clojure.test :as t]
   [clj-nock.nock2 :as sut :refer [cell nock -slot nock-nock]]))

(def nn nock-nock)

(t/deftest slot
  (let [t (cell (cell 4 5) (cell 6 (cell 14 15)))]
    (t/is (= t (-slot 1 t)))
    (t/is (= (cell 4 5)
             (-slot 2 t)))
    (t/is (= (cell 6 (cell 14 15))
             (-slot 3 t)))
    (t/is (= (cell 14 15)
             (-slot 7 t))))
  (t/is (= (nn [0 6])
           (-slot 4 (nn [[[0 6] [0 7]] 123 456]))))
  (t/is (= 123 (-slot 6 (nn [[[0 6] [0 7]] 123 456])))))

(t/deftest axis
  (let [t (cell (cell 4 5) (cell 6 (cell 14 15)))]
    (t/is (= t (sut/axis 1 t)))
    (t/is (= (cell 4 5)
             (sut/axis 2 t)))
    (t/is (= (cell 6 (cell 14 15))
             (sut/axis 3 t)))
    (t/is (= (cell 14 15)
             (sut/axis 7 t))))
  (t/is (= (nn [0 6])
           (sut/axis 4 (nn [[[0 6] [0 7]] 123 456]))))
  (t/is (= 123 (sut/axis 6 (nn [[[0 6] [0 7]] 123 456])))))

(t/deftest edit
  (let [t (cell (cell 4 5) (cell 6 (cell 14 15)))]
    (t/is (= 555 (sut/edit-in 1 555 t)))
    (t/is (= (cell 555 (cell 6 (cell 14 15)))
             (sut/edit-in 2 555 t)))
    (t/is (= (cell (cell 4 5) 555)
             (sut/edit-in 3 555 t)))
    (t/is (= (cell (cell 4 5) (cell 6 555))
             (sut/edit-in 7 555 t))))
  (t/is (= (nn [[555 [0 7]] 123 456])
           (sut/edit-in 4 555 (nn [[[0 6] [0 7]] 123 456]))))
  (t/is (= (nn [[[0 6] [0 7]] 555 456])
           (sut/edit-in 6 555 (nn [[[0 6] [0 7]] 123 456])))))

(t/deftest nock-test
  (t/testing "[[[4 5] [6 14 15]] [0 7]] ==> [14 15]"
    (let [sub (nn [[4 5] [6 14 15]])
          for (nn [0 7])]
      (t/is (= (nn [14 15])
               (nock sub for)))))
  (t/testing "*[42 [1 153 218]] ==> [153 218]"
    (t/is (= (nn [153 218])
             (nock 42 (nn [1 153 218])))))
  (t/testing "*[77 [2 [1 42] [1 1 153 218]]] ==> [153 218]"
    (t/is (= (nn [153 218])
             (nock 77 (nn [2 [1 42] [1 1 153 218]])))))
  (t/testing "*[57 [0 1]] ==> 57"
    (t/is (= 57 (nock 57 (nn [0 1])))))
  (t/testing "*[[132 19] [0 3]] ==> 19"
    (t/is (= 19 (nock (nn [132 19]) (nn [0 3])))))
  (t/testing "*[57 [4 0 1]] ==> 58"
    (t/is (= 58 (nock 57 (nn [4 0 1])))))
  (t/testing "*[[132 19] [4 0 3]] ==> 20"
    (t/is (= 20 (nock (nn [132 19]) (nn [4 0 3])))))
  (t/testing "*[42 [4 0 1]] ==> 43"
    (t/is (= 43 (nock 42 (nn [4 0 1])))))
  (t/testing "*[42 [3 0 1]] ==> 1"
    (t/is (= 1 (nock 42 (nn [3 0 1])))))
  (t/testing "*[42 [[4 0 1] [3 0 1]]] ==> [43 1]"
    (t/is (= (nn [43 1])
             (nock 42 (nn [[4 0 1] [3 0 1]])))))
  (t/testing "*[[132 19] [11 37 [4 0 3]]] ==> 20"
    (t/is (= 20 (nock (nn [132 19]) (nn [11 37 [4 0 3]])))))
  (t/testing "then *[42 [7 [4 0 1] [4 0 1]]] ==> 44"
    (t/is (= 44 (nock 42 (nn [7 [4 0 1] [4 0 1]])))))
  (t/testing "*[42 [8 [4 0 1] [0 1]]] ==> [43 42]"
    (t/is (= (nn [43 42])
             (nock 42 (nn [8 [4 0 1] [0 1]])))))
  (t/testing "*[42 [8 [4 0 1] [4 0 3]]] ==> 44"
    (t/is (= 44 (nock 42 (nn [8 [4 0 1] [4 0 2]])))))
  (t/testing "*[42 [6 [1 0] [4 0 1] [1 233]]] ==> 43"
    (t/is (= 43 (nock 42 (nn [6 [1 0] [4 0 1] [1 233]])))))
  (t/testing "*[42 [6 [1 1] [4 0 1] [1 233]]] ==> 233"
    (t/is (= 233 (nock 42 (nn [6 [1 1] [4 0 1] [1 233]])))))
  (t/testing "invoke"
    (t/is
     (= 123
        (nock (nn [[[0 6] [0 7]] 123 456])
              (nn [9 4 [0 1]]))))
    (t/is
     (= 456
        (nock (nn [[[0 6] [0 7]] 123 456])
              (nn [9 5 [0 1]]))))
    (t/is
     (= (nn [123 456])
        (nock (nn [[[0 6] [0 7]] 123 456])
              (nn [[9 4 [0 1]] [9 5 [0 1]]]))))
    (t/is
     (= (nn [124 456 999])
        (nock (nn [[[[4 0 6] [0 7]] 123 456] [[1 999] 0]])
              (nn [[9 4 [0 2]] [9 5 [0 2]] [9 2 [0 3]]])))))
  (t/testing "decrement"
    (t/testing "*[42 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]] ==> 41"
      (t/is (= 41 (nock 42 (nn [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1])))))))
