(ns clj-nock.nock5-test
  (:require
   [clojure.test :as t]
   [clj-nock.nock5 :as sut :refer [atom nock -slot nock-nock]]))

(def nn nock-nock)

(t/deftest slot
  (let [t (nn [[4 5] [6 [14 15]]])]
    (t/is (= t (-slot 1 t)))
    (t/is (= (nn [4 5])
             (-slot 2 t)))
    (t/is (= (nn [6 14 15])
             (-slot 3 t)))
    (t/is (= (nn [14 15])
             (-slot 7 t))))
  (t/is (= (nn [0 6])
           (-slot 4 (nn [[[0 6] [0 7]] 123 456]))))
  (t/is (= (sut/atom 123) (-slot 6 (nn [[[0 6] [0 7]] 123 456])))))

(t/deftest axis
  (let [t (nn [[4 5] [6 [14 15]]])]
    (t/is (= t (sut/axis (atom 1) t)))
    (t/is (= (nn [4 5])
             (sut/axis (atom 2) t)))
    (t/is (= (nn [6 14 15])
             (sut/axis (atom 3) t)))
    (t/is (= (nn [14 15])
             (sut/axis (atom 7) t))))
  (t/is (= (nn [0 6])
           (sut/axis (atom 4) (nn [[[0 6] [0 7]] 123 456]))))
  (t/is (= (sut/atom 123) (sut/axis (atom 6) (nn [[[0 6] [0 7]] 123 456])))))

(t/deftest edit
  (let [t (nn [[4 5] [6 [14 15]]])]
    (t/is (= 555 (sut/edit-in 1 555 t)))
    (t/is (= (nn [555 [6 [14 15]]])
             (sut/edit-in 2 (sut/atom 555) t)))
    (t/is (= (nn [[4 5] 555])
             (sut/edit-in 3 (sut/atom 555) t)))
    (t/is (= (nn [[4 5] [6 555]])
             (sut/edit-in 7 (sut/atom 555) t))))
  (t/is (= (nn [[555 [0 7]] 123 456])
           (sut/edit-in 4 (sut/atom 555) (nn [[[0 6] [0 7]] 123 456]))))
  (t/is (= (nn [[[0 6] [0 7]] 555 456])
           (sut/edit-in 6 (sut/atom 555) (nn [[[0 6] [0 7]] 123 456])))))

(t/deftest nock-test
  (t/testing "[[[4 5] [6 14 15]] [0 7]] ==> [14 15]"
    (t/is (= (nn [14 15])
             (nock (nn [[4 5] [6 14 15]]) (nn [0 7])))))
  (t/testing "*[42 [1 153 218]] ==> [153 218]"
    (t/is (= (nn [153 218])
             (nock 42 (nn [1 153 218])))))
  (t/testing "*[77 [2 [1 42] [1 1 153 218]]] ==> [153 218]"
    (t/is (= (nn [153 218])
             (nock 77 (nn [2 [1 42] [1 1 153 218]])))))
  (t/testing "*[57 [0 1]] ==> 57"
    (t/is (= (sut/atom 57) (nock (sut/atom 57) (nn [0 1])))))
  (t/testing "*[[132 19] [0 3]] ==> 19"
    (t/is (= (sut/atom 19) (nock (nn [132 19]) (nn [0 3])))))
  (t/testing "*[57 [4 0 1]] ==> 58"
    (t/is (= (sut/atom 58) (nock (sut/atom 57) (nn [4 0 1])))))
  (t/testing "*[[132 19] [4 0 3]] ==> 20"
    (t/is (= (sut/atom 20) (nock (nn [132 19]) (nn [4 0 3])))))
  (t/testing "*[42 [4 0 1]] ==> 43"
    (t/is (= (sut/atom 43) (nock (sut/atom 42) (nn [4 0 1])))))
  (t/testing "*[42 [3 0 1]] ==> 1"
    (t/is (= (sut/atom 1) (nock (sut/atom 42) (nn [3 0 1])))))
  (t/testing "*[42 [[4 0 1] [3 0 1]]] ==> [43 1]"
    (t/is (= (nn [43 1])
             (nock (sut/atom 42) (nn [[4 0 1] [3 0 1]])))))
  (t/testing "*[[132 19] [11 37 [4 0 3]]] ==> 20"
    (t/is (= (sut/atom 20) (nock (nn [132 19]) (nn [11 37 [4 0 3]])))))
  (t/testing "then *[42 [7 [4 0 1] [4 0 1]]] ==> 44"
    (t/is (= (sut/atom 44) (nock (sut/atom 42) (nn [7 [4 0 1] [4 0 1]])))))
  (t/testing "*[42 [8 [4 0 1] [0 1]]] ==> [43 42]"
    (t/is (= (nn [43 42])
             (nock (sut/atom 42) (nn [8 [4 0 1] [0 1]])))))
  (t/testing "*[42 [8 [4 0 1] [4 0 3]]] ==> 44"
    (t/is (= (sut/atom 44) (nock (sut/atom 42) (nn [8 [4 0 1] [4 0 2]])))))
  (t/testing "*[42 [6 [1 0] [4 0 1] [1 233]]] ==> 43"
    (t/is (= (sut/atom 43) (nock (sut/atom 42) (nn [6 [1 0] [4 0 1] [1 233]])))))
  (t/testing "*[42 [6 [1 1] [4 0 1] [1 233]]] ==> 233"
    (t/is (= (sut/atom 233) (nock (sut/atom 42) (nn [6 [1 1] [4 0 1] [1 233]])))))
  (t/testing "invoke"
    (t/is
     (= (sut/atom 123)
        (nock (nn [[[0 6] [0 7]] 123 456])
              (nn [9 4 [0 1]]))))
    (t/is
     (= (sut/atom 456)
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
      (t/is (= (sut/atom 41) (nock (sut/atom 42) (nn [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1])))))))
