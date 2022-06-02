(ns odysseus.utils-test
  (:require [clojure.test :refer :all]
            [org.clojars.prozion.odysseus.utils :refer :all]))

(deftest utils-unit-tests
  (testing "falsy and truthy"
    (is (true? (truthy? 0)))
    (is (true? (truthy? [])))
    (is (false? (truthy? nil)))
    (is (false? (truthy? false)))
    (is (false? (falsy? 0)))
    (is (false? (falsy? [])))
    (is (true? (falsy? nil))))
    (is (true? (falsy? false))))

  (testing "check ormap"
    (is (falsy? (ormap #(> % 10) [0 2 4 9])))
    (is (truthy? (ormap (fn [x y] (> y x)) '(1 2 10) '(3 4 5)))))
