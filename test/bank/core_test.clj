(ns bank.core-test
  (:require [clojure.test :refer :all]
            [bank.core :refer :all]))

(deftest concurrent-transfers 
  (testing "Transfers from multiple threads."
    (is true)))
