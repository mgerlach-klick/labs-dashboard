(ns dashboard.core-test
  (:require [dashboard.core :as dc]
            [cljs.test :refer-macros [deftest is testing run-tests use-fixtures]]
            [datascript.core :as d])
  (:require-macros [dashboard.macros :as m :refer [read-json read-json-resource]]))

(defn create-test-db []
  (let [dump (js->clj (read-json-resource "testdatadump4966nov21-25.json") :keywordize-keys true)]
    (def test-db (d/create-conn dc/db-schema))
    (dc/transact-users! test-db (seq dc/+labsters+))
    (->> dump :Entries
        (d/transact! test-db))
    (prn 'DONE)))

(use-fixtures :once
  {:before (fn [] (create-test-db))})

(deftest test-read-json
  (let [obj (js->clj (read-json-resource "testdatadump4966nov21-25.json") :keywordize-keys true)]
    (is (not (nil? obj)))
    (is (= 200 (:NumEntries obj)))))

(deftest all-labsters
  (is (= 9 (count dc/+labsters+))))

(deftest bla
  (prn test-db))

(deftest billing-matrix
  (is (= (dc/calculate-billing-matrix @test-db dc/+lab-projects+ dc/labster-ids dc/labster-names)
         [["-" "Andrew" "Ani" "Carolyn" "Kat" "Ken" "Max" "Pete" "Stephen" "Yan" "SUM" "PCT"]
          ["Labs Billable" 0 0 0 0 0 1.25 0 0 0 1.25 "2.74%"]
          ["Klick Billable" 0 0 0 0 0 2.25 0 0 0 2.25 "4.93%"]
          ["Nonbillable" 0 0 0 0 0 4.516666666666667 0 0 0 4.516666666666667 "9.89%"]
          ["Administration" 0 0 0 0 0 19.483333333333334 0 0 0 19.483333333333334 "42.65%"]
          ["Experiments" 0 2.75 0 0 0 12.3 0 0 2.25 17.3 "37.87%"]
          ["Studies" 0 0 0 0 0 0.5 0 0 0 0.5 "1.09%"]
          ["Promo" 0.38333333333333336 0 0 0 0 0 0 0 0 0.38333333333333336 "0.84%"]
          ["SUM" 0.38333333333333336 2.75 0 0 0 40.3 0 0 2.25 45.68333333333333 "100% (hopefully)"]])))

(deftest labs-project-rule
  (is (= (dc/time-spent-not-billable @test-db 4966)
         4.516666666666667)))
