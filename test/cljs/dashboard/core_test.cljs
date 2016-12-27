(ns dashboard.core-test
  (:require [dashboard.core :as dc]
            [cljs.test :refer-macros [deftest is testing run-tests use-fixtures]]
            [datascript.core :as d]
            [dashboard.queries :as q])
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

(deftest billing-matrix
  (is (= (dc/calculate-billing-matrix @test-db (dc/all-user-ids dc/+labsters+) (map :person/name dc/+labsters+))
         [["-" "Yan" "Carolyn" "Andrew" "Ken" "Ani" "Max" "Kat" "Pete" "Stephen" "SUM" "PCT"]
          ["Labs Billable" 0 0 0 0 0 1.25 0 0 0 1.25 "2.74%"]
          ["Klick Billable" 0 0 0 0 0 2.25 0 0 0 2.25 "4.93%"]
          ["Nonbillable" 0 0 0 0 0 4.516666666666667 0 0 0 4.516666666666667 "9.89%"]
          ["Administration" 0 0 0 0 0 19.483333333333334 0 0 0 19.483333333333334 "42.65%"]
          ["Experiments" 2.25 0 0 0 2.75 12.3 0 0 0 17.3 "37.87%"]
          ["Studies" 0 0 0 0 0 0.5 0 0 0 0.5 "1.09%"]
          ["Promo" 0 0 0.38333333333333336 0 0 0 0 0 0 0.38333333333333336 "0.84%"]
          ["SUM" 2.25 0 0.38333333333333336 0 2.75 40.3 0 0 0 45.68333333333333 "100% (hopefully)"]]
         )))

(deftest labs-project-rule
  (is (= (q/time-spent-not-billable @test-db 4966)
         4.516666666666667)))

(deftest add-section-items-to-user
  (is
   (=
    (dc/add-section-items-to-user @test-db
                                  dc/+dashboard-pages+
                                  {:person/userid 5959
                                   :person/name "Yan"})
    {:dashboard.core/klick-billable {:display-name "Klick Billable", :value 0, :formatted-value "-"},
     :dashboard.core/labs-billable {:display-name "Labs Billable", :value 0, :formatted-value "-"},
     :dashboard.core/promo {:display-name "Promotion", :value 0, :formatted-value "-"},
     :dashboard.core/admin {:display-name "Administration", :value 0, :formatted-value "-"},
     :person/name "Yan",
     :dashboard.core/non-billable {:display-name "Non-Billable", :value 0, :formatted-value "-"},
     :dashboard.core/studies {:display-name "Studies", :value 0, :formatted-value "-"},
     :dashboard.core/experiments {:display-name "Experiments", :value 2.25, :formatted-value "2.25"},
     :person/userid 5959}
    )))

(deftest calculate-section-sums
  (is
   (=
    (dc/calculate-section-sums dc/+dashboard-pages+ [{:dashboard.core/klick-billable {:display-name "Klick Billable", :value 0, :formatted-value "-"},
                                                      :dashboard.core/labs-billable {:display-name "Labs Billable", :value 0, :formatted-value "-"},
                                                      :dashboard.core/promo {:display-name "Promotion", :value 0, :formatted-value "-"},
                                                      :dashboard.core/admin {:display-name "Administration", :value 0, :formatted-value "-"},
                                                      :person/name "Yan",
                                                      :dashboard.core/non-billable {:display-name "Non-Billable", :value 0, :formatted-value "-"},
                                                      :dashboard.core/studies {:display-name "Studies", :value 0, :formatted-value "-"},
                                                      :dashboard.core/experiments {:display-name "Experiments", :value 2.25, :formatted-value "2.25"},
                                                      :person/userid 5959}
                                                     {:dashboard.core/klick-billable {:display-name "Klick Billable", :value 0, :formatted-value "-"},
                                                      :dashboard.core/labs-billable {:display-name "Labs Billable", :value 0, :formatted-value "-"},
                                                      :dashboard.core/promo {:display-name "Promotion", :value 0, :formatted-value "-"},
                                                      :dashboard.core/admin {:display-name "Administration", :value 1, :formatted-value "1"},
                                                      :person/name "Max",
                                                      :dashboard.core/non-billable {:display-name "Non-Billable", :value 0, :formatted-value "-"},
                                                      :dashboard.core/studies {:display-name "Studies", :value 0, :formatted-value "-"},
                                                      :dashboard.core/experiments {:display-name "Experiments", :value 2.25, :formatted-value "2.25"},
                                                      :person/userid 4966}
                                                     ])
    {:dashboard.core/klick-billable 0,
     :dashboard.core/labs-billable 0,
     :dashboard.core/promo 0,
     :dashboard.core/admin 1,
     :dashboard.core/non-billable 0,
     :dashboard.core/studies 0,
     :dashboard.core/experiments 4.5}
    )))

(deftest calculate-user-percentages
  (is
   (=
    (dc/calculate-user-percentages {:dashboard.core/klick-billable 0,
                                    :dashboard.core/admin 1,
                                    :dashboard.core/experiments 4.5}
                                   {:dashboard.core/klick-billable {:display-name "Klick Billable", :value 0, :formatted-value "-"},
                                    :dashboard.core/admin {:display-name "Administration", :value 1, :formatted-value "1"},
                                    :dashboard.core/experiments {:display-name "Experiments", :value 2.25, :formatted-value "2.25"},
                                    :person/name "Max",
                                    :person/userid 4966})

    {:dashboard.core/klick-billable {:display-name "Klick Billable", :value 0, :formatted-value "-", :percentage 0, :formatted-percentage "-"},
     :dashboard.core/admin {:display-name "Administration", :value 1, :formatted-value "1", :percentage 100, :formatted-percentage "100%"},
     :dashboard.core/experiments {:display-name "Experiments", :value 2.25, :formatted-value "2.25", :percentage 50, :formatted-percentage "50%"},
     :person/name "Max",
     :person/userid 4966})))

(deftest calculate-overall-percentages
  (is (=
       (dc/calculate-overall-percentages {:dashboard.core/klick-billable 0,
                                          :dashboard.core/labs-billable 0,
                                          :dashboard.core/promo 0,
                                          :dashboard.core/admin 1,
                                          :dashboard.core/non-billable 0,
                                          :dashboard.core/studies 0,
                                          :dashboard.core/experiments 4.5})
       {:dashboard.core/klick-billable "-",
        :dashboard.core/labs-billable "-",
        :dashboard.core/promo "-",
        :dashboard.core/admin "18.18%",
        :dashboard.core/non-billable "-",
        :dashboard.core/studies "-",
        :dashboard.core/experiments "81.82%"})))


(dc/ffind
 (dc/calculate-all @test-db dc/+dashboard-pages+ dc/+labsters+)
 :display-name "SUM")
