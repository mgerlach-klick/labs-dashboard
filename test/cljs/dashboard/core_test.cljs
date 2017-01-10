(ns dashboard.core-test
  (:require [dashboard.core :as dc]
            [cljs.test :refer-macros [deftest is are testing run-tests use-fixtures]]
            [datascript.core :as d]
            [dashboard.queries :as q])
  (:require-macros [dashboard.macros :as m :refer [read-json read-json-resource]]))

(prn "loading core-test namespace")

(defn create-test-db []
  (let [dump (js->clj (read-json-resource "testdatadump4966nov21-25.json") :keywordize-keys true)]
    (def test-db (d/create-conn dc/db-schema))
    (dc/transact-users! test-db (vals dc/+labsters+))
    (->> dump :Entries
        (d/transact! test-db))
    (prn 'DONE)))

;; (def derefed-test-db @test-db)


(use-fixtures :once
  {:before (fn [] (create-test-db))})


(deftest all-labsters
  (is (= 9 (count dc/+labsters+))))

(deftest data-table
  (let [dt (dc/calculate-data-table @test-db dc/+dashboard-sections+ dc/+labsters+)
        max (get dt :labster/max)]
    (is (= (-> max (dissoc :on-click))
     {:dashboard.core/klick-billable 2.25,
      :dashboard.core/labs-billable 1.25,
      :display-index 4,
      :dashboard.core/promo 0,
      :dashboard.core/admin 19.483333333333334,
      :person/userid 4966,
      :person/name "Max",
      :dashboard.core/sum 40.3,
      :display-name "Max",
      :dashboard.core/non-billable 4.516666666666667,
      :dashboard.core/studies 0.5,
      :dashboard.core/experiments 12.3}))))

(deftest labs-project-rule
  (is (= (q/time-spent-not-billable @test-db 4966)
         4.516666666666667)))

(deftest add-section-items-to-user
  (is
   (=
    (dc/add-section-items-to-user @test-db
                                  {::labs-billable {:display-name "Labs Billable"
                                                    :calculation (fn [db uid]
                                                                   (dc/time-spent-on-tag db :labs-billable uid))}
                                   ::klick-billable {:display-name "Klick Billable"
                                                     :calculation (fn [db uid]
                                                                    (q/time-spent-on-billable-projects db uid))}}
                                  {:person/userid 5959
                                   :person/name "Yan"})
    {:person/userid 5959,
     :person/name "Yan",
     :dashboard.core-test/labs-billable 0,
     :dashboard.core-test/klick-billable 0}
    )))

(deftest calculate-section-sums
  (is
   (=
    (dc/calculate-section-sums  {::labs-billable {:display-name "Labs Billable"
                                                  :calculation (fn [db uid]
                                                                 (dc/time-spent-on-tag db :labs-billable uid))}
                                 ::klick-billable {:display-name "Klick Billable"
                                                   :calculation (fn [db uid]
                                                                  (q/time-spent-on-billable-projects db uid))}}
                                [{:person/userid 5959,
                                  :person/name "Yan",
                                  ::labs-billable 1,
                                  ::klick-billable 2}
                                 {:person/userid 4699,
                                  :person/name "Max",
                                  ::labs-billable 3,
                                  ::klick-billable 4}])
    {:dashboard.core-test/labs-billable 4,
     :dashboard.core-test/klick-billable 6})))

(deftest calculate-user-percentages
  (is
   (=
    (dc/calculate-user-percentage {:person/userid 4699,
                                   :person/name "Max",
                                   ::labs-billable 3,
                                   ::klick-billable 4
                                   ::dc/sum 7}
                                  ::klick-billable)
    "57%")))

#_(deftest calculate-overall-percentages
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


(deftest datatable-to-vec
  (is (= (dc/data-table-to-vec {:max {:dashboard.core/klick-billable 2.25,
                                      :dashboard.core/labs-billable 1.25,
                                      :view/index 4,
                                      :dashboard.core/promo 0,
                                      :dashboard.core/admin 19.483333333333334,
                                      :person/userid 4966,
                                      :person/name "Max",
                                      :dashboard.core/sum 40.3,
                                      :view/name "Max",
                                      :dashboard.core/non-billable 4.516666666666667,
                                      :dashboard.core/studies 0.5,
                                      :dashboard.core/experiments 12.3}
                                :yan {:dashboard.core/klick-billable 2.25,
                                      :dashboard.core/labs-billable 1.25,
                                      :view/index 4,
                                      :dashboard.core/promo 0,
                                      :dashboard.core/admin 19.483333333333334,
                                      :person/name "Yan",
                                      :dashboard.core/sum 40.3,
                                      :view/name "Yan",
                                      :dashboard.core/non-billable 4.516666666666667,
                                      :dashboard.core/studies 0.5,
                                      :dashboard.core/experiments 12.3}}

                               {:dashboard.core/klick-billable {:display-name "Klick Billable",
                                                                :sort-index 2}
                                :dashboard.core/non-billable {:display-name "Non-Billable",
                                                              :sort-index 3}
                                :dashboard.core/admin {:display-name "Administration",
                                                       :sort-index 4 }}

                               #{::dc/klick-billable ::dc/non-billable ::dc/admin}

                               #{:yan :max}
                               )

         '(("-" "Yan" "Max")
          ("Klick Billable" 2.25 2.25)
          ("Administration" 19.483333333333334 19.483333333333334)
          ("Non-Billable" 4.516666666666667 4.516666666666667)))))


(deftest vec2csv
  (is (= (dc/vec2csv '(("-" "Yan" "Max")
                      ("Klick Billable" 2.25 2.25)
                      ("Administration" 19.483333333333334 19.483333333333334)
                       ("Non-Billable" 4.516666666666667 4.516666666666667)))
         "data:text/csv;charset=utf-8,%22-%22,%22Yan%22,%22Max%22%0A%22Klick%20Billable%22,%222.25%22,%222.25%22%0A%22Administration%22,%2219.483333333333334%22,%2219.483333333333334%22%0A%22Non-Billable%22,%224.516666666666667%22,%224.516666666666667%22")))


(deftest range-rules
          (let [entries
               [
                ;;2016
                  {:ProjectName "2016", :AccountPortfolioID 626, :Approved true, :IsClientBillable true, :TicketTitle "Pepper Learning & Development", :UserName "Max Gerlach", :CreatedByUserID 4966, :CompanyID 1, :UserID 6308,  :ProjectID 23404, :Date #inst "2016-11-21T14:15:00.000-00:00", :Duration 165,}
                  ;;2015
                  { :ProjectName "2015",  :AccountPortfolioID 717, :Note "Pepperesentation", :IsClientBillable true, :TicketTitle "AUA All Agency Meeting 11/21", :UserName "Max Gerlach", :CreatedByUserID 4966, :CompanyID 693, :UserID 4966,:ProjectID 23305, :Date #inst "2015-11-21T17:00:00.000-00:00", :Duration 60, :TicketID 1123773, }
                  ;;2017
                  { :ProjectName "2017",  :AccountPortfolioID 717, :Note "Pepperesentation", :IsClientBillable true, :TicketTitle "AUA All Agency Meeting 11/21", :UserName "Max Gerlach", :CreatedByUserID 4966, :CompanyID 693, :UserID 4966,:ProjectID 23305, :Date #inst "2017-11-21T17:00:00.000-00:00", :Duration 60, :TicketID 1123773, }
                ]
                maxc (dc/mk-person "constrained" 4966 1 #inst "2016" #inst "2016-12")
                maxf (dc/mk-person "fulltime" 4966 2 )
                db (d/create-conn {})
                rules '[
                        [(in-reporting-period ?person ?entry)
                         [(get-else $ ?person :labs/from #inst "1900") ?from]
                         [(get-else $ ?person :labs/to #inst "2100") ?to]
                         [?entry :Date ?datetime]
                         [(>= ?datetime ?from)]
                         [(<= ?datetime ?to)]]
                        ]
                ]

          (d/transact! db [maxf maxc])
          (d/transact! db entries)


          (is (= ["2016"]
                 (d/q '[:find [?pname ...]
                        :in $ % ?name
                        :where
                        [?p :ProjectName ?pname]
                        [?m :person/name ?name]
                        (in-reporting-period ?m ?p)
                        ]
                      @db
                      rules
                      "constrained")))

          (is (= ["2016" "2015" "2017"]
                 (d/q '[:find [?pname ...]
                        :in $ % ?name
                        :where
                        [?p :ProjectName ?pname]
                        [?m :person/name ?name]
                        (in-reporting-period ?m ?p)
                        ]
                      @db
                      rules
                      "fulltime")))
          ))
