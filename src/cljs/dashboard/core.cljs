(ns dashboard.core
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
              [cljs-http.client :as http]
              [cljs.core.async :refer [<! put! take!]]
              [datascript.core :as d]
              [cljs.pprint :as pprint]
              [goog.string :as gstring]
              [goog.string.format]
              [cljs-time.core :as t]
              [cljs-time.format :as tf]
              [testdouble.cljs.csv :as csv]
              [re-frame.core :refer [reg-event-db
                                     path
                                     reg-sub
                                     dispatch
                                     dispatch-sync
                                     subscribe]]
              ))


;;; TODO
;;; - Fill out last week's schedule, export it, and use it to write tests
;;; - Maybe factor some stuff out into own modules
;;; - try to factor out "not-labs-project" rule
;;; - click to go to labster schedule / project page





(enable-console-print!)

(declare calculate-billing-matrix )


;;------- time stuff
(def date-formatter (tf/formatters :year-month-day))
(def date-format (partial tf/unparse date-formatter))
(defn last-n-months [n] (mapv date-format [(-> n t/months t/ago) (t/now)]))




;;-------- application state

(def db-schema {
                :UserID {:db/valueType :db.type/ref}
                :person/userid {:db/unique :db.unique/identity}})

(defonce conn (d/create-conn db-schema ))

(defonce app-db (reagent/atom {:from (first (last-n-months 1))
                               :to (second (last-n-months 1))
                               :last-fetch nil
                               :calculated-billing-matrix nil
                               :loading? true}))






;; ----- hardcoded data
(def +labsters+ [
                 ["Yan" 5959]
                 ["Carolyn" 5123]
                 ["Andrew" 6157]
                 ["Ken" 4803]
                 ["Ani" 6308]
                 ["Max"  4966]
                 ["Kat" 5904]
                 ["Pete" 5466]
                 ["Stephen" 5417]
                 ])
(def labster-names (map first +labsters+))
(def labster-ids (map second +labsters+))

(def +lab-projects+ {"Labs Billable" 23409
                     "Administration"  16897
                     "Studies" 23405
                     "Experiments" 23404
                     "Promo" 22295})

(def lab-project-ids (vals +lab-projects+))








;;------- get data
(defn <time-entry
  ""
  [start end userids]
  (let [userids* (if (coll? userids) userids (vector userids))]
    (->  (str "https://genome.klick.com/api/TimeEntry.json"
              "?EndDate=" end ;"T23:59:59-04:00"
              "&StartDate=" start ;"T00:00:00-04:00"
              "&UserIDs=[" (apply str (interpose \,  userids*)) "]")
         (http/jsonp))))

(defn transact-time-entries! [db start end userids]
  (prn "Getting time entries from" start "to" end)
  (take! (<time-entry start end userids) (fn [response]
                                           (prn (:status response))
                                           (if-not (= (:status response) 200)
                                             (js/alert "There has been an error while fetching the data from Genome. Are you sure that you're logged in?")
                                             (do
                                               (->> response
                                                    :body
                                                    :Entries
                                                    (d/transact! db))
                                               (prn "received" (count (-> response :body :Entries)) "records")
                                               (swap! app-db assoc :last-fetch (str (:from @app-db) " to " (:to @app-db)))
                                               (swap! app-db assoc :calculated-billing-matrix (calculate-billing-matrix))
                                               (swap! app-db assoc :loading? false)
                                               (reagent/force-update-all))))))

(defn get-labster-time-entries [start end]
  (transact-time-entries! conn start end labster-ids))












;;----------- db stuff

(defn transact-users! [db users]
  (doseq [l users]
    (d/transact! db [{:person/name (first l)
                      :person/userid (second l)}])))


(defn new-database [start end]
  (d/reset-conn! conn (d/empty-db db-schema))
  (transact-users! conn +labsters+ )
  (get-labster-time-entries start end))

(defn format-hours [hrs]
  (cond
    (zero? hrs) "-"
    (number? hrs) (.toFixed hrs 2)
    (string? hrs) hrs))


;;-------- GO GO GO
(when (= @conn (d/empty-db db-schema))
  (let [[from to] (last-n-months 1)]
    (new-database from to)))


;;---- QUERIES

(defn time-spent-on-projs [name]
  (d/q '[:find ?p (sum ?duration) .
         :in $ ?name
         :with ?p
         :where
         [?l :person/name ?name]
         [?l :person/userid ?id]
         [?p :UserID ?id]
         [?p :ProjectName ?proj]
         [?p :Duration ?d]
         [(/ ?d 60) ?duration]
         ]
       @conn
       name))

(defn time-spent-on-projid [projid uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid ?projid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :ProjectID ?projid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          ]
        @conn
        uid
        projid)
   0))

(defn time-spent-not-billable [uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :IsClientBillable false]
          [?p :Duration ?mins]
          [?p :ProjectID ?pid]
          [(/ ?mins 60) ?duration]
          ;; +labs-projects+
          [(get-else $ ?e :IsClientBillable false)]
          ;; [(= :nil ?nobill)]
          [(not= ?pid 23409)]
          [(not= ?pid 16897)]
          [(not= ?pid 23405)]
          [(not= ?pid 23404)]
          [(not= ?pid 22295)]
          ]
        @conn
        uid)
   0))

(defn time-spent-on-billable-projects [uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :ProjectID ?pid]
          [?p :IsClientBillable true]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          ;; +labs-projects+
          [(not= ?pid 23409)]
          [(not= ?pid 16897)]
          [(not= ?pid 23405)]
          [(not= ?pid 23404)]
          [(not= ?pid 22295)]
          ]
        @conn
        uid)
   0))


(defn total-time-booked [uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          ]
        @conn
        uid)
   0))


;; Duration by project
(defn q [query]
  (prn
   (d/q query
        @conn
        )))



;; ----------- site helpers
(defn vec2csv
  ""
  [v]
  (js/encodeURI
   (str "data:text/csv;charset=utf-8,"
        (csv/write-csv v :quote true))))


(defn verify-is-date [d]
  (if-not (re-matches #"\d{4}-\d{2}-\d{2}" d)
    (js/alert (str \" d \" " is not a valid date!"))
    d))


;; -------------------------
;; Views


(defn calculate-billing-matrix
  ""
  []
  (let [
        labster-time-spent-on (fn [proj-name]
                                (map (partial time-spent-on-projid (get +lab-projects+ proj-name)) labster-ids))
        labs-billable (labster-time-spent-on "Labs Billable")
        others-billable (map (partial time-spent-on-billable-projects) labster-ids)
        others-unbillable (map (partial time-spent-not-billable) labster-ids)
        administration (labster-time-spent-on "Administration")
        experiments (labster-time-spent-on "Experiments")
        studies (labster-time-spent-on "Studies")
        promo (labster-time-spent-on  "Promo")
        personal-sum (map total-time-booked labster-ids)
        sum (partial reduce +)
        sum-all (reduce + personal-sum)
        percentall #(str (.toFixed (/ (* 100 (sum %)) sum-all) 2) \%)
        ]
  `[
    ["-" ~@labster-names "SUM" "PCT"]
    ["Labs Billable" ~@labs-billable ~(sum labs-billable) ~(percentall labs-billable)]
    ["Klick Billable" ~@others-billable ~(sum others-billable) ~(percentall others-billable)]
    ["Nonbillable" ~@others-unbillable ~(sum others-unbillable) ~(percentall others-unbillable)]
    ["Administration" ~@administration ~(sum administration) ~(percentall administration)]
    ["Experiments" ~@experiments ~(sum experiments) ~(percentall experiments)]
    ["Studies" ~@studies ~(sum studies) ~(percentall studies)]
    ["Promo" ~@promo ~(sum promo) ~(percentall promo)]
    ["SUM" ~@personal-sum ~sum-all "100% (hopefully)" ]
    ]))


(defn dashboard-table [matrix]
  (let [matrix-row #(let [r (get matrix %)]
                      (vector :tr
                              (vector :th (first r))
                              (map (comp (partial vector :td)
                                         (partial format-hours))
                                   (rest (butlast r)))
                              (vector :td (last r))))]
        [:div.row
         [:table.table.table-hover.table-bordered ; {:class "table table-striped"}
          [:thead
           [:tr ; names
            (map (partial vector :th) (get matrix 0))
            ]]

          [:tbody
           (for [idx (->> matrix
                          count
                          range
                          rest)] ; the correct indices for the non-header fields
             (matrix-row idx))]]]))

(defn dashboard-page []
  (let [billing-matrix (reaction (:calculated-billing-matrix @app-db))
        csv-billing-matrix (reaction (vec2csv @billing-matrix))
        csv-billing-matrix-filename (reaction (str "labs-billability_" (:from @app-db) "__" (:to @app-db) ".csv"))]
    (fn []
      [:div
       [:div.row
        [:div.col-sm-12
         [:h1 "Labs Billability Dashboard"]]]

       [:hr]

       [:div.row
        [:div.col-sm-4
         [:p
          [:div.btn.btn-default {:on-click #(let [[from to] (last-n-months 1)]
                                              (swap! app-db assoc :from from)
                                              (swap! app-db assoc :to to))} "Last Month"]]
         [:p
          [:div.btn.btn-default {:on-click #(let [[from to] (last-n-months 3)]
                                              (swap! app-db assoc :from from)
                                              (swap! app-db assoc :to to))} "Last 3 Months"]]
         [:p
          [:div.btn.btn-default {:on-click #(let [[from to] (last-n-months 6)]
                                              (swap! app-db assoc :from from)
                                              (swap! app-db assoc :to to))} "Last 6 Months"]]]

        [:div.col-sm-4.text-right
         [:p
          "From: " [:input {:type "text"
                            :value (:from @app-db)
                            :on-change #(swap! app-db assoc :from (-> % .-target .-value))}]]
         [:p
          "To: " [:input {:type "text"
                          :value (:to @app-db)
                          :on-change #(swap! app-db assoc :to (-> % .-target .-value))}]]]

        [:div.col-sm-4.text-right
         [:div.btn-success.btn.btn-lg {:on-click #(when (and (verify-is-date (:from @app-db))
                                                             (verify-is-date (:to @app-db)))
                                                    (do
                                                      (swap! app-db assoc :loading? true)
                                                      (new-database (:from @app-db) (:to @app-db))))} "Make it so" ]
         [:hr]
         [:a {:href @csv-billing-matrix :download @csv-billing-matrix-filename}
          [:div.btn-primary.btn  "Download as CSV" ]]]]

       [:hr]

       (if (:loading? @app-db)
         [:h3 "Loading..."]
         [:div.row
          [:div.col-sm-12
           [dashboard-table @billing-matrix]]
          [:h3.text-right {:style {:color "gray"}}  (:last-fetch @app-db)]])])))


;; -------------------------
;; Initialize app

(defn mount-root [root-element]
  (reagent/render [root-element] (.getElementById js/document "app")))

(defn init! []
  (mount-root #'dashboard-page))
