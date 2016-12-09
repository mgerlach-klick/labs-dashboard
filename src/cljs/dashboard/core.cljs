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


;;------- time stuff
(def date-formatter (tf/formatters :year-month-day))
(def date-format (partial tf/unparse date-formatter))
(defn last-n-months [n] (mapv date-format [(-> n t/months t/ago) (t/now)]))




;;-------- application state

(def db-schema {
                :UserID {:db/valueType :db.type/ref}
                :person/userid {:db/unique :db.unique/identity}})

(defonce initial-state {:conn (d/create-conn db-schema )
                        :from (first (last-n-months 1))
                        :to (second (last-n-months 1))
                        :last-fetch nil
                        :calculated-billing-matrix nil
                        :show-percentages? false
                        :loading? true})






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

(defn transact-time-entries! [conn start end userids]
  (prn "Getting time entries from" start "to" end)
  (take! (<time-entry start end userids) (fn [response]
                                           (prn (:status response))
                                           (if-not (= (:status response) 200)
                                             (js/alert "There has been an error while fetching the data from Genome. Are you sure that you're logged in?")
                                             (do
                                               (->> response
                                                    :body
                                                    :Entries
                                                    (d/transact! conn))
                                               (prn "received" (count (-> response :body :Entries)) "records")
                                               (dispatch [:update-last-fetch])
                                               (dispatch [:calculate-billing-matrix])
                                               (dispatch [:loading? false]))))))

(defn get-labster-time-entries [conn start end]
  (transact-time-entries! conn start end labster-ids))












;;----------- db stuff

(defn transact-users! [db users]
  (doseq [l users]
    (d/transact! db [{:person/name (first l)
                      :person/userid (second l)}])))


(defn new-database [conn start end]
  (d/reset-conn! conn (d/empty-db db-schema))
  (transact-users! conn +labsters+ )
  (get-labster-time-entries conn start end)
  conn)

(defn format-hours-pct [[hrs total]]
  (if  (zero? hrs) "-"
       (let [hours (cond
                     (number? hrs) (.toFixed hrs 2)
                     (string? hrs) hrs)
             percentage (* 100 (/ hrs total))
             percentage-str (.toFixed percentage 0)]
         (str hours " (" percentage-str "%)"))))

(defn format-hours [hrs]
  (cond
    (zero? hrs) "-"
    (number? hrs) (.toFixed hrs 2)
    (string? hrs) hrs))


;;-------- GO GO GO

;;---- QUERIES

(defn time-spent-on-projs [db name]
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
       db
       name))

(defn time-spent-on-projid [db projid uid]
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
        db
        uid
        projid)
   0))

(defn time-spent-not-billable [db uid]
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
        db
        uid)
   0))

(defn time-spent-on-billable-projects [db uid]
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
        db
        uid)
   0))


(defn total-time-booked [db uid]
  (or
   (d/q '[:find (sum ?duration) .
          :in $ ?uid
          :with ?p
          :where
          [?p :UserID ?uid]
          [?p :Duration ?mins]
          [(/ ?mins 60) ?duration]
          ]
        db
        uid)
   0))


;; Duration by project
(defn q [db query]
  (prn
   (d/q query
       db
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
  [db lab-projects labster-ids labster-names]
  (let [
        labster-time-spent-on (fn [db proj-name]
                                (map (partial time-spent-on-projid db (get lab-projects proj-name)) labster-ids))
        labs-billable (labster-time-spent-on db "Labs Billable")
        others-billable (map (partial time-spent-on-billable-projects db) labster-ids)
        others-unbillable (map (partial time-spent-not-billable db) labster-ids)
        administration (labster-time-spent-on db "Administration")
        experiments (labster-time-spent-on db "Experiments")
        studies (labster-time-spent-on db "Studies")
        promo (labster-time-spent-on db "Promo")
        personal-sum (map (partial total-time-booked db) labster-ids)
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


;; -- Event Handlers ----------------------------------------------------------
(reg-event-db :initialize
              (fn [db _]
                (let [[from to] (last-n-months 1)
                      initial-db (merge db initial-state)]
                  (dispatch [:new-database])
                  initial-db)))


(reg-event-db :loading?
              (fn [db [_ yesno]]
                (assoc db :loading? yesno)))

(reg-event-db :show-percentages?
              (fn [db _]
                (update db :show-percentages? not)))


(reg-event-db :set-from-to
              (fn [db [_ [from to]]]
                (assoc db
                       :from from
                       :to to)))

(reg-event-db :update-last-fetch
              (fn [db]
                (assoc db :last-fetch (str (:from db) " to " (:to db)))))

(reg-event-db :calculate-billing-matrix
              (fn [db _]
                (assoc db :calculated-billing-matrix (calculate-billing-matrix @(:conn db) +lab-projects+ labster-ids labster-names))))

(reg-event-db :new-database
              (fn [db _]
                (let [conn (:conn db)
                      from (:from db)
                      to (:to db)]
                  (new-database conn from to)
                  db)))




;; -- Subscription Handlers ---------------------------------------------------


(reg-sub :from-to
         (fn [db _]
           (select-keys db [:from :to])))

(reg-sub :billing-matrix
         (fn [db _]
            (:calculated-billing-matrix db)))

(reg-sub :last-fetch
         (fn [db _]
           (:last-fetch db)))

(reg-sub :loading?
         (fn [db _]
           (:loading? db)))

(reg-sub :show-percentages?
         (fn [db _]
           (:show-percentages? db)))



; -- Components

(defn dashboard-percentage-table [matrix]
  (let [totals (->> matrix last rest ) ; last row of matrix without header and '100%' is sums. Partition them up
        pctg-matrix-row #(let [r (get matrix %)]
                      (vector :tr
                              (vector :th (first r))
                              (map (comp (partial vector :td)
                                         (partial format-hours-pct))
                                   (map vector
                                        (rest (butlast r))
                                        totals))
                              (vector :td (last r))))
        matrix-row #(let [r (get matrix %)]
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
        (map (partial vector :th) (get matrix 0))]]

      [:tbody
       (for [idx (->> matrix
                      count
                      range
                      rest
                      butlast)] ; the correct indices for the non-header fields
         (pctg-matrix-row idx))
       (matrix-row (->> matrix count range last ))]]]))

(defn dashboard-table [matrix]
  (prn 'matrix (type matrix))
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
            (map (partial vector :th) (get matrix 0))]]

          [:tbody
           (for [idx (->> matrix
                          count
                          range
                          rest)] ; the correct indices for the non-header fields
             (matrix-row idx))]]]))

(defn dashboard-page []
  (let [billing-matrix (subscribe [:billing-matrix])
        from-to (subscribe [:from-to])
        last-fetch (subscribe [:last-fetch])
        loading? (subscribe [:loading?])
        show-percentages? (subscribe [:show-percentages?])
        csv-billing-matrix (reaction (vec2csv @billing-matrix))
        csv-billing-matrix-filename (reaction (str "labs-billability_" (:from @from-to) "__" (:to @from-to) ".csv"))]
    (fn []
      [:div
       [:div.row
        [:div.col-sm-12
         [:h1 "Labs Billability Dashboard"]]]

       [:hr]

       [:div.row
        [:div.col-sm-4
         [:div.row
          [:div.btn.btn-default {:on-click #(dispatch [:set-from-to (last-n-months 1)])} "Last Month"]]
         [:div.row
          [:div.btn.btn-default {:on-click #(dispatch [:set-from-to (last-n-months 3)])} "Last 3 Months"]]
         [:div.row
          [:div.btn.btn-default {:on-click #(dispatch [:set-from-to (last-n-months 6)])} "Last 6 Months"]]]

        [:div.col-sm-4.text-right
         [:div.row
          "From: " [:input {:type "text"
                            :value (:from @from-to)
                            :on-change #(dispatch [:set-from-to [(-> % .-target .-value) (:to @from-to)]])}]]
         [:div.row

          "To: " [:input {:type "text"
                          :value (:to @from-to)
                          :on-change #(dispatch [:set-from-to [(:from @from-to) (-> % .-target .-value)]])}]]
         [:div.row
          "Include percentages: " [:input {:type :checkbox
                                       :value @show-percentages?
                                       :on-change #(dispatch [:show-percentages?])}]]]

        [:div.col-sm-4.text-right
         [:div.btn-success.btn.btn-lg {:on-click #(when (and (verify-is-date (:from @from-to))
                                                             (verify-is-date (:to @from-to)))
                                                    (do
                                                      (dispatch [:loading? true])
                                                      (dispatch [:new-database])
                                                      ))} "Make it so" ]
         [:hr]
         [:a {:href @csv-billing-matrix :download @csv-billing-matrix-filename}
          [:div.btn-primary.btn  "Download as CSV" ]]]]

       [:hr]

       (if @loading?
         [:h3 "Loading..."]
         [:div.row
          [:div.col-sm-12
           (if @show-percentages?
             [dashboard-percentage-table @billing-matrix]
             [dashboard-table @billing-matrix])]
          [:h3.text-right {:style {:color "gray"}} @last-fetch]])])))


;; -------------------------
;; Initialize app

(defn mount-root [root-element]
  (when (empty? @re-frame.db/app-db)
    (prn "Database empty. Initializing")
    (dispatch [:initialize]))
  (reagent/render [root-element] (.getElementById js/document "app")))

(defn init! []
  (mount-root #'dashboard-page))
