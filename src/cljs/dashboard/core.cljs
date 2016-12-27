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
                                   subscribe]
             :as rf]
            [re-frame-datatable.core :as dt]

            [dashboard.queries :refer [;time-spent-on-projs
                                       time-spent-on-projid
                                       time-spent-not-billable
                                       time-spent-on-billable-projects
                                       total-time-booked]]
            ))


;;; TODO
;; - Maybe factor some stuff out into own modules
;; Use datatable component:https://kishanov.github.io/re-frame-datatable/
;; This means, end up with a column-based format, i.e. {:name "Max" :UserID 4966 :LabsBillable: 123 :Nonbillable nil}





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
(defn mk-person [name uid idx]
  {:display-name name
   :person/name name
   :person/userid uid
   :display-index idx})


(def +labsters+ [
                 (mk-person "Yan" 5959 15)
                 (mk-person "Carolyn" 5123 8)
                 (mk-person "Andrew" 6157 1)
                 (mk-person "Ken" 4803 3)
                 (mk-person "Ani" 6308 2)
                 (mk-person "Max"  4966 4)
                 (mk-person "Kat" 5904 5)
                 (mk-person "Pete" 5466 6)
                 (mk-person "Stephen" 5417 7)
                 ])

(defn all-user-ids [users]
  (map :person/userid users))

(defn all-user-names [users]
  (map :person/name users))

(def +lab-projects+ {:labs-billable 23409
                     :admin  16897
                     :studies 23405
                     :experiments 23404
                     :promo 22295})

(defn ffind
  "first object in s where a is v"
  [s a v]
  (first
   (filter #(= (get % a) v) s)))

(defn project-id-for-tag [tag]
  (get +lab-projects+ tag))


(defn project-id-for-name [name]
  (let [names {"Labs Billable" :labs-billable
               "Administration" :admin
               "Experiments" :experiments
               "Studies" :studies
               "Promo" :promo}]
    (project-id-for-tag (get names name))))



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
                                               (dispatch [:calculate-data-table])
                                               (dispatch [:loading? false]))))))

(defn get-labster-time-entries [conn start end]
  (transact-time-entries! conn start end (all-user-ids +labsters+)))












;;----------- db stuff

(defn transact-users! [db users]
  (d/transact! db users))


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


(defn time-spent-on-tag [db tag uid]
  (time-spent-on-projid db (project-id-for-tag tag) uid))

(def +dashboard-pages+ {
                        ::labs-billable {:display-name "Labs Billable"
                                         :calculation (fn [db uid]
                                                        (time-spent-on-tag db :labs-billable uid))}
                        ::klick-billable {:display-name "Klick Billable"
                                          :calculation (fn [db uid]
                                                         (time-spent-on-billable-projects db uid))}
                        ::non-billable {:display-name "Non-Billable"
                                        :calculation (fn [db uid]
                                                       (time-spent-not-billable db uid))}
                        ::admin {:display-name "Administration"
                                 :calculation (fn [db uid]
                                                (time-spent-on-tag db :admin uid))}
                        ::experiments {:display-name "Experiments"
                                       :calculation (fn [db uid]
                                                      (time-spent-on-tag db :experiments uid))}
                        ::studies {:display-name "Studies"
                                   :calculation (fn [db uid]
                                                  (time-spent-on-tag db :studies uid))}
                        ::promo {:display-name "Promotion"
                                 :calculation (fn [db uid]
                                                (time-spent-on-tag db :promo uid))}
                        ::sum {:display-name "SUM"
                               :calculation total-time-booked}
                        })

(defn add-section-items-to-user
  "Calculates the values for all the items in the sections map for a user
  and returns a new user map that has these values assoced into it"
  [db sections user]
  (reduce (fn [user [dashboard-key dashboard-item]]
            (let [uid (:person/userid user)
                  display-name (:display-name dashboard-item)
                  calc-fn (:calculation dashboard-item)
                  value (calc-fn db uid)
                  formatted-value (format-hours value)]
              (assoc user dashboard-key {:display-name display-name
                                         :value value
                                         :formatted-value formatted-value})))
          user
          (seq sections)))


(defn calculate-section-sums
  "Calculates a total for all the keys in the sections map"
  [sections users]
  (into (hash-map)
        (let [sum (partial reduce +)]
          (reduce (fn [m k]
                    (let [summed-values (sum (map #(get-in % [k :value]) users))]
                      (-> m
                          (assoc-in [k :value] summed-values)
                          (assoc-in [k :formatted-value] (format-hours summed-values)))
                      )) {} (keys sections)))))

(defn calculate-user-percentages
  "Calculates the percentages of all section items for a user based on a section-sum map"
  [section-sums user]
  (reduce (fn [user [section-key section]]
            (let [sum-all (get section :value)
                  uval (get-in user [section-key :value])
                  percentage (if (zero? uval) 0 (/ (* 100 uval) sum-all))
                  formatted-percentage (if (zero? percentage) "-" (str (.toFixed percentage 0) \%))]
              (-> user
                  (assoc-in [section-key :percentage] percentage)
                  (assoc-in [section-key :formatted-percentage] formatted-percentage))))
          user
          (seq section-sums)))

(defn calculate-overall-percentages
  "calculates percentages for section keys based on a section-sums map"
  [section-sums]
  (prn  (map :value (vals section-sums)))
  (let [all-sums (map :value (vals section-sums))
        overall-sum (reduce + all-sums)
        percentage #(if (zero? %) 0 (/ (* 100 %) overall-sum))
        formatted-percentage #(if (zero? (percentage %)) "-" (str (.toFixed (percentage %) 2) \%))]
    (reduce (fn [m [k v]]
              (-> m
                  (assoc-in [k :value] (percentage (:value v)))
                  (assoc-in [k :formatted-value] (formatted-percentage (:value v)))))
            {}
            section-sums)))

(defn calculate-all
  "Performs all the calculations and returns all the 'columns' of the table "
  [db sections users]
  (let [calc-users (map (partial add-section-items-to-user db sections) users)
        section-sums (calculate-section-sums sections calc-users)
        pct-users (map (partial calculate-user-percentages section-sums) calc-users)
        overall-percentages (calculate-overall-percentages section-sums)]
    (conj pct-users
          (assoc section-sums
                 :display-name "SUM"
                 :display-index 99)
          (assoc overall-percentages
                 :display-name "PCT"
                 :display-index 100))))


(defn calculate-billing-matrix
  ""
  [db labster-ids labster-names]
  (let [
        labster-time-spent-on (fn [db proj-tag]
                                (map (partial time-spent-on-projid db (project-id-for-tag proj-tag)) labster-ids))
        labs-billable (labster-time-spent-on db ::labs-billable)
        others-billable (map (partial time-spent-on-billable-projects db) labster-ids)
        others-unbillable (map (partial time-spent-not-billable db) labster-ids)
        administration (labster-time-spent-on db ::admin)
        experiments (labster-time-spent-on db ::experiments)
        studies (labster-time-spent-on db ::studies)
        promo (labster-time-spent-on db ::promo)
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
                (assoc db :calculated-billing-matrix (calculate-billing-matrix @(:conn db) (all-user-ids +labsters+) (all-user-names +labsters+)))))


(reg-event-db :calculate-data-table
              (fn [db _]
                (assoc db :data-table (calculate-all @(:conn db) +dashboard-pages+ +labsters+))))

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

(reg-sub :data-table
         (fn [db _]
           (:data-table db)))

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

(defn dashboard-percentage-table [matrix date]
  (let [totals (->> matrix last rest ) ; last row of matrix without header and '100%' is sums. Partition them up
        pctg-matrix-row #(let [r (get matrix %)]
                           (vector :tr
                                   (if-let [projid (project-id-for-name (first r))]
                                     (vector :th {:on-click (fn []
                                                              (js/window.open
                                                               (str "https://genome.klick.com/tasks/project/home/" projid)))}(first r))
                                     (vector :th (first r)))
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
        (map (fn [name]
               (if-let [uid (get +labsters+ name)]
                 (vector :th {:on-click (fn []
                                          (js/window.open
                                           (str "https://genome.klick.com/scheduler/#/week/" date "/user/" uid)))} name)
                 (vector :th name)))
             (get matrix 0))]]

      [:tbody
       (for [idx (->> matrix
                      count
                      range
                      rest
                      butlast)] ; the correct indices for the non-header fields
         (pctg-matrix-row idx))
       (matrix-row (->> matrix count range last ))]]]))

(defn dashboard-table [matrix date]
  (prn 'matrix (type matrix))
  (let [matrix-row #(let [r (get matrix %)]
                      (vector :tr
                              (if-let [projid (project-id-for-name (first r))]
                                (vector :th {:on-click (fn []
                                                         (js/window.open
                                                          (str "https://genome.klick.com/tasks/project/home/" projid)))}(first r))
                                (vector :th (first r)))
                              (map (comp (partial vector :td)
                                         (partial format-hours))
                                   (rest (butlast r)))
                              (vector :td (last r))))]
    [:div.row
     [:table.table.table-hover.table-bordered ; {:class "table table-striped"}
      [:thead
       [:tr ; names
        (map (fn [name]
               (if-let [uid (get +labsters+ name)]
                 (vector :th {:on-click (fn []
                                          (js/window.open
                                           (str "https://genome.klick.com/scheduler/#/week/" date "/user/" uid)))} name)
                 (vector :th name)))
             (get matrix 0))]]

      [:tbody
       (for [idx (->> matrix
                      count
                      range
                      rest)] ; the correct indices for the non-header fields
         (matrix-row idx))]]]))


(defn data-table
  [subscriptions-vector
   row-def-vec ;;contains ::row-key and ::row-label (str or key)
   ]
  (let [rows (subscribe subscriptions-vector)
        sorted-rows (sort-by :display-index < @rows)
        row-keys (map ::row-key row-def-vec )]
    (fn []
      [:table.ui.celled.striped.table
       [:thead
        [:tr
         [:th "-"]
         (for [row sorted-rows]
           (conj [:th] (get row :display-name)))]]
       [:tbody
        (for [row-def row-def-vec]
          [:tr
           [:th (::row-label row-def)]
           (for [person sorted-rows]
             (do
               (prn (get-in person [:display-name])
                    (get-in person [(::row-key row-def) :formatted-value]))
               [:td (get-in person [(::row-key row-def) :formatted-value])]))])]])))


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
           [data-table
            [:data-table]
            (vec (map (fn [[k v]]
                        {::row-key k ::row-label (:display-name v)})
                      (seq +dashboard-pages+)))
            #_{::dt/table-classes ["ui" "celled" "stripped" "table"]}
            ]
           ;; (if @show-percentages?
           ;;   [dashboard-percentage-table @billing-matrix (:to @from-to)]
           ;;   [dashboard-table @billing-matrix (:to @from-to)])
           ]
          [:h3.text-right {:style {:color "gray"}} @last-fetch]])])))


;; -------------------------
;; Initialize app

(defn mount-root []
  (when (empty? @re-frame.db/app-db)
    (prn "Database empty. Initializing")
    (dispatch [:initialize]))
  (reagent/render [#'dashboard-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
