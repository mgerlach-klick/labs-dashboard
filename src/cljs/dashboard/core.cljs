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
                                   subscribe] :as rf]
            ;; [re-frame-datatable.core :as dt]

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
                        ;; :calculated-billing-matrix nil
                        :data-table nil
                        :show-percentages? false
                        :loading? true
                        :include-yan? true})





;; ----- hardcoded data

(defn open-genome-scheduler [uid]
  (fn [date]
    (js/window.open
     (str "https://genome.klick.com/scheduler/#/week/" date "/user/" uid))))


(defn mk-person [name uid idx]
  {:display-name name
   :person/name name
   :person/userid uid
   :display-index idx
   :on-click (open-genome-scheduler uid)})


(def +labsters+ {
                 :labster/yan (mk-person "Yan" 5959 20)
                 :labster/carolyn (mk-person "Carolyn" 5123 8)
                 :labster/andrew (mk-person "Andrew" 6157 1)
                 :labster/ken (mk-person "Ken" 4803 3)
                 :labster/ani (mk-person "Ani" 6308 2)
                 :labster/max (mk-person "Max"  4966 4)
                 :labster/kat (mk-person "Kat" 5904 5)
                 :labster/pete (mk-person "Pete" 5466 6)
                 :labster/stephen (mk-person "Stephen" 5417 7)
                 })

(defn all-user-ids [users]
  (map :person/userid (vals users)))


(def +lab-projects+ {:labs-billable 23409
                     :admin  16897
                     :studies 23405
                     :experiments 23404
                     :promo 22295})


(defn project-id-for-tag [tag]
  (get +lab-projects+ tag))


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
                                               ;; (dispatch [:calculate-billing-matrix])
                                               (dispatch [:calculate-data-table])
                                               (dispatch [:loading? false]))))))

(defn get-labster-time-entries [conn start end]
  (transact-time-entries! conn start end (all-user-ids +labsters+)))












;;----------- db stuff

(defn transact-users! [db users]
  (d/transact! db users))


(defn new-database [conn start end]
  (d/reset-conn! conn (d/empty-db db-schema))
  (transact-users! conn (vals +labsters+) )
  (get-labster-time-entries conn start end)
  conn)


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

(defn open-genome-project-home [projid]
  (fn []
    (js/window.open
     (str "https://genome.klick.com/tasks/project/home/" projid))))

(def +dashboard-sections+ {
                        ::labs-billable {:display-name "Labs Billable"
                                         :sort-index 1
                                         :calculation (fn [db uid]
                                                        (time-spent-on-tag db :labs-billable uid))
                                         :on-click (open-genome-project-home (:labs-billable +lab-projects+))}

                        ::klick-billable {:display-name "Klick Billable"
                                          :sort-index 2
                                          :calculation (fn [db uid]
                                                         (time-spent-on-billable-projects db uid))}

                        ::non-billable {:display-name "Non-Billable"
                                        :sort-index 3
                                        :calculation (fn [db uid]
                                                       (time-spent-not-billable db uid))}

                        ::admin {:display-name "Administration"
                                 :sort-index 4
                                 :calculation (fn [db uid]
                                                (time-spent-on-tag db :admin uid))
                                 :on-click (open-genome-project-home (:admin +lab-projects+))}

                        ::experiments {:display-name "Experiments"
                                       :sort-index 5
                                       :calculation (fn [db uid]
                                                      (time-spent-on-tag db :experiments uid))
                                       :on-click (open-genome-project-home (:experiments +lab-projects+))}

                        ::studies {:display-name "Studies"
                                   :sort-index 6
                                   :calculation (fn [db uid]
                                                  (time-spent-on-tag db :studies uid))
                                   :on-click (open-genome-project-home (:studies +lab-projects+))}

                        ::promo {:display-name "Promotion"
                                 :sort-index 7
                                 :calculation (fn [db uid]
                                                (time-spent-on-tag db :promo uid))
                                 :on-click (open-genome-project-home (:promo +lab-projects+))}

                        ::sum {:display-name "SUM"
                               :sort-index 8
                               :calculation total-time-booked}
                        })


(defn add-section-items-to-user
  "Calculates the values for all the items in the sections map for a user
  and returns a new user map that has these values assoced into it"
  [db sections user]
  (reduce (fn [user [dashboard-key dashboard-item]]
            (let [uid (:person/userid user)
                  calc-fn (:calculation dashboard-item)
                  value (calc-fn db uid)]
              (assoc user dashboard-key value)))
          user
          (seq sections)))



(defn calculate-section-sums
  "Calculates a total for all the keys in the sections map. This will end up being the 'sum' column."
  [sections users]
  (let [sum (partial reduce +)]
    (reduce (fn [m k]
              (let [summed-values (sum (map #(get % k) users))]
                (assoc m k summed-values)))
            {} (keys sections))))



(defn calculate-user-percentage
  "Calculates the percentages of all section items for a user"
  [user section-key]
  (let [user-sum (get user ::sum)
        user-val (get user section-key)
        percentage (if (zero? user-val)
                     0
                     (/ (* 100 user-val) user-sum))
        formatted-percentage (if (zero? percentage)
                               nil
                               (str (.toFixed percentage 0) \%))]
    (if (number? user-val) ; handles the PCT column edge case
      formatted-percentage
      nil)))



(defn calculate-overall-percentages
  "calculates percentages for section keys based on a section-sums map. This will end up being the PCT column."
  [section-sums]
  (let [all-sums (vals (dissoc section-sums ::sum)) ;;mind the ::sum field...
        overall-sum (reduce + all-sums)
        percentage #(if (zero? %) 0 (/ (* 100 %) overall-sum))
        formatted-percentage #(if (zero? (percentage %)) "-" (str (.toFixed (percentage %) 2) \%))]
    (reduce (fn [m [k v]]
              (assoc m k (formatted-percentage v))) ;; this time we only want the formatted percentage, we don't care about another value
            {}
            section-sums)))



(defn calculate-data-table
  "Performs all the calculations and returns all the 'columns' of the table "
  [db sections user-map]
  (let [calc-users-map (reduce (fn [m [k v]]
                             (assoc m k (add-section-items-to-user db sections v))) {} user-map)
        section-sums (calculate-section-sums sections (vals calc-users-map))
        overall-percentages (calculate-overall-percentages section-sums)]
    (assoc calc-users-map
           :section/sum (assoc section-sums
                               :display-name "SUM"
                               :display-index 99)
           :section/pct (assoc overall-percentages
                               :display-name "PCT"
                               :display-index 100))))


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

(reg-event-db :include-yan?
              (fn [db _]
                (dispatch [:calculate-data-table])
                (update db :include-yan? not)))


(reg-event-db :set-from-to
              (fn [db [_ [from to]]]
                (assoc db
                       :from from
                       :to to)))

(reg-event-db :update-last-fetch
              (fn [db]
                (assoc db :last-fetch (str (:from db) " to " (:to db)))))


(reg-event-db :calculate-data-table
              (fn [db _]
                (let [user-map-with-yan +labsters+
                      user-map-without-yan (dissoc +labsters+ :labster/yan)
                      include-yan? (:include-yan? db)]
                  (assoc db :data-table (calculate-data-table @(:conn db)
                                                              +dashboard-sections+
                                                              (if include-yan?
                                                                user-map-with-yan
                                                                user-map-without-yan))))))

(reg-event-db :new-database
              (fn [db _]
                (let [conn (:conn db)
                      from (:from db)
                      to (:to db)]
                  (new-database conn from to)
                  db)))


;; -- CSV ---------------------------------------------------

(defn data-table-to-vec
  [data-table section-map sections users]
  (let [headers (-> data-table
                    (select-keys users)
                    (vals)
                    ((partial map :display-name)))
        user-map (-> data-table (select-keys users))
        value-rows (for [s sections]
                     (concat
                      [(:display-name (get section-map s))]
                      (map #(get % s) (vals user-map))))]
    (concat [(concat ["-"] headers)]
            value-rows)))


;; -- Subscription Handlers ---------------------------------------------------


(reg-sub :from-to
         (fn [db _]
           (select-keys db [:from :to])))

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

(reg-sub :include-yan?
         (fn [db _]
           (:include-yan? db)))



;; -- Components
(defn data-table []
  (let [data (subscribe [:data-table])
        show-percentages? (subscribe [:show-percentages?])
        from-to (subscribe [:from-to])
        rows (reaction (vals @data))
        sorted-columns (reaction (sort-by :display-index < @rows))


        row-keys (keys +dashboard-sections+)
        name-for-row #(get-in +dashboard-sections+ [% :display-name] )
        click-row-action #(get-in +dashboard-sections+ [% :on-click] )
        click-col-action #(get % :on-click )
        name-for-column #(get % :display-name)
        get-sum #(::sum %)
        value-for-section #(format-hours (get %1 %2))
        ]
    (fn []
      [:table.ui.celled.striped.table
       [:thead
        [:tr
         [:th "-"]
         (doall
          (for [c @sorted-columns]
            (let [on-click (click-col-action c)]
              (conj ^{:key [:header c]}
                    [:th (when on-click
                           {:on-click #(on-click (:from @from-to))})
                     ]
                    (name-for-column c)))))]]
       [:tbody
        (doall
         (for [rk row-keys]
           ^{:key [:tr rk]}
           [:tr
            [:th {:on-click (click-row-action rk)} (name-for-row rk)]
            (doall
             (for [person @sorted-columns]
               (do
                 ^{:key [:td rk person]}
                 [:td (value-for-section person rk)
                  (when @show-percentages?
                    (when-let [pct (calculate-user-percentage person rk)]
                      (str " (" pct ")")))])))]))]])))


(defn dashboard-page []
  (let [from-to (subscribe [:from-to])
        last-fetch (subscribe [:last-fetch])
        loading? (subscribe [:loading?])
        show-percentages? (subscribe [:show-percentages?])
        include-yan? (subscribe [:include-yan?])
        data (subscribe [:data-table])
        csv-billing-matrix-filename (reaction (str "labs-billability_" (:from @from-to) "__" (:to @from-to) ".csv"))
        csv-link (reaction
                  (vec2csv
                   (data-table-to-vec @data
                    +dashboard-sections+
                                      (remove #{::sum} (keys +dashboard-sections+))
                                      (keys +labsters+))))
        ]
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
                                           :on-change #(dispatch [:show-percentages?])}]]

         [:div.row
          "Include Yan: " [:input {:type :checkbox
                                   :value @include-yan?
                                   :default-checked @include-yan?
                                   :on-change #(dispatch [:include-yan?])}]]
         ]

        [:div.col-sm-4.text-right
         [:div.btn-success.btn.btn-lg {:on-click #(when (and (verify-is-date (:from @from-to))
                                                             (verify-is-date (:to @from-to)))
                                                    (do
                                                      (dispatch [:loading? true])
                                                      (dispatch [:new-database])
                                                      ))} "Make it so" ]
         [:hr]
         [:a {:href @csv-link :download @csv-billing-matrix-filename}
          [:div.btn-primary.btn  "Download as CSV" ]]]]

       [:hr]

       (if @loading?
         [:h3 "Loading..."]
         [:div.row
          [:div.col-sm-12
           [data-table]
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
