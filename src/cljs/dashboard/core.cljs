(ns dashboard.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [cljs-http.client :as http]
              [cljs.core.async :refer [<! put! take!]]
              [datascript.core :as d]
              [cljs.pprint :as pprint]
              [goog.string :as gstring]
              [goog.string.format]
              ))

(enable-console-print!)


(defonce conn (d/create-conn nil))


(d/listen! conn
           (fn [tx-report]
             ;; (prn tx-report)
             (reagent/force-update-all)))


;;------- get data
(defn <time-entry
  ""
  [start end userids]
  (let [userids* (if (coll? userids) userids (vector userids))]
    (->  (str "https://genome.klick.com/api/TimeEntry.json"
              "?EndDate=" end "T23:59:59-04:00"
              "&StartDate=" start "T00:00:00-04:00"
              "&UserIDs=[" (apply str (interpose \,  userids*)) "]")
         (http/jsonp))))

(defn transact-time-entries! [db start end userids]
  (take! (<time-entry start end userids) (fn [data]
                                          (->> data
                                               :body
                                               :Entries
                                               (d/transact! db)))))


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


;;----------- db stuff

(defn transact-users! [db users]
  (doseq [l users]
    (d/transact! db [{:person/name (first l)
                      :person/userid (second l)}])))

(when (= @(d/create-conn nil) (d/empty-db))
  (transact-users! conn +labsters+ )
  (transact-time-entries! conn "2016-11-21" "2016-11-25" labster-ids)
  )

(defn format-hours [hrs]
  ;; (/ (Math/round (* 100 (/ min 60))) 100)
  (cond
    (zero? hrs) "-"
    (number? hrs) (.toFixed hrs 2)
    (string? hrs) hrs))


(comment
  (prn
   (count
    (d/q '[:find ?name ?duration
           :where
           [?p :ProjectName ?name]
           [?p :Duration ?duration]
           [(not= ?name "Klick Inc. - Klick Labs - Studies")]
           [(not= ?name "Klick - Internal Project")]
           ]
         @conn
         )))


 #_(d/q '[:find ?name ?duration
        :in $ min-to-hours
        :where
        [?p :ProjectName ?name]
        [?p :Duration ?d]
        [(min-to-hours ?d) ?duration]]
      @conn
      #'min-to-hours))

;;---- QUERIES

#_(defn total-hours-spent [db name]
  (d/q '[:find (sum ?duration) .
         :in $ min-to-hours ?name
         :where
         [?l :person/name ?name]
         [?l :person/userid ?id]
         [?p :UserID ?id]
         [?p :ProjectName ?proj]
         [?p :Duration ?d]
         [(min-to-hours ?d) ?duration]
         ]
       @conn
       #'min-to-hours
       name))

(defn time-spent-on-projs [name]
  (d/q '[:find ?p (sum ?duration) .
         :in $ ?name
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

;; Duration by project
(defn q [query]
  (prn
   (d/q query
        @conn
        )))

(comment
  (q '[:find ?name ?project (sum ?mins)
       :where
       [?p :person/name ?name]
       [?p :person/name "Max"]
       [?p :person/userid ?id]
       [?x :ProjectName ?project]
       [? :UserID ?id]
       [?x :Duration ?mins]]))


;;;     :IsClientBillable true, ?
(def +lab-projects+ {"Labs Billable" 23409
                     "Administration"  16897
                     "Studies" 23405
                     "Experiments" 23404
                     "Promo" 22295})


;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to dashboard"]
   [:div [:a {:href "/about"} "go to about page"]]])

(defn about-page []
  [:div [:h2 "About dashboard"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])


(defn billing-matrix
  ""
  []
  (let [
        labs-billable (map (partial time-spent-on-projid (get +lab-projects+ "Labs Billable")) labster-ids)
        others-billable (repeat 9 0)
        administration (map (partial time-spent-on-projid (get +lab-projects+ "Administration")) labster-ids)
        experiments (map (partial time-spent-on-projid (get +lab-projects+ "Experiments")) labster-ids)
        studies (map (partial time-spent-on-projid (get +lab-projects+ "Studies")) labster-ids)
        personal-sum #("todo")
        sum (partial reduce +)
        sum-all (reduce + (map sum [labs-billable others-billable administration experiments studies]))
        percentall #(str (.toFixed (/ (* 100 (sum %)) sum-all) 2) \%)
        todo "todo"]
  `[
    ["-" ~@labster-names "SUM" "PCT"]
    ["Labs Billable" ~@labs-billable ~(sum labs-billable) ~(percentall labs-billable)]
    ["Others Billable" ~@others-billable ~(sum others-billable) ~(percentall others-billable)]
    ["Administration" ~@administration ~(sum administration) ~(percentall administration)]
    ["Experiments" ~@experiments ~(sum experiments) ~(percentall experiments)]
    ["Studies" ~@studies ~(sum studies) ~(percentall studies)]
    ["SUM" ~@(repeat 9 todo) ~sum-all "100% (hopefully)" ]
    ]))


(defn dashboard-page []
  (let [matrix (billing-matrix)
        matrix-row #(let [r (get matrix %)]
                      (vector :tr
                              (vector :th (first r))
                              (map (comp (partial vector :td)
                                         (partial format-hours))
                                   (rest (butlast r)))
                              (vector :td (last r))))]
    [:div
     [:table.table.table-hover.table-bordered ; {:class "table table-striped"}
      [:thead
       [:tr
        (map (partial vector :th) (get matrix 0))
       ]]

      [:tbody
       (matrix-row 1)
       (matrix-row 2)
       (matrix-row 3)
       (matrix-row 4)
       (matrix-row 5)
       (matrix-row 6)

       ]]]))

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'dashboard-page))

(secretary/defroute "/about" []
  (session/put! :about-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
