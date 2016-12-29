(ns dashboard.spec
  (:require [cljs.spec :as s]))

(s/def ::value number?)
(s/def ::formatted-value string?)
(s/def ::percentage number?)
(s/def ::formatted-percentage string?)
(s/def ::display-name string?)
(s/def ::display-index number?)
(s/def ::section (s/keys :req-un [::value ::formatted-value]
                         :opt-un [::percentage ::formatted-percentage]))


;; (s/def ::person (s/keys req-un []))
