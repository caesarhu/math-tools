(ns user
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [portal.api :as p]))

(comment
  (p/open)
  (add-tap #'p/submit)
  (tap> :hello)
  )