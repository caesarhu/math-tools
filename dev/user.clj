(ns user
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [portal.api :as p]))

(def portal
  ((requiring-resolve 'portal.api/open)
   {:launcher :vs-code
    :host (-> (sh/sh "hostname" "-I") :out (str/trim))
    :portal.launcher/window-title (System/getProperty "user.dir")}))

(comment
  (add-tap #'p/submit)
  (tap> :hello)
  )