(ns feeds2imap.profile-types
  (:require [midje.sweet :refer :all]
            [clojure.core.typed :refer [check-ns]]))

(defn check-ns-spit [ns-symbol]
  (let [result (atom nil)]
    (spit
      "out"
      (with-out-str
        (binding [*err* *out*]
          (let [return-value (check-ns ns-symbol :trace true :profile true)]
            (reset! result return-value)))))
    @result))

(fact "about types"
      (check-ns-spit '[feeds2imap.opml
                       feeds2imap.core
                       feeds2imap.feeds
                       feeds2imap.settings
                       feeds2imap.imap
                       feeds2imap.settings
                       feeds2imap.folder
                       feeds2imap.message]) => :ok)
