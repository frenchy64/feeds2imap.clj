(ns feeds2imap.feeds-test
  (:require [midje.sweet :refer :all]
            [feeds2imap.feeds :refer :all]
            [clojure.core.typed :refer [check-ns]]))

(fact "about types"
  (check-ns 'feeds2imap.feeds) => :ok)

(with-redefs [digest/md5 identity]
  (fact "about reduce-new-items-test"
    (fact "it properly calculates new items"
      (:new-items (reduce-new-items #{"a" "b"}
                                    {:b [{:uri "c"}]
                                     :a [{:uri "b"} {:uri "z"}]}))
      => {:b [{:uri "c"}] :a [{:uri "z"}]})
    (fact "it properly updates cache"
      (:cache (reduce-new-items #{"a" "b"}
                                {:b [{:uri "c"} {:uri "d"}]
                                 :a [{:uri "b"} {:uri "z"}]}))
      => #{"a" "b" "c" "d" "z"})
    (fact "it preffers https urls in cache"
      (let [{:keys [cache new-items]}
            (reduce-new-items #{} {:b [{:uri "http://b.com"} {:uri "https://b.com"}]
                                   :a [{:uri "http://a.com"} {:uri "https://a.com"}]})]
        (count cache) => 2
        (count (:a new-items)) => 1
        (count (:b new-items)) => 1)))
  (fact "about uniq-identifier"
    (fact "it properly replaces http with https"
      (uniq-identifier {:uri "http://a.com"}) => "https://a.com"))
  (fact "about new?"
    (fact "it detects item in cache"
      (new? #{"https://abc"} {:uri "https://cba"}) => true
      (new? #{"https://abc"} {:uri "https://abc"}) => false)))

(let [run (partial into {})]
  (let [urls {:a [{:uri "a"} {:uri "b"}]
              :b [{:uri "c"} {:uri "d"}]}
        map-result {:a ["a" "b"]
                    :b ["c" "d"]}]
    (fact "about map-items"
      (fact "it maps function over items in folders"
        (run (map-items :uri urls)) => map-result))
    (fact "about pmap-items"
      (fact "it maps function over items in folders"
        (run (pmap-items :uri urls)) => map-result)))

  (let [urls {:a [{:uri "a"} {:uri nil}]
              :b [{:uri nil} {:uri "d"}]}
        filter-result {:a [{:uri "a"}]
                       :b [{:uri "d"}]}]
    (fact "about filter-items"
      (fact "it filters items in folders"
        (run (filter-items :uri urls)) => filter-result)))

  (let [urls {:a [[{:uri "a"}] [{:uri "b"}]]
              :b [[{:uri "c"} {:uri "d"}]]}
        flatten-result {:a [{:uri "a"} {:uri "b"}]
                        :b [{:uri "c"} {:uri "d"}]}]
    (fact "about flatten-items"
      (fact "it flattens items in folders"
        (run (flatten-items urls)) => flatten-result))))
