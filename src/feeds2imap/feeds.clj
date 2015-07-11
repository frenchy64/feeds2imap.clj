(ns ^:core.typed feeds2imap.feeds
  (:refer-clojure :exclude [fn let])
  (:require [hiccup.core :refer :all]
            [feedparser-clj.core :refer :all]
            [feeds2imap.message :as message]
            [clojure.data.codec.base64 :as b64]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [feeds2imap.logging :refer [info error]]
            [feeds2imap.macro :refer :all]
            [clojure.core.typed :refer [ann Map IFn HMap fn let] :as t]
            [feeds2imap.types :refer :all]
            [feeds2imap.annotations :refer :all]
            [digest :refer [md5]]
            [clojure.string :as s])
  (:import  [java.lang IllegalArgumentException]
            [java.security MessageDigest]
            [java.net NoRouteToHostException ConnectException UnknownHostException]
            [java.io IOException]
            [javax.mail Session]
            [javax.mail.internet MimeMessage]
            [com.sun.syndication.io ParsingFeedException]
            [java.util Date]))

(ann map-items 
     (t/All [v v']
        [[v -> v'] (FolderSeq (t/Coll v)) -> (FolderSeq (t/Coll v'))]))
(defn map-items
  "Map function over items for each folder."
  [fun coll]
  (map (fn [[folder items] :- '[t/Kw (t/Seqable v)]] 
         [folder (map fun items)])
       coll))

(ann pmap-items 
     (t/All [v v']
        [[v -> v'] (FolderSeq (t/Coll v)) -> (FolderSeq (t/Coll v'))]))
(defn pmap-items
  "Map function over items for each folder using pmap."
  [fun coll]
  (pmap (fn [[folder items] :- '[t/Kw (t/Seqable v)]] 
          [folder (pmap fun items)])
        coll))

(ann filter-items [(IFn [Item -> Boolean]) (FolderSeq Items) -> (FolderSeq Items)])
(defn filter-items
  "Filter items for each folder.
   Filter folders with non empty items collection."
  [fun coll]
  (->> coll
       (map (fn [[folder items] :- '[t/Kw Items]]
              [folder (filter fun items)]))
       (filter (fn [[folder items] :- '[t/Kw Items]]
                 (seq items)))))

(ann ^:no-check flatten-items [(FolderSeq UnflattenedItems) -> (FolderSeq Items)])
(defn flatten-items [items]
  (map (fn [[folder items] :- '[t/Kw UnflattenedItems]]
         (let [c (t/tc-ignore (apply concat items))
               ;_ (assert ((t/pred Items) c))
               ]
           [folder c]))
       items))

(ann item-authors [Item -> String])
(defn item-authors [{:keys [authors]}]
  "Format each author as
   \"Name <name[at]example.com> http://example.com/\".
   Multiple authors are coma-separated"
  (let [format-author
        (fn [author :- Author]
          (let [{:keys [name email uri]} author
                email (when email
                        (t/tc-ignore
                          (format "<%s>" (apply str (replace {\@ "[at]"} email)))))
                _ (assert ((t/pred (t/U nil String)) email))
                fields (filter (complement nil?)
                               [name email uri])]
            (s/join " " fields)))]
    (s/join ", " (map format-author authors))))

(ann uniq-identifier [Item -> String])
(defn uniq-identifier
  "Generates unique identifier for item.
   First try uri, then url, then link.
   Only if items listed above are empty use md5 of title + link + authors."
  [{:keys [title uri url link] :as item}]
  (s/replace (or uri url link (str title link (item-authors item)))
             #"http://" "https://"))

(ann md5-identifier [Item -> String])
(defn md5-identifier [item]
  (-> item uniq-identifier md5))

(ann new? [Cache Item -> Boolean])
(defn new?
  "Looks up item in the cache"
  [cache item]
  (not (contains? cache (md5-identifier item))))

(ann item-content [Item -> String])
(defn item-content [item]
  (str (or (-> item :contents first :value)
           (-> item :description :value))))

(ann encoded-word [String -> String])
(defn ^:private encoded-word
  "Encodes From: field. See http://en.wikipedia.org/wiki/MIME#Encoded-Word"
  [^String s]
  (let [^bytes encode64 (t/tc-ignore 
                          (b64/encode (.getBytes s "UTF-8")))
        encoded-text (t/tc-ignore
                       (String. encode64))]
    (str "=?UTF-8?B?" encoded-text "?=")))

(ann item-pubdate [Item -> (t/U nil Date)])
(defn item-pubdate [item]
  (or (:updated-date item) (:published-date item)))

(ann to-email-map [String String Item -> MessageMap])
(defn to-email-map
  "Convert item to map for Message construction."
  [from to item]
  (let [{:keys [title link]} item
        authors (item-authors item)
        content (item-content item)
        from+   (s/join " " [(encoded-word authors) (str "<" from ">")])
        pubdate (item-pubdate item)
        html (html [:table
                    [:tbody [:tr [:td [:a {:href link} title] [:hr]]]
                     (when (seq authors)
                       [:tr [:td authors [:hr]]])
                     [:tr [:td content]]]])]
    {:from from+ :to to :date pubdate :subject title :html html}))

(ann items-to-emails [Session String String Item -> Message])
(defn items-to-emails [session from to item]
  (message/from-map session (to-email-map from to item)))

(ann to-emails [Session String String (FolderSeq Items) -> (FolderSeq Messages)])
(defn to-emails
  "Convert items to Messages."
  [session from to items]
  (map-items (t/ann-form
               (partial items-to-emails session from to)
              [Item -> Message])
             items))

(ann set-entries-authors [ParsedFeed -> ParsedFeed])
(defn set-entries-authors [feed]
  (let [feed-as-author :- Author, {:name (:title feed) :uri (:link feed)}
        set-authors (fn [e :- Item] :- Item
                      (if (seq (:authors e))
                        e
                        (assoc e :authors [feed-as-author])))
        entries (map set-authors (:entries feed))]
    (assoc feed :entries entries)))

(ann parse [String -> ParsedFeed])
(defn parse [url]
  (let [log-try (fn [url    :- String
                     n-try  :- t/Num
                     reason :- (t/U t/Kw Exception)]
                  (if (> n-try 1)
                    (error "Fetching" url "try" n-try "reason is" reason)
                    (info  "Fetching" url)))
        parse-try
        (fn parse-try
          ([url :- String] :- ParsedFeed
           (parse-try url 1 :no-reason))
          ([url    :- String
            n-try  :- t/Num
            reason :- (t/U t/Kw Exception)] :- ParsedFeed
           (log-try url n-try reason)
           (try*
             (if (< n-try 3)
               (-> url parse-feed set-entries-authors)
               {:entries ()})
             (catch* [ConnectException
                      NoRouteToHostException
                      UnknownHostException
                      ParsingFeedException
                      IllegalArgumentException
                      IOException] e (parse-try url (inc n-try) e)))))]
    (parse-try url)))

(t/defalias NewItems
  (HMap :mandatory {:new-items (Folder Items)
                    :cache Cache}))

(ann ^:no-check reduce-new-items [Cache (FolderSeq Items) -> NewItems])
(defn reduce-new-items [cache parsed-feeds]
  (reduce (fn [{:keys [cache new-items] :as val} :- NewItems
                 [folder items] :- '[t/Kw Items]]
            (reduce (fn [{:keys [cache] :as val} :- NewItems
                           item :- Item]
                      ;{:post [((t/pred NewItems) %)]}
                      (if (new? cache item)
                        (t/tc-ignore
                          (-> val
                              (update-in [:cache] (fn [new-cache] (assoc new-cache (md5-identifier item) (System/currentTimeMillis))))
                              (update-in [:new-items folder] (fn [new-items] (conj new-items item)))))
                        val))
                    val
                    items))
          {:cache cache :new-items {}}
          parsed-feeds))

(ann new-items [Cache (Folder Urls) -> NewItems])
(defn new-items [cache urls]
  (->> urls
       (pmap-items parse)
       (map-items (t/ann-form :entries [ParsedFeed -> Items]))
       flatten-items
       (reduce-new-items cache)))
