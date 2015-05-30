(ns ^:core.typed feeds2imap.types
  (:require [clojure.core.typed :refer [defalias Set HMap Seqable Vec Map Keyword Option Any TFn Num Coll U]])
  (:import [javax.mail.internet MimeMessage]
           [java.net URL]
           [java.util Date]))

(defalias Cache (Map String Num))

;; FIXME correct type?
(defalias Link String)
;; FIXME correct type?
(defalias Title String)

;; FIXME correct type?
(defalias Author
  '{:name (U nil Title)
    :uri  (U nil Link)})

;; FIXME correct type?
(defalias Item
  (HMap :mandatory {:authors (Coll Author)
                    :contents (Coll (HMap :optional {:value String}))
                    :title Title
                    :uri (U nil String)
                    :url (U nil String)
                    :link (U nil Link)
                    :updated-date (U nil Date)
                    :published-date (U nil Date)
                    :description (HMap :optional {:value String})}))

(defalias Items (Coll Item))
(defalias UnflattenedItems (Coll Items))

(defalias ParsedFeed
  (HMap :mandatory {:entries Items}
        :optional {:title Title
                   :link Link}))

(defalias Message MimeMessage)
(defalias Messages (Seqable Message))

(defalias Urls (Vec String))

(defalias Folder
  (TFn [[x :variance :covariant]] (Map Keyword x)))

(defalias FolderSeq
  (TFn [[x :variance :covariant]] (Coll '[Keyword x])))

(defalias MessageMap
  (HMap :mandatory {:from    String
                    :to      String
                    :subject String
                    :html    String}
        :optional  {:date (Option Date)}))

(defalias XML (Map Keyword Any))

(defalias ImapConfiguration
  (HMap :mandatory {:host String
                    :port Num
                    :username String
                    :password String}
        :optional {:to String
                   :from String}))

(defalias ShellResult
  (HMap :mandatory {:exit Num}
        :optional {:err String
                   :out String}))
