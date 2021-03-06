(defproject feeds2imap "0.3.1"
  :description "Pull RSS/Atom feeds to your IMAP folders with Clojure on JVM."
  :url "https://github.com/Gonzih/feeds2imap.clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :core.typed {:check [feeds2imap.core
                       feeds2imap.feeds
                       feeds2imap.folder
                       feeds2imap.imap
                       feeds2imap.message
                       feeds2imap.settings]}
  :dependencies [[org.clojure/clojure "1.7.0-RC2"]
                 [javax.mail/mail "1.4.7"]
                 [org.clojars.scsibug/feedparser-clj "0.4.0"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/core.typed "0.3.8-SNAPSHOT" :classifier "slim"]
                 [hiccup "1.0.5"]
                 [digest "1.4.4"]
                 [com.taoensso/timbre "2.1.2"]
                 [org.clojure/core.match "0.2.2"]]
  ;:repl-options {:nrepl-middleware [clojure.core.typed.repl/wrap-clj-repl]}
  ;:main feeds2imap.core
  :profiles {:dev {:dependencies [[midje "1.6.3"]
                                  [org.clojure/test.check "0.7.0"]
                                  #_[com.taoensso/timbre "2.1.2"]]
                   :plugins [[lein-midje "3.1.3"]]
                   }
             :uberjar {:aot :all}}
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

  :min-lein-version "2.0.0")
