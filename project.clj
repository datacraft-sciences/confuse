(defproject confuse "0.1.0-SNAPSHOT"
  :description "Utilities for Data Science"
  :url "https://github.com/datacraft-sciences/confuse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [net.mikera/core.matrix "0.57.0"]
                 [com.rpl/specter "1.0.0"]
                 [incanter/incanter-core "1.9.1"]
                 ]
  :profiles  {:dev  {:dependencies  [[org.clojure/test.check "0.9.0"]]}}
  )
