(defproject dwarves "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[environ "0.4.0"]
                 [org.clojure/clojure "1.5.1"]
                 [overtone "0.9.1"]
                 [overtone.orchestra "0.1.0-SNAPSHOT"]
                 [quil "1.6.0"]]
  :jvm-opts ^:replace ["-Xmx512m" "-server"])
