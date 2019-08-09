(defproject algorithm-data-structure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-kibit "0.1.6"]
            ;; lein cljfmt check/fix
            [lein-cljfmt "0.5.7"]
            [lein-with-env-vars "0.1.0"]]
  ;; use neanderthal: lein with-env-vars repl
  :env-vars {:LD_LIBRARY_PATH "/opt/intel/mkl/lib/intel64:/opt/intel/lib"}
  ;; category thoery to reduce functional programming duplicate codes?
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [uncomplicate/neanderthal "0.21.0"]])
