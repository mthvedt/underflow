(defproject eightnotrump/underflow "0.1.0-SNAPSHOT"
  :description "Fast continuations that work."
  :profiles {:bench-detail {:gloabl-vars {*warn-on-reflection* true}
                            :test-paths ["perf"]
                            :jvm-opts ^:replace ["-XX:+PrintCompilation"
                                                 "-XX:+UnlockDiagnosticVMOptions"
                                                 "-XX:+PrintInlining"]}
             :bench {:gloabl-vars {*warn-on-reflection* true}
                     :test-paths ["perf"]
                     :jvm-opts ^:replace []}}
  :aliases {"bench-detail" ["with-profile" "bench-detail" "test"]
            "bench" ["with-profile" "bench" "test"]}
  :java-source-paths ["java"]
  :dependencies [[criterium "0.4.3" :scope "test"]
                 [org.clojure/clojure "1.6.0"]])
