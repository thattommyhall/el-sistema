(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'cc-practice.core
   :output-to "out/cc_practice.js"
   :output-dir "out"})
