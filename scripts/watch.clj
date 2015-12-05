(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'el-sistema.core
   :output-to "out/el_sistema.js"
   :output-dir "out"})
