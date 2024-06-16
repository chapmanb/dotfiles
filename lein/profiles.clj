{:user {:dependencies  [
                        [hashp "0.2.0"]
                        [io.aviso/pretty "1.1"]
                        ;; [com.cognitect/REBL "0.9.240"]
                        ;; [djblue/portal  "0.8.0"]
                        ]
        :plugins [[io.aviso/pretty "1.1"]]
        :injections  [(require 'hashp.core)]
        :middleware [io.aviso.lein-pretty/inject]
        }
 :repl  {:plugins  [[cider/cider-nrepl "0.28.4"]
                    [mx.cider/enrich-classpath "1.9.0"]]}}
