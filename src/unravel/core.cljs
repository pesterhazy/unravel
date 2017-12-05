(ns unravel.core
  (:require [clojure.spec.alpha :as spec]
            [unravel.log :as ul]
            [unravel.version :as uv]
            [unravel.loop :as uo])
  (:import [goog.string StringBuffer]))

(defn fail [message]
  (println message)
  (js/process.exit 1))

(defn print-version! []
  (println "Unravel" uv/version (str "(Lumo " lumo.core/*lumo-version* ")"))
  (js/process.exit 0))

(defn init [])

(spec/def ::cmdline-args
  (spec/cat
   :options (spec/* (spec/alt :version #{"--version"}
                              :debug #{"--debug"}
                              :cp (spec/& (spec/cat :_ #{"--classpath" "-c"} :path string?) (spec/conformer #(:path %)))
                              :blobs (spec/& (spec/cat :_ #{"--blob"} :path string?) (spec/conformer #(:path %)))
                              :flags (spec/& (spec/cat :_ #{"--flag"} :path string?) (spec/conformer #(:path %)))))
   :host (spec/? string?) :port (spec/and string? #(re-matches #"\d+" %))))

(defn parse-arg [m [arg nxt :as args]]
  (let [switch (fn [test-fn kw]
                 (when (test-fn arg)
                   [(assoc m :version? true) (rest args)]))
        mult (fn mult
               ([test-fn kw] (mult test-fn kw [] identity))
               ([test-fn kw empty-coll] (mult test-fn kw empty-coll identity))
               ([test-fn kw empty-coll val-fn]
                (when (test-fn arg)
                  (assert (some? nxt) "Needs parameter")
                  [(update m kw (fn [elements] (conj (or elements empty-coll) (val-fn nxt)))) (rest (rest args))])))]
    (or
     (switch #{"--version"} :version?)
     (switch #{"--debug"} :debug?)
     (mult #{"--classpath" "-c"} :cp)
     (mult #{"--blob"} :blobs)
     (mult #{"--flag"} :flags #{} keyword))))

(defn parse-args [args]
  (loop [m {}
         [arg :as args] args]
    (if-let [[m* args*] (parse-arg m args)]
      (recur m* args*)
      (cond
        (nil? arg)
        m

        (.startsWith arg "-")
        (throw (js/Error. (str "Unknown argument: " arg)))

        :else
        (recur (update m :positional (fn [xs] (conj (or xs []) arg)))
               (rest args))))))

(def help-text
  "Syntax: unravel [--debug] [-c|--classpath <paths>] [--blob blob1 [--blob blob2 ...]] [--flag flag1 [--flag --flag2 ...]] [<host>] <port>\n        unravel --version")

(defn -main [& more]
  (init)
  (let [{:keys [version? debug? positional] :as args} (parse-args more)
        [host port] positional]
    (when version? (print-version!))
    (when debug? (reset! ul/debug? true))
    (when-not (= 2 (count positional))
      (throw (js/Error. "You need to pass exactly 2 arguments")))
    (uo/start (or host "localhost")
              port
              args)))
