(ns unravel.core
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as spec]
            [unravel.log :as ul]
            [unravel.version :as uv]
            [unravel.loop :as uo]
            [unravel.node :as un])
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

(defn parse-args [args]
  (loop [m {}
         [arg nxt :as args] args]
    (cond
      (nil? arg)
      m

      (= "--version" arg)
      (recur (assoc m :version? true) (rest args))

      (= "--debug" arg)
      (recur (assoc m :debug? true) (rest args))

      (#{"--classpath" "-c"} arg)
      (do
        (assert (some? nxt) "Needs parameter")
        (recur (update m :cp (fn [elements] (conj (or elements []) nxt))) (rest (rest args))))

      (#{"--blob"} arg)
      (do
        (assert (some? nxt) "Needs parameter")
        (recur (update m :blobs (fn [elements] (conj (or elements []) nxt))) (rest (rest args))))

      (#{"--flag"} arg)
      (do
        (assert (some? nxt) "Needs parameter")
        (recur (update m :flags (fn [elements] (conj (or elements #{}) nxt))) (rest (rest args))))

      (.startsWith arg "-")
      (throw (js/Error. (str "Unknown argument: " arg)))

      :else
      (recur (update m :positional (fn [xs] (conj (or xs []) arg)))
             (rest args)))))

(def help-text
  "Syntax: unravel [--debug] [-c|--classpath <paths>] [--blob blob1 [--blob blob2 ...]] [--flag flag1 [--flag --flag2 ...]] [<host>] <port>\n        unravel --version")

(defn -main [& more]
  (init)
  (let [{:keys [version? debug? positional] :as args} (parse-args more)
        [host port] positional]
    (when version? (print-version!))
    (when debug? (reset! ul/debug? true))
    (when-not (= 2 (count positional))
      (throw (js/Errorr. "You need to pass exactly 2 arguments")))
    (uo/start (or host "localhost")
              port
              args)))
