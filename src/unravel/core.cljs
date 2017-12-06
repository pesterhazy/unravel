(ns unravel.core
  (:require [clojure.spec.alpha :as spec]
            [cljs.reader]
            [unravel.log :as ud]
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

(defn parse-arg [m [arg nxt :as args]]
  (let [switch (fn [test-fn kw]
                 (when (test-fn arg)
                   [(assoc m kw true) (rest args)]))
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
  "Syntax: unravel [--debug] [-c|--classpath <paths>] [--blob blob1 [--blob blob2 ...]] [--flag flag1 [--flag --flag2 ...]] [<host> <port>]\n        unravel --version")

(defn jack-in [cb]
  (let [pr (.spawn (js/require "child_process")
                   "bash" #js ["scripts/jack-in"])
        sb (StringBuffer.)]
    (-> pr .-stdout (.on "data" (fn [data]
                                  (.append sb (.toString data))
                                  (if-let [match (re-find #"\[:jack-in/ready.*" (.toString sb))]
                                    (let [[tag {:keys [port]} :as msg] (cljs.reader/read-string match)]
                                      (ud/dbug :jack-in/response msg)
                                      (when (not= tag :jack-in/ready)
                                        (throw (js/Error. "Could not parse :jack-in/ready message")))
                                      (cb port))))))))

(defn -main [& more]
  (init)
  (let [{:keys [version? debug? positional] :as args} (parse-args more)]
    (when version? (print-version!))
    (when debug? (reset! ud/debug? true))
    (let [jack-in? (case (count positional)
                     2 false
                     0 true
                     (throw (js/Error. "You need to pass 0 or 2 positional arguments")))
          start-fn (fn [host port]
                     (uo/start (or host "localhost")
                               port
                               args))]
      (if jack-in?
        (jack-in (fn [port] (start-fn "localhost" port)))
        (start-fn (first positional) (second positional))))))
