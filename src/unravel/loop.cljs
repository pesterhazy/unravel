(ns unravel.loop
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.walk]
            [lumo.core]
            [lumo.io :refer [slurp]]
            [cljs.reader :refer [read-string]]
            [unravel.version :as uv]
            [unravel.node :as un]
            [unravel.network :as uw]
            [unravel.terminal :as ut]
            [unravel.tags :as ug]
            [unravel.log :as ud]
            [unravel.util :as uu]
            [unravel.lisp :as ul]
            [unravel.exception :as ue]))

(defn squawk [rl & xs]
  (println)
  (prn (vec xs))
  (.prompt rl true))

(def start-cmd "")

(defn send-command [{:keys [terminating?] :as ctx} s]
  (when-not @terminating?
    (uw/send! (:conn-out ctx) s)))

(defn send-aux-command [{:keys [terminating? aux-out] :as ctx} s]
  (when-not (or @terminating? (nil? aux-out))
    (uw/send! aux-out s)))

(defn set-prompt [{:keys [rl state]} ns warn?]
  (some-> rl (.setPrompt (if (ut/interactive?)
                           (str (or ns (:ns @state))
                                (when warn? "\33[31m")
                                "=> "
                                (when warn? "\33[0m"))
                           ""))))

(defmulti process (fn [command origin ctx] [origin (first command)]))

(defmethod process [:conn :prompt] [[_ opts] _ {:keys [rl state] :as ctx}]
  (let [ns (:form (get opts 'clojure.core/*ns*))]
    (when ns
      (swap! state assoc :ns ns)
      (set-prompt ctx ns false))
    (some-> rl (.prompt true)))
  ctx)

(defn- terminate! [ctx]
  (reset! (:terminating? ctx) true)
  (some-> ctx :conn-out .end)
  (some-> ctx :aux-out .end)
  (some-> ctx :loader-out .destroy)) ; plain .end hangs

(defmethod process [:conn :eval] [[_ result counter] _ ctx]
  (if (and (some? (:trigger ctx)) (= (:trigger ctx) result))
    (terminate! ctx)
    (ut/cyan #(prn result)))
  (assoc ctx :pending-eval nil))

(defmethod process [:conn :started-eval] [[_ {:keys [actions]}] _ ctx]
  (assoc ctx :pending-eval {:action actions}))

(defmethod process [:aux :eval] [[_ result counter] _ {:keys [rl callbacks] :as ctx}]
  (when (and (vector? result) (= :unravel/rpc (first result)))
    (let [[_ eval-id v] result]
      (when-let [f (-> @callbacks (get eval-id))]
        (f v))))
  ctx)

(defmethod process [:conn :exception] [[_ e] _ ctx]
  (ut/red #(println (uu/rstrip-one (with-out-str (ue/print-ex-form (:ex e))))))
  (assoc ctx :pending-eval nil))

(defmethod process [:conn :out] [[_ s] _ {:keys [rl] :as ctx}]
  (.write js/process.stdout s)
  ctx)

(defmethod process [:conn :err] [[_ s] _ {:keys [rl] :as ctx}]
  (ut/yellow #(.write js/process.stdout s))
  ctx)

(defmethod process :default [command _ ctx]
  (ud/dbug :unknown-command command)
  ctx)

(defn cmd-complete [prefix]
  ;; Compliment options:
  ;; - :ns - namespace where completion is initiated;
  ;; - :context - code form around the prefix;
  ;; - :sort-order (either :by-length or :by-name);
  ;; - :plain-candidates - if true, returns plain strings instead of maps;
  ;; - :extra-metadata - set of extra fields to add to the maps;
  ;; - :sources - list of source keywords to use.
  (list 'compliment.core/completions prefix '{:plain-candidates true}))

(defn cmd-doc [word]
  (str "(do (require 'clojure.repl)(clojure.repl/doc " word "))"))

(defn do-doc [ctx line cursor]
  (when-let [word (ul/find-word-at line (max 0 (dec cursor)))]
    (println)
    (send-command ctx (str (cmd-doc word)))))

(defn banner [host port]
  (println (str "Unravel " uv/version " connected to " host ":" port "\n"))
  (println "Type ^O for full docs of symbol under cursor, ^D to quit")
  (println "Enter #__help for help")
  (println))

(defn help []
  (println)
  (println "Type ^O for full docs of symbol under cursor, ^D to quit.")
  (println "Lines starting with `#__` are treated as special commands and
interpreted by the REPL client. The following specials are available:

- `#__help` shows a help screen
- `#__1`, `#__2`, `#__3` ...: expand the numberd lazy seq ellipsis
- `#__`: expand the most recent lazy seq ellipsis ")
  (println))

(defn read-payload []
  (->> ["compliment-blob.clj" "blob.clj"]
       (map #(lumo.io/slurp (un/join-path (or js/process.env.UNRAVEL_HOME ".")
                                          "resources"
                                          "unrepl"
                                          %)))
       (clojure.string/join "\n")))

(defn special [{:keys [conn-out rl] :as ctx} cmd]
  (cond
    (or (= "help" cmd))
    (do
      (help)
      (.prompt rl))
    
    (or (nil? cmd) (re-matches #"^\d*$" cmd))
    (if-let [cmd (get @ug/ellipsis-store (or (some-> cmd js/parseInt) @ug/ellipsis-counter))]
      (send-command ctx (str cmd))
      (.prompt rl))))

(defn socket-connector
  "Returns a function from blob (as string) and terminating? (an boolean atom)
   to streams pairs (as a map with keys :chars-out and :edn-in)."
  [host port]
  (fn connect
    ([blob terminating?]
      (connect blob terminating? "[:unrepl/hello"))
    ([blob terminating? sync-string]
      (let [conn (.Socket. un/net)]
        {:chars-out conn
         :edn-in (-> (doto conn
                       (.connect port
                         host
                         (fn []
                           (when-not @terminating?
                             (.setNoDelay conn true)
                             (ud/dbug :connect (count blob))
                             (.write conn blob)
                             (.write conn "\n"))))
                       (.on "error" (fn [err]
                                      (println "Socket error:" (pr-str err))
                                      (js/process.exit 1))))
                   (.pipe (uw/make-skip sync-string))
                   (.pipe (uw/make-edn-stream)))}))))

(defn call-remote [{:keys [rl callbacks] :as ctx} form cb]
  (let [eval-id (str (gensym))
        cmd (pr-str [:unravel/rpc eval-id form])]
    (swap! callbacks
           assoc
           eval-id
           cb)
    (send-aux-command ctx cmd)))

(defn plausible-symbol? [s]
  (re-matches #"^[*+=?!_?a-zA-Z-.]+(/[*+=?!_?a-zA-Z-.]+)?$" s))

(defn cut [s n]
  (or (some-> (some->> s (re-matches #"^(.{67})(.{3}).*$") second) (str "...")) s))

(defn show-doc [{:keys [rl ostream state] :as ctx} full?]
  (when-let [word (ul/find-word-at (.-line rl) (max 0 (dec (.-cursor rl))))]
    (when (plausible-symbol? word)
      (when (or (not= word (:word @state)) full?)
        (swap! state assoc :word word)
        (call-remote ctx
                     (if full?
                       (list '->>
                             (list 'clojure.repl/doc (symbol word))
                             'with-out-str)
                       (list '->> (list 'clojure.repl/doc (symbol word))
                             'with-out-str
                             '(re-matches (re-pattern "(?is)(.*?\n(.*?\n)?(.*?\n)?(.*?\n)?)(.*)$"))
                             'rest))
                     (fn [r]
                       (if full?
                         (do
                           (println)
                           (.clearScreenDown ostream)
                           (println r)
                           (.prompt rl true))
                         (let [[result more] r]
                           (when result
                             (let [pos (._getCursorPos rl)
                                   lines (str/split-lines (cond-> (str/trimr result)
                                                                       more
                                                                       (str "...")))]
                               (println)
                               (doseq [line lines]
                                 (.clearLine ostream)
                                 (println (cut line 70)))
                               (.moveCursor (js/require "readline")
                                            (.-output rl)
                                            (.-cols pos)
                                            (- (+ (count lines) 1)))))))))))))

(defn complete [ctx line cb]
  (let [word (or (ul/find-word-at line (count line)) "")
        timeout (fn []
                  (println "\n*** completer timed out ***")
                  (cb nil #js[#js[] word]))
        [cb* timeout*] (uu/once-many cb timeout)]
    (call-remote ctx
                 (cmd-complete word)
                 (fn [completions]
                   (cb* nil (clj->js [(map str completions) word]))
                   (show-doc ctx false)))))

(defn interrupt [{:keys [rl pending-eval] :as ctx}]
  (if pending-eval
    (some->> pending-eval :action :interrupt pr-str (send-aux-command ctx))
    (do
      (println)
      (ut/yellow #(println "^C interrupts current evaluation; use ^D to exit."))
      (when (ut/rich?)
        (.clearLine rl))
      (.prompt rl false)))
  ctx)

(defn check-readable-cmd [s]
  (list '(fn [s] (when-not (str/blank? s) (try (read-string s) nil (catch Exception e (.getMessage e))))) s))

(defn check-readable [{:keys [rl state ostream] :as ctx} full?]
  (call-remote ctx
    (check-readable-cmd (.-line rl))
    (fn [ex-str]
      (if full?
        (when ex-str
          (println)
          (.clearScreenDown ostream)
          (ut/red #(println "Cannot read:" ex-str))
          (.prompt rl true))
        (let [warn? (boolean ex-str)]
          (when (not= warn? (boolean (:warn? @state)))
            (swap! state assoc :warn? warn?)
            (set-prompt ctx nil warn?)
            (.prompt rl true)))))))

(defn- state-machine [state-map rf]
  (let [state (volatile! state-map)
        reentrant-calls (volatile! nil)
        sm (fn [& args]
             (if @reentrant-calls
               (vswap! reentrant-calls conj args)
               (try
                 (vreset! reentrant-calls [])
                 (vreset! state
                   (loop [state (apply rf @state args)]
                     (if-some [calls (seq @reentrant-calls)]
                       (do (vreset! reentrant-calls [])
                         (recur (reduce #(apply rf %1 %2) state calls)))
                       state)))
                 (finally
                   (vreset! reentrant-calls nil)))))]
    (vswap! state assoc :sm sm)
    sm))

(defmethod process [:conn :unrepl/hello]
  [[_ {:as session-info {:keys [start-aux :unrepl.jvm/start-side-loader]} :actions}] _ {:keys [sm connect banner] {:keys [cp]} :options :as ctx}]
  (let [{aux-in :edn-in aux-out :chars-out} (connect (pr-str start-aux) (:terminating? ctx))]
    (ud/dbug :main-connection-ready)
    (when (ut/interactive?) (banner))
    (.on aux-in "data" (fn [msg] (sm :aux msg)))
    (cond-> (into ctx {:session-info session-info :aux-in aux-in :aux-out aux-out})
      (seq cp)
      (assoc :loader-out
        (let [{loader-in :edn-in loader-out :chars-out} (connect (pr-str start-side-loader) (:terminating? ctx) "[:unrepl.jvm.side-loader/hello")]
          (.on loader-in "data"
            (fn [[tag payload :as msg]]
              (case tag
                (:class :resource)
                (let [resource (str (if (= :class tag) (str (str/replace payload "." "/") ".class") payload))
                      file (str/replace resource "/" (.-sep un/path))
                      lookup (fn lookup [cp]
                               (if-some [[path & cp] (seq cp)]
                                 (.stat un/fs path (fn [err stats]
                                                     (cond
                                                       err (lookup cp)
                                                       
                                                       ; directory entry on the classpath
                                                       (.isDirectory stats)
                                                       (.readFile un/fs (un/join-path path file)
                                                         (fn [err bytes]
                                                           (if err
                                                             (lookup cp)
                                                             (.write loader-out (prn-str (.toString bytes "base64"))))))
                                                     
                                                       :else ; assumes zip (jar) file
                                                       (-> (.file un/open-jar path)
                                                         (.then (fn [d]
                                                                  (if-some [bytes (->> d .-files (some #(when (= file (.-path %))
                                                                                                          (.buffer %))))]
                                                                    (.then bytes (fn [bytes]
                                                                                   (.write loader-out (prn-str (.toString bytes "base64")))))
                                                                    (lookup cp))))))))
                                 (.write loader-out "nil\n")))]
                  (if (re-find #"^/|(/|^)\.\.(/|$)|\\" resource) ; say no to injection!
                    (.write loader-out "nil\n")
                    (lookup cp)))
                (ud/dbug :sideloader-ignoring msg))))
          loader-out)))))

(defn invoke [template params]
  (pr-str
    (clojure.walk/postwalk
      (fn [x]
        (if (and (tagged-literal? x) (= (:tag x) 'unrepl/param))
          (get params (:form x))
          x))
      template)))

(defmethod process [:aux :unrepl/hello]
  [[_ {:as aux-session-info {:keys [print-limits]} :actions}] _ {:keys [sm aux-out] :as ctx}]
  (ud/dbug :aux-connection-ready)
  (.createInterface un/readline
    #js{:input js/process.stdin
        :output js/process.stdout
        :path (un/join-path (un/os-homedir) ".unravel" "history")
        :maxLength 1000
        :completer (fn [line cb] (sm :readline [:complete [line cb]]))
        :next (fn [rl] (sm :readline [:ready rl]))})
  (ud/dbug :aux-set-limits)
  (uw/send! aux-out (invoke print-limits {:unrepl.print/string-length 0xFFFFffff}))
  ctx)

(defmethod process [:readline :complete]
  [[_ [line cb]] _ ctx]
  (when-some [completer (:completer-fn ctx)]
    (completer ctx line cb))
  ctx)

(defmethod process [:readline :interrupt]
  [_ _ ctx]
  (interrupt ctx))

(defmethod process [:readline :ready]
  [[_ rl] _ {:keys [sm connect] :as ctx}]
  (let [ctx (assoc ctx :rl rl :completer-fn complete)]
    (when-some [ns (some-> ctx :state deref :ns)]
      (set-prompt ctx ns false)
      (.prompt rl))
    (doto rl
      (.on "line" #(sm :readline [:line %]))
      (.on "close" #(sm :readline [:close]))
      (.on "SIGINT" #(sm :readline [:interrupt])))
    (doto (:istream ctx)
      (.on "error" #(.exit js/process 143))
      (.on "keypress" (fn [chunk key] (sm :readline [:keypress [chunk key]]))))
    ctx))

(defmethod process [:readline :line]
  [[_ line] _ ctx]
  (when (ut/rich?)
    (doto (:ostream ctx) .clearLine .clearScreenDown))
  (if-let [[_ cmd] (re-matches #"^\s*#__([a-zA-Z0-9]*)?\s*$" line)]
    (special ctx cmd)
    (send-command ctx line))
  ctx)

(defn- current-time-micros []
  (let [[secs nanos] (.hrtime js/process)]
    (+ (* secs 1e6) (* nanos 1e-3))))

(defmethod process [:readline :keypress]
  [[_ [chunk key]] _ ctx]
  (let [now (current-time-micros)
        is-pasting (< (- now (:last-keypress ctx)) 10000)]
    (cond
      (and (.-ctrl key) (= "o" (.-name key)))
      (do
        #_(check-readable ctx true)
        (show-doc ctx true))
      :else
      (when-not is-pasting
        (check-readable ctx false)
        (show-doc ctx false)))
    (assoc ctx :last-keypress now)))

(defmethod process [:readline :close]
  [_ _ ctx]
  (when (ut/rich?)
    (println))
  (if (ut/interactive?)
    (doto ctx terminate!)
    ; in non-interactive mode we don't want to close until everyhing has been processed
    (let [trigger (keyword "unravel.loop" (gensym "I'm-done!__"))]
      (.emit (:rl ctx) "line" (str trigger))
      (assoc ctx :trigger trigger))))

(defn default-blob-fname []
  (un/join-path (or js/process.env.UNRAVEL_HOME ".") "resources" "unrepl" "blob.clj"))

(defn start [host port {:keys [blobs] :as options}]
  (let [connect (socket-connector host port)
        terminating? (atom false)
        payload (->> (or blobs [(default-blob-fname)])
                     (map lumo.io/slurp)
                     (clojure.string/join "\n"))
        {conn-in :edn-in conn-out :chars-out} (connect payload terminating?)
        sm
        (state-machine {:istream js/process.stdin
                        :ostream js/process.stdout
                        :conn-in conn-in
                        :conn-out conn-out
                        :terminating? terminating?
                        :callbacks (atom {})
                        :pending-eval nil
                        :state (atom {})
                        :connect connect
                        :banner #(banner host port)
                        :options options
                        :last-keypress (current-time-micros)}
          (fn [ctx origin msg]
            (ud/dbug :receive {:origin origin} msg)
            (process msg origin ctx)))]
    (.on conn-in "data" #(sm :conn %))))
