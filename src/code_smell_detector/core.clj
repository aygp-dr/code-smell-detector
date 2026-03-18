(ns code-smell-detector.core
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [cheshire.core :as json]))

;; --- Utility ---

(defn file-extension [path]
  (let [name (str (fs/file-name path))]
    (when-let [idx (str/last-index-of name ".")]
      (subs name (inc idx)))))

(def ext->lang
  {"py" "python" "pyw" "python"
   "js" "javascript" "mjs" "javascript" "cjs" "javascript"
   "ts" "javascript" "tsx" "javascript"
   "java" "java"
   "go" "go"
   "rb" "ruby"
   "c" "c" "cpp" "cpp" "h" "c" "hpp" "cpp"
   "rs" "rust"})

(defn indentation-level [line]
  (when-not (str/blank? line)
    (- (count line) (count (str/triml line)))))

(defn- strip-comments [line lang]
  (cond
    (#{"python" "ruby"} lang) (str/replace line #"#.*$" "")
    (#{"javascript" "java" "go" "c" "cpp" "rust"} lang) (str/replace line #"//.*$" "")
    :else line))

;; --- Detector: Long Methods (>30 lines) ---

(def ^:private function-patterns
  {"python"     #"^\s*def\s+(\w+)\s*\("
   "javascript" #"(?:function\s+(\w+)|(?:const|let|var)\s+(\w+)\s*=\s*(?:async\s+)?function)\s*\("
   "java"       #"(?:public|private|protected|static|\s)+\w+(?:<[^>]+>)?\s+(\w+)\s*\("
   "go"         #"func\s+(?:\([^)]+\)\s+)?(\w+)\s*\("
   "ruby"       #"^\s*def\s+(\w+)"
   "rust"       #"(?:pub\s+)?fn\s+(\w+)"
   "c"          #"^\s*(?:\w+[\s*]+)+(\w+)\s*\([^;]*$"
   "cpp"        #"^\s*(?:\w+[\s*]+)+(\w+)\s*\([^;]*$"})

(defn- find-functions [lines lang]
  (when-let [pattern (get function-patterns lang)]
    (->> lines
         (map-indexed vector)
         (keep (fn [[idx line]]
                 (when-let [m (re-find pattern line)]
                   (let [groups (if (string? m) [m] (vec m))
                         name (or (some identity (rest groups)) "unknown")]
                     {:name name :start-line idx :indent (or (indentation-level line) 0)})))))))

(defn- function-length-by-indent [lines start indent]
  (let [body (drop (inc start) lines)]
    (inc (count (take-while
                 (fn [line]
                   (let [ind (indentation-level line)]
                     (or (nil? ind) (> ind indent))))
                 body)))))

(defn- function-length-by-braces [lines start]
  (loop [idx start depth 0 started false]
    (if (>= idx (count lines))
      (- idx start)
      (let [line (nth lines idx)
            stripped (-> line
                        (str/replace #"\"(?:[^\"\\\\]|\\\\.)*\"" "")
                        (str/replace #"'(?:[^'\\\\]|\\\\.)*'" "")
                        (str/replace #"//.*$" ""))
            opens (count (re-seq #"\{" stripped))
            closes (count (re-seq #"\}" stripped))
            new-depth (+ depth opens (- closes))
            now-started (or started (pos? opens))]
        (if (and now-started (<= new-depth 0))
          (inc (- idx start))
          (recur (inc idx) new-depth now-started))))))

(defn detect-long-methods
  ([lines lang file] (detect-long-methods lines lang file {}))
  ([lines lang file {:keys [threshold] :or {threshold 30}}]
   (let [fns (find-functions lines lang)
         indent-based? #{"python" "ruby"}]
     (->> fns
          (keep (fn [{:keys [name start-line indent]}]
                  (let [length (if (indent-based? lang)
                                 (function-length-by-indent lines start-line indent)
                                 (function-length-by-braces lines start-line))]
                    (when (> length threshold)
                      {:file file :line (inc start-line) :smell "long-method" :severity "high"
                       :message (format "Method '%s' is %d lines (threshold: %d)"
                                        name length threshold)}))))))))

;; --- Detector: Deep Nesting (>4 levels) ---

(defn detect-deep-nesting
  ([lines lang file] (detect-deep-nesting lines lang file {}))
  ([lines lang file {:keys [threshold] :or {threshold 4}}]
   (if (#{"python" "ruby"} lang)
     (->> lines
          (map-indexed vector)
          (keep (fn [[idx line]]
                  (when-let [indent (indentation-level line)]
                    (let [depth (quot indent 4)]
                      (when (> depth threshold)
                        {:file file :line (inc idx) :smell "deep-nesting" :severity "medium"
                         :message (format "Nesting depth %d exceeds threshold %d"
                                          depth threshold)}))))))
     ;; Brace-based languages
     (loop [idx 0 depth 0 results []]
       (if (>= idx (count lines))
         results
         (let [line (nth lines idx)
               stripped (-> line
                           (strip-comments lang)
                           (str/replace #"\"[^\"]*\"" "")
                           (str/replace #"'[^']*'" ""))
               opens (count (re-seq #"\{" stripped))
               closes (count (re-seq #"\}" stripped))
               peak-depth (+ depth opens)
               new-depth (max 0 (+ depth opens (- closes)))]
           (recur (inc idx) new-depth
                  (if (> peak-depth threshold)
                    (conj results {:file file :line (inc idx) :smell "deep-nesting"
                                   :severity "medium"
                                   :message (format "Nesting depth %d exceeds threshold %d"
                                                    peak-depth threshold)})
                    results))))))))

;; --- Detector: God Class (>300 lines) ---

(defn detect-god-class
  ([lines _lang file] (detect-god-class lines _lang file {}))
  ([lines _lang file {:keys [threshold] :or {threshold 300}}]
   (when (> (count lines) threshold)
     [{:file file :line 1 :smell "god-class" :severity "high"
       :message (format "File has %d lines (threshold: %d)" (count lines) threshold)}])))

;; --- Detector: Long Parameter List (>5 params) ---

(def ^:private param-patterns
  {"python"     #"def\s+(\w+)\s*\(([^)]+)\)"
   "javascript" #"function\s+(\w+)\s*\(([^)]+)\)"
   "java"       #"(?:public|private|protected|static|\s)+\w+\s+(\w+)\s*\(([^)]+)\)"
   "go"         #"func\s+(?:\([^)]+\)\s+)?(\w+)\s*\(([^)]+)\)"
   "ruby"       #"def\s+(\w+)\s*\(([^)]+)\)"
   "rust"       #"fn\s+(\w+)\s*\(([^)]+)\)"
   "c"          #"(\w+)\s*\(([^)]{10,})\)\s*\{"
   "cpp"        #"(\w+)\s*\(([^)]{10,})\)\s*\{"})

(defn detect-long-parameter-list
  ([lines lang file] (detect-long-parameter-list lines lang file {}))
  ([lines lang file {:keys [threshold] :or {threshold 5}}]
   (when-let [pattern (get param-patterns lang)]
     (->> lines
          (map-indexed vector)
          (keep (fn [[idx line]]
                  (when-let [m (re-find pattern line)]
                    (let [name (nth m 1)
                          params-str (nth m 2)
                          param-count (count (str/split params-str #","))]
                      (when (> param-count threshold)
                        {:file file :line (inc idx) :smell "long-parameter-list"
                         :severity "medium"
                         :message (format "Function '%s' has %d parameters (threshold: %d)"
                                          name param-count threshold)})))))))))

;; --- Detector: Duplicate Code ---

(defn detect-duplicate-code
  ([lines _lang file] (detect-duplicate-code lines _lang file {}))
  ([lines _lang file {:keys [block-size] :or {block-size 3}}]
   (let [indexed (->> lines
                      (map-indexed vector)
                      (remove (fn [[_ l]] (str/blank? l)))
                      (mapv (fn [[idx l]] [idx (str/trim l)])))
         n (count indexed)]
     (when (>= n block-size)
       (let [seen (atom {})
             results (atom [])]
         (doseq [i (range (- n (dec block-size)))]
           (let [block (subvec indexed i (+ i block-size))
                 content (mapv second block)
                 start-line (ffirst block)]
             (if-let [first-line (get @seen content)]
               (swap! results conj
                      {:file file :line (inc start-line) :smell "duplicate-code"
                       :severity "medium"
                       :message (format "Duplicate block (first seen at line %d)"
                                        (inc first-line))})
               (swap! seen assoc content start-line))))
         @results)))))

;; --- Detector: Magic Numbers ---

(defn detect-magic-numbers
  ([lines lang file] (detect-magic-numbers lines lang file {}))
  ([lines lang file _opts]
   (let [allowed #{0 1 -1 2 100}]
     (->> lines
          (map-indexed vector)
          (keep (fn [[idx line]]
                  (let [stripped (strip-comments line lang)
                        stripped (-> stripped
                                     (str/replace #"\"[^\"]*\"" "\"\"")
                                     (str/replace #"'[^']*'" "''"))
                        skip? (or (re-find #"(?i)^\s*(?:const|final|#define|[A-Z_]{2,}\s*=)" stripped)
                                  (re-find #"(?i)^\s*(?:import|from|require|include|package)" stripped)
                                  (re-find #"(?i)^\s*(?:version|port)" stripped)
                                  (re-find #"range\(" stripped))
                        nums (when-not skip?
                               (->> (re-seq #"(?<!\w)(\d+\.?\d*)(?!\w)" stripped)
                                    (keep (fn [[_ n]]
                                            (try
                                              (let [v (Double/parseDouble n)]
                                                (when-not (contains? allowed (long v))
                                                  n))
                                              (catch Exception _ nil))))))]
                    (when (seq nums)
                      {:file file :line (inc idx) :smell "magic-number" :severity "low"
                       :message (format "Magic number(s): %s" (str/join ", " nums))}))))))))

;; --- Detector: Dead Code (unreachable after return) ---

(def ^:private return-patterns
  {"python"     #"^\s*(return|raise|sys\.exit|exit)\b"
   "javascript" #"^\s*(return|throw|process\.exit)\b"
   "java"       #"^\s*(return|throw)\b"
   "go"         #"^\s*(return|panic)\b"
   "ruby"       #"^\s*(return|raise|exit)\b"
   "rust"       #"^\s*(return|panic!)\b"
   "c"          #"^\s*(return|exit)\b"
   "cpp"        #"^\s*(return|throw|exit)\b"})

(defn detect-dead-code
  ([lines lang file] (detect-dead-code lines lang file {}))
  ([lines lang file _opts]
   (when-let [pattern (get return-patterns lang)]
     (->> (range (dec (count lines)))
          (keep (fn [idx]
                  (let [line (nth lines idx)
                        next-line (nth lines (inc idx))
                        cur-indent (indentation-level line)
                        next-indent (indentation-level next-line)]
                    (when (and (re-find pattern line)
                               next-indent cur-indent
                               (>= next-indent cur-indent)
                               (not (re-find #"^\s*[}\])]" next-line))
                               (not (re-find #"^\s*(else|elif|except|catch|finally|rescue|end|case|default|when)\b"
                                             next-line)))
                      {:file file :line (+ idx 2) :smell "dead-code" :severity "high"
                       :message (format "Unreachable code after '%s'" (str/trim line))}))))))))

;; --- Scanning ---

(defn scan-file [path]
  (let [ext (file-extension path)
        lang (get ext->lang ext)]
    (when lang
      (try
        (let [content (slurp (str path))
              lines (str/split-lines content)
              file (str path)]
          (concat
           (detect-long-methods lines lang file)
           (detect-deep-nesting lines lang file)
           (detect-god-class lines lang file)
           (detect-long-parameter-list lines lang file)
           (detect-duplicate-code lines lang file)
           (detect-magic-numbers lines lang file)
           (detect-dead-code lines lang file)))
        (catch Exception e
          [{:file (str path) :line 0 :smell "error" :severity "low"
            :message (str "Could not scan: " (.getMessage e))}])))))

(defn scan-directory [dir _opts]
  (let [extensions (set (keys ext->lang))
        files (->> (fs/glob dir "**")
                   (filter fs/regular-file?)
                   (filter #(contains? extensions (file-extension %)))
                   (remove #(str/includes? (str %) "node_modules"))
                   (remove #(str/includes? (str %) ".git/"))
                   (remove #(str/includes? (str %) "venv"))
                   (remove #(str/includes? (str %) "__pycache__"))
                   (remove #(str/includes? (str %) "target/")))]
    (->> files
         (mapcat scan-file)
         (sort-by (juxt :file :line)))))

;; --- Formatting ---

(def severity-rank {"high" 3 "medium" 2 "low" 1})

(defn format-text [findings]
  (if (empty? findings)
    "No code smells found."
    (str/join "\n"
      (concat
        [(format "Found %d code smell(s):\n" (count findings))]
        (map (fn [{:keys [file line severity smell message]}]
               (format "  %s:%d [%s] (%s) %s"
                       file line (str/upper-case severity) smell message))
             findings)
        [""
         (format "Summary: %d high, %d medium, %d low"
                 (count (filter #(= (:severity %) "high") findings))
                 (count (filter #(= (:severity %) "medium") findings))
                 (count (filter #(= (:severity %) "low") findings)))]))))

(defn format-json [findings]
  (json/generate-string
    {:total (count findings)
     :by-severity {:high (count (filter #(= (:severity %) "high") findings))
                   :medium (count (filter #(= (:severity %) "medium") findings))
                   :low (count (filter #(= (:severity %) "low") findings))}
     :by-smell (frequencies (map :smell findings))
     :findings findings}
    {:pretty true}))

;; --- CLI ---

(def cli-spec
  {:dir {:desc "Directory to scan"
         :default "."
         :alias :d}
   :format {:desc "Output format: text, json, edn"
            :default "text"
            :alias :f}
   :severity {:desc "Minimum severity: low, medium, high"
              :default "low"
              :alias :s}
   :help {:desc "Show help"
          :alias :h
          :coerce :boolean}})

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})
        _ (when (:help opts)
            (println "code-smell-detector — Detect code smells and suggest refactoring")
            (println)
            (println (cli/format-opts {:spec cli-spec}))
            (System/exit 0))
        min-sev (get severity-rank (:severity opts) 1)
        findings (->> (scan-directory (:dir opts) opts)
                      (filter #(>= (get severity-rank (:severity %) 0) min-sev)))]
    (println
      (case (:format opts)
        "json" (format-json findings)
        "edn" (pr-str findings)
        (format-text findings)))
    (System/exit (if (seq findings) 1 0))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
