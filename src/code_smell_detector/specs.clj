(ns code-smell-detector.specs
  "Data specs for code-smell-detector (https://clojure.org/guides/spec).
  Function specs (s/fdef) live next to each defn in code-smell-detector.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))

;; --- Inputs ---

(s/def ::lang #{"python" "javascript" "java" "go" "ruby" "c" "cpp" "rust"})

(def ^:private code-fragments
  ["def f(a, b):" "    return x" "    raise ValueError()" "  if (x) {" "}" "x = 42"
   "function g(a, b, c, d, e, f) {" "# comment 7" "const K = 9;" "        y = y + 1"
   "fn h(a: i32) -> i32 {" "func (s *S) Run() {" "public static int m(int a) {" ""])

;; One line of source text, as produced by clojure.string/split-lines.
(s/def ::line-text
  (s/with-gen (s/and string? #(not (re-find #"[\r\n]" %)))
    #(gen/one-of [(gen/elements code-fragments)
                  (gen/fmap (fn [[n s]] (str (str/join (repeat n " ")) s))
                            (gen/tuple (gen/choose 0 24) (gen/string-alphanumeric)))])))

(s/def ::lines (s/coll-of ::line-text :kind vector? :gen-max 40))

;; A relative or absolute file path, as a string or java.nio.file.Path.
(defn- gen-path-string []
  (gen/fmap (fn [[dirs base ext]]
              (str/join "/" (conj dirs (cond-> base ext (str "." ext)))))
            (gen/tuple (gen/vector (gen/not-empty (gen/string-alphanumeric)) 0 3)
                       (gen/not-empty (gen/string-alphanumeric))
                       (gen/one-of [(gen/return nil)
                                    (gen/elements ["py" "js" "go" "rs" "rb" "java" "clj" "txt"])
                                    (gen/string-alphanumeric)]))))

(s/def ::path-string (s/with-gen (s/and string? seq) gen-path-string))
(s/def ::path-like
  (s/with-gen (s/or :string ::path-string
                    :path #(instance? java.nio.file.Path %))
    gen-path-string))

;; Detector options (each detector reads the key it cares about).
(s/def ::threshold (s/int-in 0 64))
(s/def ::block-size (s/int-in 1 8))
(s/def ::opts (s/keys :opt-un [::threshold ::block-size]))

;; --- Findings ---

(s/def ::file string?)
(s/def ::line nat-int?)
(s/def ::smell #{"long-method" "deep-nesting" "god-class" "long-parameter-list"
                 "duplicate-code" "magic-number" "dead-code" "error"})
(s/def ::severity #{"high" "medium" "low"})
(s/def ::message string?)
(s/def ::finding (s/keys :req-un [::file ::line ::smell ::severity ::message]))
(s/def ::findings (s/coll-of ::finding :kind sequential? :gen-max 10))

;; --- Shared fdef pieces for the detect-* fns ---

(s/def ::detector-args
  (s/cat :lines ::lines :lang ::lang :file ::file :opts (s/? ::opts)))

(defn findings-within-input?
  "s/fdef :fn for detectors: every finding points at the scanned file and at
  a 1-based line that exists in the input."
  [{{:keys [lines file]} :args ret :ret}]
  (every? #(and (= file (:file %)) (<= 1 (:line %) (count lines))) ret))

;; --- CLI ---

(s/def ::format #{"text" "json" "edn"})
(s/def ::min-severity ::severity)
