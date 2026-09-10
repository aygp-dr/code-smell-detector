(ns code-smell-detector.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [code-smell-detector.core :as sut]
            [code-smell-detector.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
(def ^:private side-effecting
  #{`sut/scan-file `sut/scan-directory `sut/-main})

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'code-smell-detector.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/lines ::specs/path-like ::specs/opts ::specs/finding ::specs/findings]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(deftest real-values-conform
  (testing "lookup tables"
    (is (s/valid? (s/map-of string? ::specs/lang) sut/ext->lang))
    (is (s/valid? (s/map-of ::specs/severity pos-int?) sut/severity-rank)))
  (testing "a real detector result"
    (let [lines (vec (cons "def f():" (repeat 40 "    x = 1")))]
      (is (s/valid? ::specs/findings (sut/detect-long-methods lines "python" "t.py"))))))
