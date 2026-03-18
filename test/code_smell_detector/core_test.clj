(ns code-smell-detector.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [code-smell-detector.core :as sut]))

;; --- Test Fixtures ---

(def python-long-method
  (str "def very_long_function():\n"
       (str/join "\n" (repeat 35 "    x = x + 1"))
       "\n    return x"))

(def python-short-method
  "def short_function():\n    x = 1\n    return x")

(def python-deep-nesting
  (str "def nested():\n"
       "    if True:\n"
       "        if True:\n"
       "            if True:\n"
       "                if True:\n"
       "                    if True:\n"
       "                        x = 1"))

(def js-deep-nesting
  (str "function foo() {\n"
       "  if (true) {\n"
       "    if (true) {\n"
       "      if (true) {\n"
       "        if (true) {\n"
       "          if (true) {\n"
       "            x = 1;\n"
       "          }\n"
       "        }\n"
       "      }\n"
       "    }\n"
       "  }\n"
       "}"))

(def python-many-params
  "def many_params(a, b, c, d, e, f, g):\n    pass")

(def python-few-params
  "def few_params(a, b):\n    pass")

(def python-dead-code
  "def foo():\n    return 1\n    x = 2")

(def python-no-dead-code
  "def foo():\n    x = 1\n    return x")

(def python-magic-numbers
  "def calc():\n    x = 42\n    y = x * 3.14")

(def python-no-magic
  "def calc():\n    x = 0\n    y = x + 1")

(def python-duplicate
  (str "x = 1\n"
       "y = 2\n"
       "z = 3\n"
       "a = 4\n"
       "x = 1\n"
       "y = 2\n"
       "z = 3"))

(def python-no-duplicate
  "x = 1\ny = 2\nz = 3\na = 4\nb = 5")

(def python-god-class
  (str/join "\n" (repeat 310 "x = 1")))

(def js-long-function
  (str "function longFunction() {\n"
       (str/join "\n" (repeat 35 "  var x = 1;"))
       "\n}"))

;; --- Tests ---

(deftest test-file-extension
  (testing "extracts file extensions"
    (is (= "py" (sut/file-extension "foo.py")))
    (is (= "js" (sut/file-extension "bar.js")))
    (is (= "java" (sut/file-extension "Baz.java")))
    (is (nil? (sut/file-extension "noext")))))

(deftest test-indentation-level
  (testing "measures indentation"
    (is (= 0 (sut/indentation-level "hello")))
    (is (= 4 (sut/indentation-level "    hello")))
    (is (= 8 (sut/indentation-level "        hello")))
    (is (nil? (sut/indentation-level "")))))

(deftest test-detect-long-methods-python
  (testing "detects long Python methods"
    (let [lines (str/split-lines python-long-method)
          results (sut/detect-long-methods lines "python" "test.py")]
      (is (= 1 (count results)))
      (is (= "long-method" (:smell (first results))))
      (is (= "high" (:severity (first results))))
      (is (= 1 (:line (first results))))))
  (testing "ignores short methods"
    (let [lines (str/split-lines python-short-method)
          results (sut/detect-long-methods lines "python" "test.py")]
      (is (empty? results)))))

(deftest test-detect-long-methods-js
  (testing "detects long JavaScript functions"
    (let [lines (str/split-lines js-long-function)
          results (sut/detect-long-methods lines "javascript" "test.js")]
      (is (= 1 (count results)))
      (is (= "long-method" (:smell (first results)))))))

(deftest test-detect-deep-nesting-python
  (testing "detects deep nesting in Python"
    (let [lines (str/split-lines python-deep-nesting)
          results (sut/detect-deep-nesting lines "python" "test.py")]
      (is (pos? (count results)))
      (is (every? #(= "deep-nesting" (:smell %)) results))
      (is (every? #(= "medium" (:severity %)) results)))))

(deftest test-detect-deep-nesting-js
  (testing "detects deep nesting in JavaScript"
    (let [lines (str/split-lines js-deep-nesting)
          results (sut/detect-deep-nesting lines "javascript" "test.js")]
      (is (pos? (count results)))
      (is (every? #(= "deep-nesting" (:smell %)) results)))))

(deftest test-detect-god-class
  (testing "detects files over 300 lines"
    (let [lines (str/split-lines python-god-class)
          results (sut/detect-god-class lines "python" "test.py")]
      (is (= 1 (count results)))
      (is (= "god-class" (:smell (first results))))
      (is (= "high" (:severity (first results))))))
  (testing "ignores small files"
    (let [lines (str/split-lines python-short-method)
          results (sut/detect-god-class lines "python" "test.py")]
      (is (nil? results)))))

(deftest test-detect-long-parameter-list
  (testing "detects functions with many parameters"
    (let [lines (str/split-lines python-many-params)
          results (sut/detect-long-parameter-list lines "python" "test.py")]
      (is (= 1 (count results)))
      (is (= "long-parameter-list" (:smell (first results))))
      (is (= "medium" (:severity (first results))))))
  (testing "ignores functions with few parameters"
    (let [lines (str/split-lines python-few-params)
          results (sut/detect-long-parameter-list lines "python" "test.py")]
      (is (empty? results)))))

(deftest test-detect-duplicate-code
  (testing "detects duplicate code blocks"
    (let [lines (str/split-lines python-duplicate)
          results (sut/detect-duplicate-code lines "python" "test.py")]
      (is (pos? (count results)))
      (is (every? #(= "duplicate-code" (:smell %)) results))))
  (testing "no duplicates in unique code"
    (let [lines (str/split-lines python-no-duplicate)
          results (sut/detect-duplicate-code lines "python" "test.py")]
      (is (empty? results)))))

(deftest test-detect-magic-numbers
  (testing "detects magic numbers"
    (let [lines (str/split-lines python-magic-numbers)
          results (sut/detect-magic-numbers lines "python" "test.py")]
      (is (pos? (count results)))
      (is (every? #(= "magic-number" (:smell %)) results))
      (is (every? #(= "low" (:severity %)) results))))
  (testing "allows 0 and 1"
    (let [lines (str/split-lines python-no-magic)
          results (sut/detect-magic-numbers lines "python" "test.py")]
      (is (empty? results)))))

(deftest test-detect-dead-code
  (testing "detects unreachable code after return"
    (let [lines (str/split-lines python-dead-code)
          results (sut/detect-dead-code lines "python" "test.py")]
      (is (= 1 (count results)))
      (is (= "dead-code" (:smell (first results))))
      (is (= "high" (:severity (first results))))
      (is (= 3 (:line (first results))))))
  (testing "no false positive for normal code"
    (let [lines (str/split-lines python-no-dead-code)
          results (sut/detect-dead-code lines "python" "test.py")]
      (is (empty? results)))))

(deftest test-format-text
  (testing "formats empty findings"
    (is (= "No code smells found." (sut/format-text []))))
  (testing "formats findings as text"
    (let [findings [{:file "test.py" :line 1 :severity "high"
                     :smell "long-method"
                     :message "Method 'foo' is 50 lines (threshold: 30)"}]
          output (sut/format-text findings)]
      (is (str/includes? output "Found 1 code smell(s)"))
      (is (str/includes? output "test.py:1"))
      (is (str/includes? output "HIGH"))
      (is (str/includes? output "long-method")))))

(deftest test-format-json
  (testing "formats findings as JSON"
    (let [findings [{:file "test.py" :line 1 :severity "high"
                     :smell "long-method"
                     :message "Method 'foo' is 50 lines"}]
          output (sut/format-json findings)]
      (is (str/includes? output "\"total\" : 1"))
      (is (str/includes? output "long-method")))))

(deftest test-severity-filtering
  (testing "severity-rank maps correctly"
    (is (= 3 (get sut/severity-rank "high")))
    (is (= 2 (get sut/severity-rank "medium")))
    (is (= 1 (get sut/severity-rank "low")))))
