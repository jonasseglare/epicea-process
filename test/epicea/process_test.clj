(ns epicea.process-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<!!]]
            [epicea.process :refer :all]
            [epicea.result :as result]))

(deftest string-test
  (testing "FIXME, I fail."
    (is (= "kattskit" (bytes-to-string (string-to-bytes "kattskit"))))
    (is (= (:stdout (result/get-value (<!! (call-sort "kba\nabc"))))
           "abc\nkba\n"))))

(deftest edn-test
  (testing "edn"
    (is (= (bytes-to-edn (edn-to-bytes {:a 1}))
           {:a 1}))))

(deftest identity-edn
  (testing "EDN"
    (let [filename (.getFile (clojure.java.io/resource "testscript.sh"))
          cmd (str "sh " filename)

          ;; OBS: "sh" and filename are two different arguments!
          pfun (make-edn-process-fun ["sh" filename])]

      ;; OBS: The test script will corrupt line endings (\n), so dont try that
      (let [s "mjao"]
        (is (= (:stdout (result/get-value (<!! (pfun s)))) s)))

      (is (= (:stdout (result/get-value (<!! (pfun [:a 1]))))
             [:a 1])))))
