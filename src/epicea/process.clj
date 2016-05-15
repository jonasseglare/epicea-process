(ns epicea.process
  (:import [java.lang ProcessBuilder Process StringBuilder]
           [org.apache.commons.io IOUtils]
           [java.io BufferedReader InputStreamReader])
  (:require [clojure.core.async :as async :refer [<!! >!! <! >!]]
            [mummi2.result :as result]
            [mummi2.debug :as debug]))

;;http://stackoverflow.com/questions/16714127/how-to-redirect-process-builders-output-to-a-string
;; http://stackoverflow.com/questions/1264709/convert-inputstream-to-byte-array-in-java

(defn write-all-to-process [process source-channel result]
  (let [output (.getOutputStream process)]
    (async/go 
      (try
        (loop []
          (let [x (<! source-channel)]
            (if x
              (do (.write output x) (recur))
              (do (.close output)))))
        (catch RuntimeException e
          (println "Failed in write-all-to-process")
          (>! result (result/make-error e)))))))

(defn read-all-from-process [stream]
  (let [dst (StringBuilder.)]
    (async/go
      (try 
        (IOUtils/toByteArray stream)
        (catch RuntimeException e
          (println "Failed in read-all-from-process")
          (result/make-error e))))))

(defn call-process-with-channels [args input-channel output-channel]
  (assert (sequential? args))
  (assert (<= 1 (count args)))
  (async/go
    (try
      (let [process (.start (ProcessBuilder. args))]
        (when input-channel
          (write-all-to-process process input-channel output-channel))
        (let [stdout-c (read-all-from-process (.getInputStream process))
              stderr-c (read-all-from-process (.getErrorStream process))
              stdout (<! stdout-c)
              stderr (<! stderr-c)
              any-error (or (:error stdout) (:error stderr))
              result (if any-error (result/make-error any-error)
                         (result/make-result {:stdout stdout
                                              :stderr stderr}))]
          (>! output-channel result)))
      (catch RuntimeException e
        (println "Failed in call-process-with-channels")
        (>! output-channel (result/make-error e)))))
  output-channel)


(defn call-process 
  ([args input]
   (let [output (async/chan)]
     (call-process-with-channels args input output)
     output))
  ([args]
   (call-process args nil)))

(defn make-transformed-process-fun [args input-fun output-fun]
  (fn [input]
    (let [result-chan (async/chan 1 (map output-fun))]
      (call-process-with-channels 
       args 
       (async/go (input-fun input))
       result-chan))))

(defn bytes-to-string [x]
  (String. x))

(defn string-to-bytes [x]
  (.getBytes x))

(defn result-to-x-fun [fun]
  (fn [result]
    (if (result/failure? result)
      result
      (try
        (result/with-result
          result (fn [x] {:stdout (fun (:stdout x))
                          :stderr (fun (:stderr x))}))
        (catch RuntimeException e
          (println "Failed in result-to-x-fun")
          (println "stdout: " (:stdout (:result result)))
          (println "stderr: " (:stdout (:result result)))
          (result/make-error e))))))

(def result-to-string (result-to-x-fun bytes-to-string))
   
(defn make-string-process-fun [args]
  (make-transformed-process-fun 
   args
   string-to-bytes
   result-to-string))

(def edn-to-bytes  (comp string-to-bytes pr-str))
;(defn edn-to-bytes [x]
;  (string-to-bytes 
;   (pr-str x)))


;(def bytes-to-edn (comp read-string bytes-to-string))
(defn bytes-to-edn [x]
  (try
    (read-string (bytes-to-string x))
    (catch Throwable e
      nil)))

(defn make-edn-process-fun [args]
  (make-transformed-process-fun
   args
   edn-to-bytes
   (result-to-x-fun bytes-to-edn)))
   

(def call-sort (make-string-process-fun ["sort"]))

(defn launch-ls [] 
  (let [output (call-process ["ls"])]
    (async/go (println "OUTPUT: \n" (String. (<! output))))))

(defn sort-demo []
  (println 
   (str
    "OUTPUT:\n" 
    (:stdout (:result (<!! (call-sort "bajs\nrulle\nmjao\napa")))))))
