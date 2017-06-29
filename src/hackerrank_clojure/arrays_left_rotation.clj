(ns hackerrank-clojure.arrays-left-rotation
  (:require [clojure.string :as str]
            [clojure.test :as tst]
            [clojure.java.io :as io])
  (:use [clojure.test]))

(defn main [reader]
  (let [lines (-> reader java.io.BufferedReader. line-seq)
        n_temp (nth lines 0) 
        n_t (str/split n_temp #"\s+") 
        array-len (Integer/parseInt (n_t 0)) 
        num-left-rotations (as-> (n_t 1) nlr-intermed
                             (Integer/parseInt nlr-intermed)
                             ;; mod (modulus) below - for the case that the num-rotations are greater
                             ;; than the length of the array then doing the rotations beyond the
                             ;; length of the arrays are duplicate and wasted
                             (mod nlr-intermed array-len))]
    (let [a_temp (nth lines 1) 
          string-coll (str/split a_temp #"\s+") 
          array (as-> string-coll result-coll
                  (map #(Integer/parseInt %) result-coll)
                  (int-array array-len result-coll))]
      (let [temp-array (int-array num-left-rotations)
            num-shifts (- array-len num-left-rotations)
            result (if (= 0 num-left-rotations)
                     array
                     (loop [i 0]
                       ;; (println (with-out-str (clojure.pprint/pprint {:i i
                       ;;                                                :num-left-rotations num-left-rotations})))
                       (if (>= i array-len)
                         array
                         (do
                           (when (< i num-left-rotations)
                             (aset temp-array i (nth array i)))
                           (if (< i num-shifts)
                             (aset array i (nth array (+ i num-left-rotations)))
                             (aset array i (nth temp-array (- i num-shifts))))
                           (recur (inc i))))))]
        (println (str/join " " result))
        result))))



(def input-08 (clojure.java.io/reader (clojure.java.io/resource "input08.txt")))


(defn to-inputstream-reader [str-in]
  (-> str-in
      .getBytes
      io/input-stream
      java.io.InputStreamReader.))

(defn- main-as-vec [reader]
  (-> (main reader) aclone vec))

(deftest simple-test
  (is (= [3 4 5 1 2] (main-as-vec (to-inputstream-reader (str/join "\n" ["5 2"
                                                               "1 2 3 4 5"]))))))

(deftest simple-test-2
  (is (= [3 4 5 0 1 2] (main-as-vec (to-inputstream-reader (str/join "\n" ["6 3"
                                                                 "0 1 2 3 4 5"]))))))

(deftest simple-test-3
  (is (= [3 4 0 1 2] (main-as-vec (to-inputstream-reader (str/join "\n" ["5 3"
                                                               "0 1 2 3 4"]))))))

