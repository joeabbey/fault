(ns sample.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import (java.util Date)))

(def version "1.0.0")

(defn greet
  "Greets a user."
  [name]
  (str "Hello, " name))

(defn- internal-helper
  [x]
  (* x 2))

(defn process-items
  [items]
  (map str/upper-case items))
