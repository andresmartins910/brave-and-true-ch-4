(ns fwpd.core
  (:require [clojure.string]
            [clojure.java.io]
            [clojure.data.csv :as csv]))

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int [str]
  (Integer. str))

(def conversions {:name identity :glitter-index str->int})

(defn convert [vamp-key value]
  ((get conversions vamp-key) value))

(convert :glitter-index "10")

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",") (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(def suspects (mapify (parse (slurp filename))))

(defn map->csv-str [coll]
  (->> coll
       (map vals)
       (map (partial clojure.string/join ","))
       (clojure.string/join "\n")))

(map->csv-str suspects)

(defn glitter-filter
  [minimum-glitter records]
  (into '() (map :name (filter #(>= (:glitter-index %) minimum-glitter) records))))

(glitter-filter 3 (mapify (parse (slurp filename))))

(defn int->str [int]
  (str int))

(defn validate [{:keys [name glitter-index]}]
  (when (and name glitter-index)
    [name (int->str glitter-index)]))

(def not-empty? (complement empty?))

(defn append-to-csv [record]
  (let [new-row (validate record)]
    (when (not-empty? new-row)
      (with-open [file-path (clojure.java.io/writer "suspects.csv" :append true)]
        (csv/write-csv file-path [new-row])))))


