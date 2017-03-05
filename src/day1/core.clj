(ns day1.core
    (:require [clojure.string :refer [split trim-newline]]
              [ysera.collections :refer [index-of]]
              [ysera.error :refer [error]]
              [ysera.test :refer [is=]]))

(def directions [:north :east :south :west])

(defn
  ^{:test (fn []
            (is= (circle-nth [1 2 3] 4)
                 2))}
  circle-nth [seq n]
  (nth seq (mod n (count seq))))

(defn
  ^{:test (fn []
            (is= (rotate :north "R")
                 :east)
            (is= (rotate :east "R")
                 :south)
            (is= (rotate :south "R")
                 :west)
            (is= (rotate :west "R")
                 :north)
            (is= (rotate :west "L")
                 :south))}
  rotate [start-direction rotation]
  (let [index (index-of directions start-direction)]
    (condp = rotation
      "R"   (circle-nth directions (inc index))
      "L"   (circle-nth directions (dec index))
      :else (error "Invalid rotation"))))

(defn
  ^{:test (fn []
            (is= (move {:direction   :north
                        :coordinates [0 0]}
                       "R9")
                 {:direction   :east
                  :coordinates [9 0]})
            (is= (move {:direction   :east
                        :coordinates [0 0]}
                       "R124")
                 {:direction   :south
                  :coordinates [0 -124]})
            (is= (move {:direction   :south
                        :coordinates [12 -10]}
                       "R2")
                 {:direction   :west
                  :coordinates [10 -10]}))}
  move [{start-direction :direction start-coordinates :coordinates} move]
  (let [rotation (str (first move))
        steps (read-string (subs move 1))
        new-direction (rotate start-direction rotation)]
    {:direction new-direction
     :coordinates (->> (condp = new-direction
                         :north [0 steps]
                         :east  [steps 0]
                         :south [0 (- steps)]
                         :west  [(- steps) 0])
                       (map + start-coordinates))}))

(defn
  ^{:test (fn []
            (is= (find-end-coordinates [0 0] "R2, L3")
                 [2 3])
            (is= (find-end-coordinates [0 0] "R2, R2, R2")
                 [0 -2])
            (is= (find-end-coordinates [1 -1] "R5, L5, R5, R3")
                 [11 1]))}
  find-end-coordinates [start-coordinates moves]
  (:coordinates (reduce move
                        {:direction :north
                         :coordinates start-coordinates}
                        (split moves #", "))))

(defn abs [x]
    (if (> x 0) x (* -1 x)))

(defn
  ^{:test (fn []
            (is= (get-taxi-distance [-2 -2] [2 2])
                 8))}
  get-taxi-distance [from-coordinates to-coordinates]
  (reduce (fn [distance dimension-index]
            (+ distance (abs (- (nth from-coordinates dimension-index)
                                (nth to-coordinates dimension-index)))))
          0
          (range (count from-coordinates))))

(defn
  solve! []
  (->> (slurp "resources/input.txt")
       (trim-newline)
       (find-end-coordinates [0 0])
       (get-taxi-distance [0 0])))

(solve!)
