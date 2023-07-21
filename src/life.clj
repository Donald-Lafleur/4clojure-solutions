(ns life
  (:require [clojure.string]))

; Extension of a solution to problem 94.

; To run from a repl started in the repo:
; (load "life")
; (life/gol life/glider-gun)

(defn make-neighbors
  "Returns a function which given an index in a grid of dimensions height
  by width returns a list of the indices of all the neighbors of that cell,
  where 0 is the top left cell of the grid."
  [height width]
  (fn [idx]
    (let [row (quot idx width)
          col (mod idx width)]
      (for [r [(dec row) row (inc row)]
            c [(dec col) col (inc col)]
            :when (not= [r c] [row col])
            :when (>= r 0)
            :when (>= c 0)
            :when (< r height)
            :when (< c width)]
        (+ c (* r width))))))

(defn gol
  "Takes a starting board comprised of a vector of equal length strings
  representing the start state and prints iters proceeding states, once
  every refresh milliseconds. Spaces represent dead cells and # represents
  a living cell."
  ([board] (gol board 100))
  ([board iters] (gol board iters 100))
  ([board iters refresh]
   (let [disp [\space \#]
         height (count board)
         width (count (first board))
         neighbors (make-neighbors height width)
         nv (into [] (map #(neighbors %1) (range (* height width))))
         transition [[0 0 0 1 0 0 0 0 0] [0 0 1 1 0 0 0 0 0]]
         next-state (fn [bv idx] ((transition (bv idx)) (reduce + (map #(bv %) (nv idx)))))
         next-bv (fn [bv] (into [] (map #(next-state bv %) (range (* height width)))))
         print-board (fn [bv] (print (clojure.string/join "\n"
                                                          (map #(apply str %)
                                                               (partition width (map #(disp %) bv)))))
                       (newline)
                       (flush))]
     (loop [bv (into [] (map #({\space 0 \# 1} %) (mapcat vec board)))
            n iters]
       (print-board bv)
       (if (= n 0) nil
           (do (Thread/sleep refresh)
               (recur (next-bv bv) (dec n))))))))

(def glider-gun
  ["                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "                                                            "
   "            ##                                              "
   "           #   #                                            "
   "          #     #       #                                   "
   "##        #   # ##    # #                                   "
   "##        #     #   ##                                      "
   "           #   #    ##            ##                        "
   "            ##      ##            ##                        "
   "                      # #                                   "
   "                        #                                   "])