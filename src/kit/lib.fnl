;; Utility functions to make fennel more like CLJ

(fn first [coll] (?. coll 1))
(fn last [coll] (?. coll (length coll)))
(fn nil? [x] (= x nil))

(fn ^in [x ...] (doto x (table.insert ...)))
(fn table? [x] (= (type x) :table))
(fn arr? [x] (and (table? x) (?. x 1)))

(fn cons [head rest]
  "Inserts head into rest, if array. Turns into array if not."
  (let [t? (table? rest)]
    (if
     (and (nil? head) t?)    rest
     (and (nil? rest) head)  [head]
     t?                      (^in rest 1 head)
     :else                   [head rest])))

(macro *args [val]
  `(if (arr? ,val) ,val (cons ,val [...])))

(macro hargs [val]
  `(if (and (not (arr? ,val)) (table? ,val))
       ,val
       (let [list# (cons ,val [...])
             acc# {}]
         (for [i# 1 (count list#) 2]
           (tset acc# (?. list# i#) (?. list# (+ i# 1))))
         acc#)))

(fn count [coll]
  (if (arr? coll)
      (length coll)
      (table? coll)
      (accumulate [count 0 i v (pairs coll)] (+ count 1))
      coll (length coll)
      nil))
(fn empty? [coll]
  (if (nil? coll) true (= 0 (count coll))))

(fn into [arr val ...]
  "Insert a list of values at the end of an existing list. "
  (assert (table? arr) "first arg must be collection")
  (let [coll (*args val)]
    (each [_ v (ipairs coll)]
      (^in arr v))
    arr))

(fn merge [h1 h2 ...]
  (let [new-hash
        (accumulate [new {}
                     k v (pairs h1)]
          (doto new (tset k v)))]
    (accumulate [new new-hash k v (pairs (hargs h2))] (doto new (tset k v)))))

(fn update [hash k func]
  (doto hash (tset k (func (?. hash k)))))

(fn min [a ...]
  "Find min of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [min (first coll) _ v (ipairs coll)]
      (if (< v min) v min))))

(fn max [a ...]
  "Find max of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [max (first coll) _ v (ipairs coll)]
      (if (< v max) max v))))

(fn sum [a ...]
  "Find sum of a collection (or list of arguments)"
  (let [coll (*args a)]
    (accumulate [acc 0 _ v (ipairs coll)]
      (if v (+ v acc) acc))))

(fn clamp [val min max]
  (if (> val max) max
      (< val min) min
      val))

(fn between? [val min max]
  (if (> val max) false
      (< val min) false
      true))

(fn mapv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [_ v (ipairs coll)] (func v)))

(fn filterv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [_ v (ipairs coll)]
    (if (func v) v)))

(fn mapiv [func coll]
  (assert (table? coll) "last arg must be collection")
  (icollect [i v (ipairs coll)] (func i v)))

(fn take [n coll]
  (assert (table? coll) "last arg must be collection")
  (accumulate [acc []
               i v (ipairs coll) :until (<= n (length acc))]
    (into acc v)))

(fn partition [arr max-cnt]
  "Takes an single dimension table and splits into pages of size count"
  (assert (table? arr) "first arg must be collection")
  (accumulate [acc [[]]
               index val (ipairs arr)]
    (if
     ;; If max <= current last size, new last
     (<= max-cnt (count (last acc)))
     (do (into acc [[val]]) acc)
     ;; Else, append to last
     (do (into (last acc) val) acc))))

(fn chars [str]
  (local acc [])
  (for [i 1 (count str)]
    (^in acc (string.sub str i i)))
  acc)

(fn words [str]
  (local acc [])
  (each [v (string.gmatch (string.gsub str "\n" " _NEWLINE_ ") "[^ \t]+")]
    (^in acc v))
  acc)

(fn inspect-serialize [val name skipnewlines depth]
  (let [skipnewlines (or skipnewlines false)
        depth (or depth 0)]
    (var tmp (string.rep " " depth))
    (when name (set tmp (.. tmp name " = ")))
    (if (= (type val) :table)
        (do
          (set tmp
               (.. tmp "{"
                   (or (and (not skipnewlines)
                            "\n")
                       "")))
          (each [k v (pairs val)]
            (set tmp
                 (.. tmp
                     (inspect-serialize v k
                                      skipnewlines
                                      (+ depth 1))
                     ","
                     (or (and (not skipnewlines)
                              "\n")
                         ""))))
          (set tmp
               (.. tmp (string.rep " " depth) "}")))
        (= (type val) :number)
        (set tmp (.. tmp (tostring val)))
        (= (type val) :string)
        (set tmp (.. tmp (string.format "%q" val)))
        (= (type val) :boolean)
        (set tmp
             (.. tmp (or (and val :true) :false)))
        (set tmp
             (.. tmp
                 "\"[datatype:" (type val) "]\"")))
    tmp))

(macro inspect [val name]
  (let [inspected (or name (tostring val))]
    `(let [result# ,val]
       (trace (inspect-serialize result# ,inspected))
       result#)))
