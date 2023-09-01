###
### data.janet 
###
### Compare data structures using `diff`.
###

(comment (use judge))

(varfn diff [])

(defn- atom-diff
  [a b]
  (if (= a b) @[nil nil a] @[a b nil]))

(comment
  (deftest "atom-diff: test integers - Same" (test (atom-diff 24 24) @[nil nil 24]))
  (deftest "atom-diff: test integers - Different" (test (atom-diff 14 95) @[14 95 nil]))
  (deftest "atom-diff: test strings - Same" (test (atom-diff "Hello" "Hello") @[nil nil "Hello"]))
  (deftest "atom-diff: test strings - Different" (test (atom-diff "Hello" "World") @["Hello" "World" nil]))
  (deftest "atom-diff: test string and tuple" (test (atom-diff "Hello" [1 2 3 4]) @["Hello" [1 2 3 4] nil]))
  (deftest "atom-diff: test keyword and struct - Same" (test (atom-diff :ahoy {:ahoy "there"}) @[:ahoy {:ahoy "there"} nil])) 
  )

(defn- in? [x ds]
  (if (index-of x ds)
    true
    false))

(defn- safe-in [ds n]
  (if (in? (type ds) [:array :tuple])
    (if (<= n (dec (length ds)))
      (in ds n)
      nil)
    (in ds n)))

(defn- vectorize [m]
  (unless (or (nil? m) (empty? m))
    (when (in? (type m) [:array :tuple :table :struct])
      (reduce
        (fn [result [k v]] (put result k v))
        (array/new-filled (max ;(keys m)))
        (pairs m)))))

(defn- diff-associative-key [a b k]
  (let [va (safe-in a k)
        vb (safe-in b k)
        [a* b* ab] (diff va vb)
        same (and (in? k (keys a)) (in? k (keys b))
                  (or (not (nil? ab))
                      (and (nil? va) (nil? vb))))]
    [(when (and (in? k (keys a)) (or (not (nil? a*)) (not same))) {k a*})
     (when (and (in? k (keys b)) (or (not (nil? b*)) (not same))) {k b*})
     (when same {k ab})]))

(defn- diff-associative [a b &opt ks]
  (default ks (distinct (array/concat (keys a) (keys b))))
  (let [reduced (reduce
                 (fn [diff1 diff2]
                   (map |(if (empty? $) nil $)
                        (map |(merge (or $0 @{}) (or $1 @{})) diff1 diff2)))
                 [nil nil nil]
                 (map (partial diff-associative-key a b) ks))]
    reduced))

(defn- diff-sequential [a b]
  (map vectorize (diff-associative
                   (if (array? a) a (array ;a))
                   (if (array? b) b (array ;b))
                   (range (max (length a) (length b))))))

(varfn diff
  ``` 
  Compares a and b recursively. Returns an array of 
  @[things-only-in-a things-only-in-b things-in-both].   
  ```
  [a b]
  (if (deep= a b)
    @[nil nil (postwalk |(cond (tuple? $) (array ;$)
                           (struct? $) (struct/to-table $) $) a)]
    (do 
      (cond
      (all indexed? [a b]) (diff-sequential a b)
      (all dictionary? [a b]) (diff-associative a b)
      (atom-diff a b)))))

(comment
  (deftest "Should be: Integers, same" (test (diff 1 1) @[nil nil 1]))
  (deftest "Should be: Integers, different" (test (diff 1 2) @[1 2 nil]))
  (deftest "Should be: Strings, same" (test (diff "1" "1") @[nil nil "1"]))
  (deftest "Should be: Strings, different" (test (diff "1" "2") @["1" "2" nil]))
  (deftest "Should be: String and Integer, different" (test (diff "String" 1) @["String" 1 nil]))
  (deftest "Should be: Tuples, same" (test (diff [1 2 3] [1 2 3]) @[nil nil @[1 2 3]]))
  (deftest "Should be: Tuples, element added" (test (diff [1 2 3] [1 2 3 4]) @[nil @[nil nil nil 4] @[1 2 3]]))
  (deftest "Should be: Tuples, element removed" (test (diff [1 2 3] [1 2]) @[@[nil nil 3] nil @[1 2]]))
  (deftest "Should be: Tuples, element changed" (test (diff [1 2 3] [1 5 3]) @[@[nil 2] @[nil 5] @[1 nil 3]]))
  (deftest "Should be: Tuples, element changed and element removed" (test (diff [1 2 3] [1 5]) @[@[nil 2 3] @[nil 5] @[1]]))
  (deftest "Should be: Arrays, same" (test (diff @[1 2 3] @[1 2 3]) @[nil nil @[1 2 3]]))
  (deftest "Should be: Arrays, element added" (test (diff @[1 2 3] @[1 2 3 4]) @[nil @[nil nil nil 4] @[1 2 3]]))
  (deftest "Should be: Arrays, element removed" (test (diff @[1 2 3] @[1 2]) @[@[nil nil 3] nil @[1 2]]))
  (deftest "Should be: Arrays, element changed" (test (diff @[1 2 3] @[1 5 3]) @[@[nil 2] @[nil 5] @[1 nil 3]]))
  (deftest "Should be: Arrays, element changed and element removed" (test (diff @[1 2 3] @[1 5]) @[@[nil 2 3] @[nil 5] @[1]]))
  (deftest "Should be: Structs, same" (test (diff {:a 1 :b 2} {:a 1 :b 2}) @[nil nil @{:a 1 :b 2}]))
  (deftest "Should be: Structs, element added" (test (diff {:a 1 :b 2} {:a 1 :b 2 :c 3}) @[nil @{:c 3} @{:a 1 :b 2}]))
  (deftest "Should be: Structs, element removed" (test (diff {:a 1 :b 2} {:a 1}) @[@{:b 2} nil @{:a 1}]))
  (deftest "Should be: Structs, element changed" (test (diff {:a 1 :b 2} {:a 1 :b 5}) @[@{:b 2} @{:b 5} @{:a 1}]))
  (deftest "Should be: Structs, element changed and element removed" (test (diff {:a 1 :b 2} {:b 5}) @[@{:a 1 :b 2} @{:b 5} nil]))
  (deftest "Should be: Tables, same" (test (diff @{:a 1 :b 2} @{:a 1 :b 2}) @[nil nil @{:a 1 :b 2}]))
  (deftest "Should be: Tables, element added" (test (diff @{:a 1 :b 2} @{:a 1 :b 2 :c 3}) @[nil @{:c 3} @{:a 1 :b 2}]))
  (deftest "Should be: Tables, element removed" (test (diff @{:a 1 :b 2} @{:a 1}) @[@{:b 2} nil @{:a 1}]))
  (deftest "Should be: Tables, element changed" (test (diff @{:a 1 :b 2} @{:a 1 :b 5}) @[@{:b 2} @{:b 5} @{:a 1}]))
  (deftest "Should be: Tables, element changed and element removed" (test (diff @{:a 1 :b 2} @{:b 5}) @[@{:a 1 :b 2} @{:b 5} nil]))
  )

### Nested Tuples

(comment
  (deftest "Should be: Nested Tuples, same" (test (diff [1 [1 2]] [1 [1 2]]) @[nil nil @[1 @[1 2]]]))
  (deftest "Should be: Nested Tuples, element added, outer" (test (diff [1 [1 2]] [1 [1 2] 3]) @[nil @[nil nil 3] @[1 @[1 2]]]))
  (deftest "Should be: Nested Tuples, element added, inner" (test (diff [1 [1 2]] [1 [1 2 3]]) @[nil @[nil @[nil nil 3]] @[1 @[1 2]]]))
  (deftest "Should be: Nested Tuples, element removed, outer" (test (deep= (diff [1 [1 2]] [[1 2]]) @[@[1 @[1 2]] @[@[1 2]] nil]) false))
  (deftest "Should be: Nested Tuples, element removed, inner" (test (diff [1 [1 2]] [1 [1]]) @[@[nil @[nil 2]] nil @[1 @[1]]]))
  (deftest "Should be: Nested Tuples, element changed" (test (diff [1 [1 2]] [1 [1 5]]) @[@[nil @[nil 2]] @[nil @[nil 5]] @[1 @[1]]]))
  (deftest "Should be: Nested Tuples, element changed and element removed" (test (deep= (diff [1 [1 2]] [[1 5]]) @[@[1 @[1 2]] @[@[1 5]] nil]) false))
  )

### Nested Arrays

(comment
  (deftest "Should be: Nested Arrays, same" (test (diff @[1 @[1 2]] @[1 @[1 2]]) @[nil nil @[1 @[1 2]]]))
  (deftest "Should be: Nested Arrays, element added, outer" (test (diff @[1 @[1 2]] @[1 @[1 2] 3]) @[nil @[nil nil 3] @[1 @[1 2]]]))
  (deftest "Should be: Nested Arrays, element added, inner" (test (diff @[1 @[1 2]] @[1 @[1 2 3]]) @[nil @[nil @[nil nil 3]] @[1 @[1 2]]]))
  (deftest "Should be: Nested Arrays, element removed, outer" (test (deep= (diff @[1 @[1 2]] @[@[1 2]]) @[@[1 @[1 2]] @[@[1 2]] nil]) true))
  (deftest "Should be: Nested Arrays, element removed, inner" (test (diff @[1 @[1 2]] @[1 @[1]]) @[@[nil @[nil 2]] nil @[1 @[1]]]))
  (deftest "Should be: Nested Arrays, element changed" (test (diff @[1 @[1 2]] @[1 @[1 5]]) @[@[nil @[nil 2]] @[nil @[nil 5]] @[1 @[1]]]))
  (deftest "Should be: Nested Arrays, element changed and element removed" (test (deep= (diff @[1 @[1 2]] @[@[1 5]]) @[@[1 @[1 2]] @[@[1 5]] nil]) true))
  )

### Nested Tables

(comment
  (deftest "Should be: Nested Tables, same" (test (diff @{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1 :d 2}}) @[nil nil @{:a 1 :b @{:c 1 :d 2}}]))
  (deftest "Should be: Nested Tables, element added - inner" (test (diff @{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1 :d 2 :e {:f 1 :g 2 :h 3}}}) @[nil @{:b @{:e {:f 1 :g 2 :h 3}}} @{:a 1 :b @{:c 1 :d 2}}]))
  (deftest "Should be: Nested Tables, element added - outer" (test (diff @{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1 :d 2 :e {:f 1 :g 2}} :h 3}) @[nil @{:b @{:e {:f 1 :g 2}} :h 3} @{:a 1 :b @{:c 1 :d 2}}]))
  (deftest "Should be: Nested Tables, element removed - inner" (test (diff @{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1}}) @[@{:b @{:d 2}} nil @{:a 1 :b @{:c 1}}]))
  (deftest "Should be: Nested Tables, element removed - outer" (test (diff @{:a 1 :b {:c 1 :d 2}} @{:b {:c 1 :d 2}}) @[@{:a 1} nil @{:b @{:c 1 :d 2}}]))
  (deftest "Should be: Nested Tables, element changed" (test (diff @{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1 :d 5}}) @[@{:b @{:d 2}} @{:b @{:d 5}} @{:a 1 :b @{:c 1}}]))
  (deftest "Should be: Nested Tables, element changed and element removed" (test (diff @{:a 1 :b {:c 1 :d 2}} @{:b {:c 1 :d 5}}) @[@{:a 1 :b @{:d 2}} @{:b @{:d 5}} @{:b @{:c 1}}]))
  )
  
### Structs and Tables

(comment
  (deftest "Should be: Struct and Table, same" (test (diff {:a 1 :b 2} @{:a 1 :b 2}) @[nil nil @{:a 1 :b 2}]))
  (deftest "Should be: Struct and Table, different" (test (diff {:a 1 :b 2} @{:a 1 :b 2 :c 4 :d 5}) @[nil @{:c 4 :d 5} @{:a 1 :b 2}]))
  (deftest "Should be: Array and Tuple, same" (test (diff @[1 2 3] [1 2 3]) @[nil nil @[1 2 3]]))
  (deftest "Should be: Array and Tuple, different" (test (diff @[1 2 3] [1 2 3 4 5]) @[nil @[nil nil nil 4 5] @[1 2 3]]))
  )

### Nested Complex Data Structures 

(comment
  (deftest "Should be: Nested Complex Data Structures, same" (test (deep= (diff @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}} @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}) @[nil nil @{6 @{7 @{:f "test" :g @{8 @[9 10 11] :h 12}}} :a @[1 2 @{:b @{:c 3}}] 5 @[:d :e 4]}]) true))
  (deftest "Should be: Nested Complex Data Structures, deep insertion" (test (deep= (diff @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}} @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 {:z 100} 11] :h 12}}}}) @[@{6 @{7 @{:g @{8 @[nil nil 11]}}}} @{6 @{7 @{:g @{8 @[nil nil {:z 100} 11]}}}} @{5 @[:d :e 4] 6 @{7 @{:f "test" :g @{8 @[9 10] :h 12}}} :a @[1 2 @{:b @{:c 3}}]}]) true))
  (deftest "Should be: Nested Complex Data Structures, deep delete" (test (deep= (diff @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}} @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11]}}}}) @[@{6 @{7 @{:g @{:h 12}}}} nil @{6 @{7 @{:g @{8 @[9 10 11]} :f "test"}} 5 @[:d :e 4] :a @[1 2 @{:b @{:c 3}}]}]) true))
  (deftest "Should be: Nested Complex Data Structures, deep update" (test (deep= (diff @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}} @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [:z 10 11] :h 12}}}}) @[@{6 @{7 @{:g @{8 @[9]}}}} @{6 @{7 @{:g @{8 @[:z]}}}} @{6 @{7 @{:f "test" :g @{8 @[nil 10 11] :h 12}}} 5 @[:d :e 4] :a @[1 2 @{:b @{:c 3}}]}]) true))
  (deftest "Should be: Nested Complex Data Structures, deep update of a whole structure" (test (deep= (diff @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}} @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 {:z 100 :x 10000 :y 100000} :h 12}}}}) @[@{6 @{7 @{:g @{8 [9 10 11]}}}} @{6 @{7 @{:g @{8 {:x 10000 :y 100000 :z 100}}}}} @{:a @[1 2 @{:b @{:c 3}}] 5 @[:d :e 4] 6 @{7 @{:f "test" :g @{:h 12}}}}]) true))
  (deftest "Should be: Nested Complex Data Structures, multiple deep updates" (test (deep= (diff @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}} @{:zz [1 10000 {:b {:c 3}}] 5 @[:d :e 1000] 6 @{7 {:f "test" :g {8 [9 10 11 {:z 10}] :h 12}}}}) @[@{5 @[nil nil 4] :a [1 2 {:b {:c 3}}]} @{5 @[nil nil 1000] 6 @{7 @{:g @{8 @[nil nil nil {:z 10}]}}} :zz [1 10000 {:b {:c 3}}]} @{5 @[:d :e] 6 @{7 @{:f "test" :g @{8 @[9 10 11] :h 12}}}}]) true))
  )
