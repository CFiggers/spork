(use ../spork/test)
(import ../build/spork/csv :as csv)

(use judge)

(start-suite)

(def test-file (slurp "./test/assets/csv/test-csv.csv"))

(assert (deep= (csv/parse test-file)
               @[@["header1" "value1"]
                 @["header2" "value2"]
                 @["header3" "value3"]
                 @["header3" "value3"]]))

(assert (deep= (csv/parse test-file true)
               @{"header1" @["value1"]
                 "header2" @["value2"]
                 "header3" @["value3"]
                 "header3_" @["value3"]}))

(assert (deep= (csv/parse test-file true true)
               @{:header1 @["value1"]
                 :header2 @["value2"]
                 :header3 @["value3"]
                 :header3_ @["value3"]}))

# Round trip
# (assert (= test-file (csv/encode (csv/parse test-file))))
# (assert (= test-file (csv/encode (csv/parse test-file true) true)))
# (assert (= test-file (csv/encode (csv/parse test-file true true) true true)))

(end-suite)
