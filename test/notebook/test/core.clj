(ns notebook.test.core
  (:use [notebook.core]
        [clojure.test]))

(deftest test-line->node
  (is (= (line->node "# title")
         {:type :section :depth 1 :title "title" :content [] :empty false})))

(deftest test-hierarchy-4-section
  (is (= (transform ["# title" "line 1" "## sub 1" "" "sub line 1" "" "## sub 2" "" "sub line 2"])
         {:current :note, :type :note,
          :content
          [{:empty false,:type :section, :depth 1,:title "title",
            :content
            [{:content ["line 1"] :type :paragraph,:empty false}
             {:type :section, :depth 2,:title "sub 1",:empty false,
              :content
              [{:content ["sub line 1"], :type :paragraph, :empty false}]}
             {:type :section, :depth 2,:title "sub 2",:empty false,
              :content
              [{:content ["sub line 2"], :type :paragraph, :empty false}]}]}]})))

