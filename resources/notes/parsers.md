# Parsing Techniques

## Introduction

With a simple but pratical example : build a parser for markdown format file, compare different techniques to transform markown file to a clojure's data structure.

+ reduce / ->> / iterate
+ "real" parser : combinator incremental

## Naive Solutions 

Original file to parse
'''
###andreadipersio 2010-03-19 16:10:00###                                                                                
USER     COMM               PID  PPID  %CPU %MEM      TIME  
root     launchd              1     0   0.0  0.0   2:46.97  
root     DirectoryService    11     1   0.0  0.2   0:34.59  
root     notifyd             12     1   0.0  0.0   0:20.83  
root     diskarbitrationd    13     1   0.0  0.0   0:02.84`
....

###andreadipersio 2010-03-19 16:20:00###                                                                                
USER     COMM               PID  PPID  %CPU %MEM      TIME  
root     launchd              1     0   0.0  0.0   2:46.97  
root     DirectoryService    11     1   0.0  0.2   0:34.59  
'''

'''clojure Reduce Solution
(defn parse [file]
  (first
   (reduce
    (fn [[data interval] line]
      (cond
       (interval? line) [data (parse-line line)]
       (skip? line)     [data interval]
       :else            [(conj data (concat interval (parse-line line))) interval]))
    [[] nil]
    (io/read-lines file))))

'''

'''clojure ->> Solution
(->> (line-seq (java.io.BufferedReader. (java.io.StringReader. test-data)))
     (remove #(re-find discard-pattern %)) ; throw out "USER  COMM ..."
     (partition-by is-header?)
     (partition 2)
     ;; mapcat performs a map, then concatenates results
     (mapcat extract-fields-add-headers))

'''

Iterate solution based on [Malcolm spark blog](http://blog.malcolmsparks.com/?p=17) 