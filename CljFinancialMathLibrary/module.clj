[
 :module "CljFinancialMathLibrary"
 :project "org.soulspace.clj"
 :type :library
 :version "0.3.0"
 :description "Clojure financial math library"
 :plugins ["global"
           ["org.soulspace.baumeister/ClojurePlugin"]
           ["org.soulspace.baumeister/ClojureTestPlugin"]
           ["org.soulspace.baumeister/PackagePlugin"]]
 :dependencies [["org.clojure/clojure, 1.7.0"]
                ["org.soulspace.clj/CljMathLibrary, 0.3.0"]]
 ]
