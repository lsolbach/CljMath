[
 :module "CljComplexMathLibrary"
 :project "org.soulspace.clj"
 :type :library
 :version "0.4.0"
 :description "The CljComplexMathLibrary is a library for complex numbers"
 :plugins [["org.soulspace.baumeister/ClojurePlugin"]
           ["org.soulspace.baumeister/ClojureTestPlugin"]
           ["org.soulspace.baumeister/PackagePlugin"]]
 :dependencies [["org.clojure/clojure, 1.8.0"]
                ["org.soulspace.clj/CljMathLibrary, 0.5.0"]]
 ]
