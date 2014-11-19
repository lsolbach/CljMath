[
 :module "CljMathLibrary"
 :project "org.soulspace.clj"
 :type :library
 :version "0.2.0"
 :description "Clojure math library"
 :plugins ["global"
           ["org.soulspace.baumeister/DependencyPlugin"]
           ["org.soulspace.baumeister/ClojurePlugin"]
           ["org.soulspace.baumeister/ClojureTestPlugin"]
           ["org.soulspace.baumeister/PackagePlugin"]]
 :dependencies [["org.clojure/clojure, 1.5.1"]]
 ]