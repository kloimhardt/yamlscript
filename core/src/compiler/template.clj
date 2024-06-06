(ns compiler.template
  (:import (org.antlr.stringtemplate StringTemplateGroup StringTemplate)))

#_(defn indexed ;;;klm999
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn create-view "Return new view template - useful as mentioned here:
    http://hardlikesoftware.com/weblog/2006/12/12/using-json-with-stringtemplate/"
  ([]
   (StringTemplate.))
  ([^String template]
   (StringTemplate. template)))


(defn stringify [any]
  (if (keyword? any)
    (name any)
    (str any)))


(declare kv-to-sv)
(declare scan-kv-to-sv)


(defn each-kv-to-sv "If element is a collection type, do deep transformation"
  [each]
  (if (map? each)
    (kv-to-sv each)
    (if (or (vector? each) (list? each) (seq? each) (set? each))
      (scan-kv-to-sv each)
      each)))


(defn scan-kv-to-sv
  "Scans a collection and turns any contained map within from kv to sv"
  [coll]
  (map each-kv-to-sv coll))


(defn kv-to-sv
  "Transforms keyword-value map {:a 10 :b 20 :c 30}
     to string-value map {\"a\" 10 \"b\" 20 \"c\" 30}"
  [mp]
  (let [m (into {} mp)
        k (keys m)
        v (vals m)]
    (zipmap
     (map stringify k)
     (scan-kv-to-sv v))))

#_#_:klm (defn get-view-from-classpath "Return the view template from classpath"
  [^String view-name]
  (let [st-group (StringTemplateGroup. "default")]
    (.getInstanceOf st-group view-name)))


#_#_:klm (defn get-view-from-dir "Return the view template from specified directory"
  [^String view-name ^String root-dir]
  (let [st-group (StringTemplateGroup. "default" root-dir)]
    (.getInstanceOf st-group view-name)))


#_#_:klm (defn reset-view! "Reset view template with supplied content"
  [^StringTemplate view ^String template]
  (.setTemplate view template))


(defn fill-view! "Fill view template with key/value pairs"


  ([^StringTemplate template k v]
   (let [sk (stringify k)
         ksv (each-kv-to-sv v)]
   (cond
     (and (= (type sk) java.lang.String)
          (= (type ksv) java.lang.String))
     (.setAttribute template ^java.lang.String sk ^java.lang.String ksv)

     (and (= (type sk) java.lang.String)
          (= (type ksv) clojure.lang.LazySeq))
     (.setAttribute template ^java.lang.String sk ^clojure.lang.LazySeq ksv)

     :else (println "fill-view hopefully never gets here"))

   ;; (.setAttribute template (stringify k) (each-kv-to-sv v)) ;;klm999
   )
   template)


  ([^StringTemplate template kv-map]

   (println "fill-view 2 hopefully never called ") ;;klm99
   ;; (.setAttributes template (kv-to-sv kv-map))
   template))


(defn render-view "Return rendered view for the template"
  [^StringTemplate template]
  (.toString template))

(defmacro render-template [template & vars]
  `(let [vars# (hash-map ~@vars)
         view# (create-view ~template)]
     (doseq [[k# v#] vars#]
       (fill-view! view# (name k#) v#))
     (render-view view#)))
