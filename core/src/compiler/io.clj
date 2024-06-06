(ns compiler.io
  (:gen-class)
  (:use [clojure.java.io]
        [clojure.tools [logging :only [warn info]]])
  (:import (org.apache.commons.io FileUtils)
           (java.io BufferedReader InputStreamReader)))

(def extension-cpp "cpp")

#_#_:klm (defn os-name []
  (let [os (-> (System/getProperty "os.name") .toLowerCase)]
    (cond (.contains os "win")      :windows
          (.contains os "mac")      :mac
          (or (.contains os "nix")
              (.contains os "nux")
              (.contains os "aix")) :unix
          (.contains os "sunos")    :solaris)))

(defn exit-failure []
  (System/exit 1))

#_#_:klm (defn exit-success []
  (System/exit 0))

(defn read-file-from-url [f]
  (with-open [in (.getResourceAsStream (ClassLoader/getSystemClassLoader) f)
              rdr (BufferedReader. (InputStreamReader. in))]
    (apply str (interpose \newline (line-seq rdr)))))

(defn read-file [f & [options]]
  (try
    (read-file-from-url f)
    (catch Exception e-url
      (try
        (if (nil? options)
          (FileUtils/readFileToString (file f))
          (FileUtils/readFileToString (file (str (:path options) f))))
        (catch Exception e-path
          (warn "error reading =>" f)
          (exit-failure))))))

#_(defn write-to-file [f s] ;;klm999
  (println "write-to-file hopefully never called") ;;klm999
  #_(FileUtils/writeStringToFile (file f) (.trim s)))

(defn escape-string [s]
  (org.apache.commons.lang.StringEscapeUtils/escapeJava s))

(defn file-path [file]
  (let [path (str (org.apache.commons.io.FilenameUtils/getPrefix file)
                  (org.apache.commons.io.FilenameUtils/getPath file))]
    (if (empty? path)
      "./"
      path)))

(defn file-extension [f]
  (org.apache.commons.io.FilenameUtils/getExtension f))

(defn file-base-name [f]
  (org.apache.commons.io.FilenameUtils/getBaseName f))

#_(defn file-exists [f] ;;klm999
  (.exists (file f)))

(defn make-file [p n e]
  (file (str p n "." e)))
