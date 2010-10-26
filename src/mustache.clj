(ns mustache
  "Mustache template renderer"
  (:use [clojure.contrib.java-utils :only (as-str)]
        [clojure.java.io :as io :only (reader resource)])
  (:import (java.io StringReader PushbackReader FileNotFoundException)))

(def otag)
(def ctag)

;; Copied from weavejester's hiccup
(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  (.. ^String (as-str text)
    (replace "&"  "&amp;")
    (replace "<"  "&lt;")
    (replace ">"  "&gt;")
    (replace "\"" "&quot;")))

(defn parse-reader [reader]
  (binding [otag "{{"
            ctag "}}"]
    (loop [token []
           model []
           state :static]
      (let [i (.read reader)]
        (if (= i -1)
          ;; End of stream
          (if (= state :static)
            (if (seq token)
              (conj model {:type :static :content (apply str token)})
              model)
            (throw (Exception. (str "Mustache tag not closed at end of template: " (apply str token)))))
          ;; Parse next character
          (let [c (char i)]
            (cond
             ;; Possible start of tag
             (and (= state :static) (= c (first otag)))
             (let [r (rest otag)
                   buf (char-array (count r))]
               (.read reader buf 0 (count r))
               (if (= r (seq buf))
                 (recur []
                        (if (seq token)
                          (conj model {:type :static, :content (apply str token)})
                          model)
                        :in-tag)
                 (do (.unread reader buf)
                     (recur (conj token c) model state))))
             ;; Possible end of tag
             (and (= state :in-tag) (= c (first ctag)))
             (let [r (rest ctag)
                   buf (char-array (count r))]
               (.read reader buf 0 (count r))
               (if (= r (seq buf))
                 (let [tagcontent (.trim (apply str token))]
                   (if-let [[_ open close] (re-matches #"^=([^\s=]+)\s+([^\s=]+)=$" tagcontent)]
                     (binding [otag open
                               ctag close]
                       (recur [] model :static))
                     (recur []
                            (conj model {:type :tag, :content tagcontent})
                            :static)))
                 (do (.unread reader buf)
                     (recur (conj token c) model state))))
             :default (recur (conj token c) model state))))))))

(defn parse-low-level [input]
  (with-open [r (PushbackReader.
                 (if (or (> (.indexOf input (int \newline)) -1)
                         (> (.indexOf input (int \{)) -1)
                         (> (.indexOf input (int \space)) -1))
                   (StringReader. input)
                   (try (io/reader input)
                        (catch FileNotFoundException e
                          (io/reader (io/resource input))))))]
    (parse-reader r)))

(defn parse-transform [input]
  (for [item (parse-low-level input)]
    (let [t (:type item)]
      (cond
       (= :static t) item
       (= :tag t) (let [[_ modifier name] (re-matches #"^([!/#^>]?)\s*(.+)$" (:content item))]
                    (cond
                     (= modifier "")  {:type :var, :name (keyword name)}
                     (= modifier "!") {:type :comment, :content name}
                     (= modifier "#") {:type :section-start, :name (keyword name)}
                     (= modifier "^") {:type :section-start, :name (keyword name), :inverted true}
                     (= modifier "/") {:type :section-end, :name (keyword name)}
                     (= modifier ">") {:type :partial, :name (keyword name)}))))))

(defn remove-leading-whitespace-line [s]
  (let [out (drop-while #(and (java.lang.Character/isWhitespace %) (not (= % \newline))) s)]
    (if (= (first out) \newline)
      (apply str (rest out))
      s)))

(defn remove-trailing-whitespace-line [s]
  (let [out (drop-while #(and (java.lang.Character/isWhitespace %) (not (= % \newline))) (reverse s))]
    (if (= (first out) \newline)
      (apply str (reverse out))
      s)))

(defn remove-whitespace-after-section-tags [items]
  (cons (first items)
        (map (fn [this before]
               (if (and (= (:type this) :static) (or (= (:type before) :section-start) (= (:type before) :section-end)))
                 (assoc this :content (remove-leading-whitespace-line (this :content)))
                 this))
             (rest items)
             items)))

(defn remove-whitespace-before-section-tags [items]
  (map (fn [this after]
         (if (and (= (:type this) :static) (or (= (:type after) :section-start) (= (:type after) :section-end)))
           (assoc this :content (remove-trailing-whitespace-line (this :content)))
           this))
       items
       (concat (rest items) [{}])))

(defn recursive-section-handler [items]
  (loop [items items
         out []
         section nil
         sectioncontent []]
    (if-let [i (first items)]
      (if section
        (if (and (= (:type i) :section-end) (= (:name i) (:name section)))
          (recur (rest items) (conj out (assoc section :content (recursive-section-handler sectioncontent))) nil [])
          (recur (rest items) out section (conj sectioncontent i)))
        (cond
         (= (:type i) :section-start) (recur (rest items) out (assoc i :type :section) [])
         (= (:type i) :section-end) (throw (Exception. (str "Closing tag without opening tag for section: " (:name i))))
         :default (recur (rest items) (conj out i) nil [])))
      (if section
        (throw (Exception. (str "Section " (:name section) " not closed!")))
        out))))

(defn parse [input]
  (recursive-section-handler (remove-whitespace-after-section-tags (remove-whitespace-before-section-tags (parse-transform input)))))

(defmulti render-item :type)
(defmethod render-item :static [item context]
           (print (:content item)))

(defmethod render-item :var [item context]
           (print (escape-html (context (:name item)))))

(defmethod render-item :section [item context]
           (let [value (context (:name item))]
             (when (and (not (:inverted item)) value (not (and (coll? value) (empty? value))))
               (cond
                (map? value) (dorun (map #(render-item % (merge context value)) (:content item)))
                (coll? value) (dorun (for [i value
                                           tmpl (:content item)]
                                       (render-item tmpl (merge context i))))
                :default (dorun (map #(render-item % context) (:content item)))))
             (when (and (:inverted item) (or (not value) (empty? value)))
               (dorun (map #(render-item % context) (:content item))))))

(defn compile [input]
  (let [template-items (parse input)]
    (fn [context]
      (dorun (map #(render-item % context) template-items)))))

(defn render [template context]
  ((compile template) context))

(defn render-to-string [& args]
  (with-out-str (apply render args)))
