(ns mustache
  "Mustache template renderer"
  (:import (java.io StringReader PushbackReader)))

(def otag)
(def ctag)

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

(defprotocol Parse-Template
  (parse-low-level [input]))

(extend-protocol Parse-Template
  String (parse-low-level [input] (parse-reader (PushbackReader. (StringReader. input)))))

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
  (apply str
         (let [out (drop-while #(and (java.lang.Character/isWhitespace %) (not (= % \newline))) s)]
           (if (= (first out) \newline)
             (rest out)
             out))))

(defn remove-whitespace-after-section-tags [items]
  (cons (first items)
        (map (fn [this before]
               (if (and (= (:type this) :static) (or (= (:type before) :section-start) (= ( :type before) :section-end)))
                 (assoc this :content (remove-leading-whitespace-line (this :content)))
                 this))
             (rest items)
             items)))

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
  (recursive-section-handler (remove-whitespace-after-section-tags (parse-transform input))))

(defmulti render-item :type)
(defmethod render-item :static [item context]
           (print (:content item)))

(defmethod render-item :var [item context]
           (print (context (:name item))))

(defmethod render-item :section [item context]
           (if-let [value (context (:name item))]
             (cond
              (map? value) (dorun (map #(render-item % value) (:content item)))
              (coll? value) (dorun (for [i value
                                       tmpl (:content item)]
                                   (render-item tmpl i)))
              :default (dorun (map #(render-item % context) (:content item))))))

(defn compile [input]
  (let [template-items (parse input)]
    (fn [context]
      (dorun (map #(render-item % context) template-items)))))

(defn render [template context]
  ((compile template) context))

(defn render-to-string [& args]
  (with-out-str (apply render args)))
