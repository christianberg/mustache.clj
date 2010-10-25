(ns mustache.test
  (:use [mustache] :reload)
  (:use [lazytest.describe]))

(defmacro throws [exception & body]
  `(try
     ~@body
     false
     (catch ~exception e# true)))

(describe parse-low-level
          (testing "basic parsing"
            (it "parses a trivial static template"
                (= (parse-low-level "Hello") [{:type :static, :content "Hello"}]))
            (it "parses a variable tag"
                (= (parse-low-level "{{foo}}") [{:type :tag, :content "foo"}]))
            (it "ignores single brackets"
                (= (parse-low-level "{foo}") [{:type :static, :content "{foo}"}]))
            (it "ignores whitespace at start and end of tags"
                (= (parse-low-level "{{  foo
                                            }}") [{:type :tag, :content "foo"}]))
            (it "preserves whitespace between tags"
                (= (parse-low-level "{{foo}}\n{{bar}}") [{:type :tag, :content "foo"}{:type :static, :content "\n"}{:type :tag, :content "bar"}])))
          (testing "delimiters"
            (it "allows changing the tag delimiters"
                (= (parse-low-level "{{foo}}{{=<% %>=}}<%bar%>") [{:type :tag, :content "foo"}, {:type :tag, :content "bar"}]))
            (it "allows changing the tag delimiters back"
                (= (parse-low-level "{{foo}}{{=<% %>=}}<%bar%><%={{ }}=%>{{baz}}")
                   [{:type :tag, :content "foo"},
                    {:type :tag, :content "bar"},
                    {:type :tag, :content "baz"}])))
          (testing "error handling"
            (it "fails for unclosed tags"
                (throws Exception
                  (parse-low-level "{{foo")))))

(describe parse-transform
          (testing "basic parsing"
            (it "parses static text"
                (= (parse-transform "Hello") [{:type :static, :content "Hello"}]))
            (it "parses a variable tag"
                (= (parse-transform "{{foo}}") [{:type :var, :name :foo}]))
            (it "parses a comment"
                (= (parse-transform "{{ !  I'm a comment  }}") [{:type :comment, :content "I'm a comment"}]))
            (it "parses a section start"
                (= (parse-transform "{{#foo}}") [{:type :section-start, :name :foo}]))
            (it "parses an inverted section start"
                (= (parse-transform "{{^foo}}") [{:type :section-start, :name :foo, :inverted true}]))
            (it "parses a section end"
                (= (parse-transform "{{/foo}}") [{:type :section-end, :name :foo}]))
            (it "parses a partial"
                (= (parse-transform "{{> foo}}") [{:type :partial, :name :foo}]))))

(describe parse
          (testing "basic parsing"
            (it "parses static text"
                (= (parse "Hello") [{:type :static, :content "Hello"}]))
            (it "parses a variable tag"
                (= (parse "{{foo}}") [{:type :var, :name :foo}]))
            (it "parses a comment"
                (= (parse "{{ !  I'm a comment  }}") [{:type :comment, :content "I'm a comment"}])))
          (testing "sections"
            (it "should nest the inside of a section"
                (= (parse "{{#foo}}bar{{/foo}}") [{:type :section, :name :foo, :content [{:type :static, :content "bar"}]}]))
            (it "should handle nested sections"
                (= (parse "{{#foo}}{{#bar}}baz{{/bar}}{{/foo}}")
                   [{:type :section, :name :foo, :content [{:type :section, :name :bar, :content [{:type :static, :content "baz"}]}]}]))
            (it "should handle empty sections"
                (= (parse "{{#foo}}{{/foo}}") [{:type :section, :name :foo, :content []}]))
            (it "should handle inverted sections"
                (= (parse "{{^foo}}{{/foo}}") [{:type :section, :name :foo, :content [], :inverted true}])))
          (testing "section error handling"
            (it "should fail for section without closing tag"
                (throws Exception
                        (parse "{{#foo}}")))
            (it "should fail for section without opening tag"
                (throws Exception
                        (parse "{{/foo}}")))
            (it "should fail for incorrectly nested sections"
                (throws Exception
                        (parse "{{#foo}}{{#bar}}{{/foo}}{{/bar}}")))))

(describe render-to-string
          (it "renders trivial static template"
              (= (render-to-string "Hello" nil) "Hello"))
          (it "renders a simple template"
              (=
               (render-to-string "Hello {{name}}!" {:name "World"})
               "Hello World!"))
          (it "renders a template that is only a tag"
              (=
               (render-to-string "{{foo}}" {:foo "bar"})
               "bar"))
          (testing "sections"
            (it "skips whitespace after section tags"
                (= (render-to-string "foo\n{{#show?}}   \nbar\n{{/show?}}\nbaz", {:show? true})
                   "foo\nbar\nbaz"))
            (it "renders a section for a hash-map"
                (=
                 (render-to-string "{{#person}}Hi {{name}}!{{/person}}", {:person {:name "Waldo"}})
                 "Hi Waldo!"))
            (it "does not render a section for a non-existing value"
                (=
                 (render-to-string "a{{#b}}b{{/b}}c", {})
                 "ac"))
            (it "renders a section repeatedly for a vector"
                (=
                 (render-to-string "<ul>{{#crew}}<li>{{name}}</li>{{/crew}}</ul>", {:crew [{:name "Mal"}, {:name "Zoe"}, {:name "Wash"}]})
                 "<ul><li>Mal</li><li>Zoe</li><li>Wash</li></ul>"))
            (it "renders values from the outer context for boolean section value"
                (=
                 (render-to-string "Hello {{name}}\nYou have just won ${{value}}!\n{{#in_ca}}\nWell, ${{taxed_value}}, after taxes.\n{{/in_ca}}\n",
                                   {:name "Chris", :value 10000, :taxed_value (* 10000 0.6), :in_ca true})
                 "Hello Chris\nYou have just won $10000!\nWell, $6000.0, after taxes.\n"))))
