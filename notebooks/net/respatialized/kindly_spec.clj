^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns ^{:nextjournal.clerk/toc true} net.respatialized.kindly-spec
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as-alias v]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.experimental.describe :as malli.describe]
            [malli.dot]
            [malli.plantuml]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))

;; # A formal specification for Kindly values

;; ## Context

;; Kindly exists as a convention but not as a formal specification. This
;; namespace is intended to provide a preliminary attempt at specifying it.

;; ## Goal 1: Resolve ambiguity.

;; Use of kindly has indicated there are 3 primary ways of specifying
;; a "kindly value" depending on the context and tool.
;; 1. A Clojure value implementing the IObj interface with kindly-specific
;; metadata
;; 2. A Clojure value that does not implement the IObj interface, wrapped in a
;; vector
;; 3. A map with kindly-specific keys denoting the value and its kind.

;; At present, there exist values that are ambiguous between (1) and (2). This
;; is a brief example:

(= (kind/code 3) (kind/code [3]))
(= (meta (kind/code 3))
   ;; this appears to be a Clerk bug related to value metadata
   (dissoc (meta (kind/code [3])) :line :column))

;; This specification can hopefully resolve this ambiguity.

;; Because kindly values can contain arbitrary Clojure data structures, which
;; may themselves have Kindly metadata attached to them, and because Kindly
;; itself specifies a data structure that may contain one or more kindly
;; values—
;;  `:kind/fragment` — a kindly value must be defined in a mutually recursive
;; way.

;; ## Goal 2: Define equality for kindly values

;; Form (3) defined above is unambiguous and can serve as the "canonical form"
;; of a kindly value for equality checks.


;; # Specification (v1)

;; In order to build the schema, we will take a compositional approach,
;; starting
;; from the necessary metadata.

^{::clerk/visibility {:result :hide}}
(def Kind-Properties
  (m/schema
   [:map
    {:description
     "The properties required by Kindly, either as a map or in the form of metadata."}
    [:kind {:description "The kind of the value"} :keyword]
    [:kindly/hide-code
     {:description "Whether to hide the source expression in the output"
      :optional    true} :boolean]
    [:kindly/options
     {:description
      "Additional options for the kind. May be kind-specific or general."
      :optional true}
     [:maybe
      [:map
       [:hide-value
        {:optional true :description "Whether to hide the value in the output."}
        :boolean]
       [:wrapped-value
        {:optional true
         :description
         "Whether the value has been 'wrapped' in a vector to carry Kindly metadata"}
        :boolean]]]]]))

^{::clerk/visibility {:result :hide}}
(def kindly-properties? (m/validator Kind-Properties))

^{::clerk/visibility {:result :hide}}
(defn kindly-metadata?
  [v]
  (and (instance? clojure.lang.IObj v) (kindly-properties? (meta v))))


^{::clerk/visibility {:result :hide}}
(def Wrapped-Value-Properties
  (mu/merge
   Kind-Properties
   [:map
    {:description
     "A vector containing a single value, with metadata indicating that the value is wrapped"}
    [:kindly/options [:map [:wrapped-value [:= true]]]]]))

^{::clerk/visibility {:result :hide}}
(defn wrapped-value?
  [v]
  (and (vector? v) (m/validate Wrapped-Value-Properties (meta v))))

;; With these helper functions and map schemas in place, defining the schema
;; becomes simpler. Here's a first-pass example:

^{::clerk/visibility {:result :hide}}
(def Kindly-Value
  (m/schema
   [:schema
    {:registry
     ;; start from the top and proceed downward
     {:kindly/value [:or {:description "A Kindly value"}
                     [:ref :kindly/meta-value] [:ref :kindly/wrapped-val]
                     [:ref :kindly/map] [:ref :kindly/fragment]]
      :kindly/meta-value
      [:and {:description "A value with Kindly-specific metadata"}
       [:fn kindly-metadata?] [:ref :clojure/value] [:not [:ref :kindly/map]]]
      :kindly/wrapped-val
      [:and
       {:description "A plain value wrapped in a vector with Kindly metadata"}
       [:fn wrapped-value?] [:vector {:min 1 :max 1} :any]]
      :kindly/map
      (mu/merge Kind-Properties
                ;; the ref needs to be "pulled in" to
                ;; the subschema here, apparently
                [:map
                 {:registry    {:clojure/value [:ref :clojure/value]}
                  :description "A Kindly value as a plain Clojure map"}
                 [:code :string] [:form :any] [:value [:ref :clojure/value]]])
      :kindly/fragment
      [:or
       {:description "A Kindly fragment contains a sequence of Kindly values"}
       [:and [:fn kindly-metadata?] [:vector [:ref :kindly/value]]]
       (mu/merge Kind-Properties
                 [:map {:registry {:kindly/value [:ref :kindly/value]}}
                  [:code :string] [:form :any] [:kind [:= :fragment]]
                  [:value [:vector [:ref :kindly/value]]]])]
      :clojure/value
      [:or
       {:description
        "Kindly values are themselves Clojure values,
         but not all Clojure values are Kindly values."}
       [:and :any #_[:not [:ref :kindly/value]]]
       [:map-of [:ref :clojure/value] [:ref :clojure/value]]
       [:sequential [:ref :clojure/value]] [:set [:ref :clojure/value]]
       ;; putting the refs later ensures the base case gets found and
       ;; the stack doesn't blow up
       [:ref :kindly/value]]}} :kindly/value]))

;; From this, we can derive a basic natural-language description:

;; 1. A **Kindly value** is one of four things:
;;    1. A **meta value** - A **Clojure value** with Kindly-specific metadata.
;;    2. A **wrapped value** - A plain Clojure value in a single-element vector
;;    with Kindly metadata.
;;    3. A **Kindly map** - A plain Clojure map containing the same keys as
;;    Kindly metadata maps. The **Clojure value** is contained in this map
;;    under the `:value` key.
;;    4. A **fragment** - A **Kindly value** whose **Clojure value** is a
;; homogenous vector of **Kindly values**.

;; 2. A **Clojure value** is the value that Kindly is annotating. **Clojure
;; values** may themselves contain **Kindly values** at arbitrary levels.
;; A Clojure value is either:
;;    1. A Clojure value without Kindly metadata
;;    2. A Clojure collection containing **Clojure values**.
;;    3. A **Kindly value**.

;; This definition is self-referential, but because it contains the base case
;; of plain Clojure data structures in 2.1, it can cover both simple and
;; arbitrarily complex Kindly values.[^order-dependence] It will always "bottom
;; out" in ordinary Clojure values.

;; [^order-dependence]: The order of these cases _does_ matter for `malli`; if
;; the recursive case (2.3) is specified before the base case, attempts to
;; validate will not terminate.

;; This specification is not of Kindly values as they actually exist; the
;; `:kindly/wrapped-value` schema, for example, carries the necessary metadata
;; to show that it is distinct from a single-element vector with Kindly
;; metadata. Kindly currently does not return values with this information.

;; We can check validation against a nested example:

^{::clerk/visibility {:result :hide}}
(def example-nested-value
  ^{:kind :fragment}
  [^{:kind :hiccup}
   [:div {:class "example"}
    "An example Hiccup div containing a nested kindly element"
    (kind/md "Nested **markdown** text in a Hiccup element")]
   ^{:kind :code} '(+ 3 4 5 6)
   {:kind :md :value "_More_ markdown text inside of a Kindly map."}
   ^{:kind :edn} {:a 1 :b 2 :description "EDN map"}])

(m/validate Kindly-Value example-nested-value)


;; ## Questions raised by this specification

;; 1. A map representation of a Kindly value can itself carry Kindly metadata.
;; Should the explicit map values take priority over the map's metadata? I
;; believe the answer should be "yes."
;; 2. Should a fragment be restricted to being only explicit Kindly values? Or
;; should it allow for "plain" Clojure values to be displayed using the
;; implementation defaults?
;; 3. How should the schema specify "value that is not a Kindly value?" Trying
;; to use `[:and [:not [:ref :kindly/value]] :any]` creates a non-terminating
;; schema definition.
;; 4. Should the schema use `:multi` instead of `:or`? Using `meta` for the
;; `:dispatch` argument might simplify things quite a bit and help eliminate
;; the predicate functions.
;;   - no, `:or` is the correct construct to use, as `:multi` is used when you
;;     want to match on a concrete value and not on the schema/shape a value
;;     conforms to.

;; # Follow-up work

;; 1. Define transformers or functions that "canonicalize" a Kindly value into
;; the map representation
;; 2. Define an equality function in light of the canonical form
;; 3. Define generators for each of these subschemas and use them to build
;; generative tests of arbitrarily nested Kindly data structures
;; 4. Switch refs to
;; [vars?](https://github.com/metosin/malli/?tab=readme-ov-file#var-registry)
;; 5. "Inline" more of the helper definitions to create a self-contained schema
;; 6. Specify that meta values are not also kindly maps



^{::clerk/visibility {:code :hide :result :hide}}
(comment
  (kind/vector [3])
  (kind/vector 3)
  (= (kind/code 3) (kind/code [3]))
  (kind/fragment [(kind/code 'a) (kind/hiccup [:div "example"])])
  (clerk/serve! {:browse true})
  (clerk/show! *ns*))



;; # Specification (v2)

;; ## A custom `malli` Schema for metadata

;; As suggested on the Clojure slack, a new way of defining
;; a schema on a value would be to [extend Malli to a new
;; type](https://github.com/metosin/malli/?tab=readme-ov-file#custom-schema-types).
;; The basic idea:
;; **`[:meta ?schema]`**

;; This schema can then be readily composed with other schemas using `:and`
;; when necessary.

(defn compile-meta-schema
  ;; TODO: fix case of registry/props being passed in
  ([properties [?schema] _opts]
   (let [schema    (try (m/schema ?schema (select-keys properties [:registry]))
                        (catch Exception e
                          (m/-fail! ::invalid-schema
                                    {:schema ?schema :properties properties})))
         validator (m/validator schema)]
     {:pred #(validator (meta %)) :properties properties})))

;; this implementation of compile/simple-schema may not allow for `:ref`s from
;; local registries; when it tries to compile the schema it cannot `:ref`
;; schemas
;; outside the lexical scope of the `compile-meta-schema` function.

;; this is probably why other schemas that directly implement the
;; `-into-schema`
;; protocol have enormously long implementations; dereferencing the schemas
;; appropriately sounds tricky!



(def Meta
  (m/-simple-schema {:type `Meta :min 1 :max 1 :compile compile-meta-schema}))


(m/validate (m/schema [:and [:vector :int] [Meta [:map [:k :keyword]]]])
            ^{:k :abc} [1])

(m/validate (m/schema [:and [:vector :int]
                       [Meta {:description "test"} [:map [:k :keyword]]]])
            ^{:k :abc} [1])

(def registry (merge (m/default-schemas) (mu/schemas)))

^{::clerk/visibility {:result :hide}}
(def Kindly-Value-v2
  (m/schema
   [:schema
    {:registry
     {:kindly/properties
      (m/schema
       [:map
        {:description
         "The properties required by Kindly, either as a map or in the form of metadata."}
        [:kind {:description "The kind of the value"} :keyword]
        [:kindly/hide-code
         {:description "Whether to hide the source expression in the output"
          :optional    true} :boolean]
        [:kindly/options
         {:description
          "Additional options for the kind. May be kind-specific or general."
          :optional true}
         [:maybe
          [:map
           [:hide-value
            {:optional    true
             :description "Whether to hide the value in the output."} :boolean]
           [:wrapped-value
            {:optional true
             :description
             "Whether the value has been 'wrapped' in a vector to carry Kindly metadata"}
            :boolean]]]]])
      ;; start from the top and proceed downward
      :kindly/value [:or {:description "A Kindly value"}
                     [:ref :kindly/meta-value] #_[:ref :kindly/wrapped-val]
                     #_[:ref :kindly/map] #_[:ref :kindly/fragment]]
      :kindly/meta-value
      [:and {:description "A value with Kindly-specific metadata"}
       [Meta
        {:registry {:kindly/properties (m/deref [:ref :kindly/properties])}}
        :map #_[:ref :kindly/properties]] :any #_[:ref :clojure/value]
       [:not [:ref :kindly/map]]]
      #_#_:kindly/wrapped-val
        [:and
         {:description
          "A plain value wrapped in a vector with Kindly
          metadata"}
         [:fn wrapped-value?] [:vector {:min 1 :max 1} :any]]
      #_#_:kindly/map
        (mu/merge
         Kind-Properties
         ;; the ref needs to be "pulled in" to
         ;; the subschema here, apparently
         [:map
          {:registry {:clojure/value [:ref :clojure/value]}
           :description
           "A Kindly value as a plain Clojure
                     map"}
          [:code :string] [:form :any] [:value [:ref :clojure/value]]])
      #_#_:kindly/fragment
        [:or
         {:description
          "A Kindly fragment contains a sequence of Kindly
          values"}
         [:and [:fn kindly-metadata?] [:vector [:ref :kindly/value]]]
         (mu/merge Kind-Properties
                   [:map {:registry {:kindly/value [:ref :kindly/value]}}
                    [:code :string] [:form :any] [:kind [:= :fragment]]
                    [:value [:vector [:ref :kindly/value]]]])]
      #_#_:clojure/value
        [:or
         {:description
          "Kindly values are themselves Clojure values,
          but not all Clojure values are Kindly values."}
         [:and :any #_[:not [:ref :kindly/value]]]
         [:map-of [:ref :clojure/value] [:ref :clojure/value]]
         [:sequential [:ref :clojure/value]] [:set [:ref :clojure/value]]
         ;; putting the refs later ensures the base case gets found and
         ;; the stack doesn't blow up
         [:ref :kindly/value]]}} :kindly/value]))


(let [s (m/schema [:merge [:map [:k1 {:optional true} :boolean]]
                   [:map [:k1 [:= true]]]]
                  {:registry registry})]
  (m/validate s {:k1 false}))

(comment
  (m/validate [:multi {:dispatch (comp boolean meta)} [true vector?]
               [false map?]]
              (with-meta [1 2] {:some :thing}))
  (m/validate [:multi {:dispatch (comp boolean meta)} [true vector?]
               [false map?]]
              (with-meta {1 2} {:some :thing}))
  (m/validate [:multi {:dispatch (comp boolean meta)} [true vector?]
               [false map?]]
              {1 2})
  (m/schema [:merge (m/form Kind-Properties)
             [:map
              {#_#_:registry {:clojure/value [:ref :clojure/value]}
               :description "A Kindly value as a plain Clojure map"}
              [:code :string] [:form :any] [:value :any]]]))
