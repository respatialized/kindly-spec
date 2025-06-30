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
(= (meta (kind/code 3)) (meta (kind/code [3])))

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

;; This specification is not of Kindly values as they actually exist; the
;; `"wrapped-value"` schema, for example, carries the necessary metadata to
;; show that it is distinct from a single-element vector with Kindly metadata.

^{::clerk/visibility {:result :hide}}
(def Kindly-Value
  (m/schema
   [:schema
    {:registry
     ;; start from the top and proceed downward
     {"kindly"        [:or [:ref "meta-value"] [:ref "wrapped-value"]
                       [:ref "kindly-map"] [:ref "fragment"]]
      "meta-value"    [:and [:fn kindly-metadata?] [:ref "value"]]
      "wrapped-value" [:and [:fn wrapped-value?] [:vector {:min 1 :max 1} :any]]
      "kindly-map"    (mu/merge Kind-Properties
                                ;; the ref needs to be "pulled in" to the
                                ;; subschema here, apparently
                                [:map {:registry {"value" [:ref "value"]}}
                                 [:code :string] [:form :any]
                                 [:value [:ref "value"]]])
      "fragment"      [:or
                       [:and [:fn kindly-metadata?] [:vector [:ref "kindly"]]]
                       (mu/merge Kind-Properties
                                 [:map {:registry {"kindly" [:ref "kindly"]}}
                                  [:code :string] [:form :any]
                                  [:kind [:= :fragment]]
                                  [:value [:vector [:ref "kindly"]]]])]
      "value"
      ;; a value is a Kindly or plain Clojure value.
      [:or :any
       ;; putting the ref first means the base case doesn't get found and
       ;; the stack blows up
       [:ref "kindly"]]}} "kindly"]))


#_(clerk/md (malli.describe/describe Kindly-Value))

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

;; # Follow-up work

;; 1. Define transformers or functions that "canonicalize" a Kindly value into
;; the map representation
;; 2. Define an equality function in light of the canonical form
;; 3. Define generators for each of these subschemas and use them to build
;; generative tests of arbitrarily nested Kindly data structures


^{::clerk/visibility {:code :hide :result :hide}}
(comment
  (kind/vector [3])
  (kind/vector 3)
  (= (kind/code 3) (kind/code [3]))
  (kind/fragment [(kind/code 'a) (kind/hiccup [:div "example"])])
  (clerk/serve! {:browse true})
  (clerk/show! *ns*))
