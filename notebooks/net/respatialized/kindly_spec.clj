(ns ^{:nextjournal.clerk/toc true} net.respatialized.kindly-spec
  (:require [nextjournal.clerk :as clerk]
            [malli.core :as m]
            [malli.util :as mu]
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


;; ## The specification

(def Form
  "The map representation of a Clojure form & value used by Kindly"
  (m/schema
   [:map
    {:description
     "The map representation of a Clojure form & value used by Kindly"}
    [:code
     {:description "The source code of a form that produced a Kindly value"}
     :string]
    [:form {:description "The Clojure form that produced a Kindly value"} :any]
    [:value {:description "The Kindly value returned by a Clojure form"} :any]
    [:kind {:description "The Kindly kind annotation for the value"} :keyword]
    [:kindly/hide-code
     {:description "Whether to hide the source expression in the output"
      :optional    true} :boolean]
    [:kindly/options
     {:description "Additional options for kindly forms" :optional true}
     [:maybe
      [:map
       ;; :hide-value is an undocumented option from the kindly-render
       ;; library but it make sense to include it here because the
       ;; capability of hiding results is also required by the template
       ;; implementation
       [:hide-value
        {:optional true :description "Whether to hide the value in the output."}
        :boolean]]]]]
   #_[:schema {:registry {::form :map}}]))

(def Fragment
  "A Kindly fragment, a special Kindly value consisting of a vector of multiple kindly values."
  (mu/merge Form
            (m/schema
             [:map
              {:description
               "A Kindly fragment consisting of multiple kindly values"}
              [:kind {:description "The kindly type (fragment)"} [:= :fragment]]
              [:value {:description "The vector of kindly values"}
               [:sequential Form]]])))



(comment
  (kind/vector [3])
  (kind/vector 3)
  (= (kind/code 3) (kind/code [3]))
  (kind/fragment [(kind/code 'a) (kind/hiccup [:div "example"])]))


(comment
  (clerk/serve! {:browse true}))
;; # Specification 2

;; We will take a compositional approach, starting from the necessary metadata.

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

(def kindly-properties? (m/validator Kind-Properties))

(defn kindly-metadata?
  [v]
  (and (instance? clojure.lang.IObj v) (kindly-properties? (meta v))))

(kindly-metadata? ^{:kind :vector} [1 2 3])

(def Wrapped-Value-Properties
  (mu/merge
   Kind-Properties
   [:map
    {:description
     "A value carrying metadata indicating that it has been wrapped to support kindly metadata"}
    [:kindly/options [:map [:wrapped-value [:= true]]]]]))

(defn wrapped-value?
  [v]
  (and (vector? v) (m/validate Wrapped-Value-Properties (meta v))))

(m/schema [:and [:fn wrapped-value?] [:vector {:min 1 :max 1} :any]])

(def Kindly-Value
  (m/schema
   [:schema
    {:registry
     ;; start from the top and proceed downward
     {::kindly        [:or [:ref ::meta-value] [:ref ::wrapped-value]
                       [:ref ::kindly-map] #_[:ref ::fragment]]
      ::meta-value    [:and [:fn kindly-metadata?] [:ref ::value]]
      ::wrapped-value [:and [:fn wrapped-value?] [:vector {:min 1 :max 1} :any]]
      ::kindly-map    (mu/merge Kind-Properties
                                [:map [:code :string] [:form :any]
                                 [:value :any #_[:ref ::value]]])
      #_#_::fragment
        [:or [:and [:fn kindly-metadata?] [:vector [:ref ::kindly]]]
         (mu/merge Kind-Properties
                   [:map [:code :string] [:form :any] [:kind [:= :fragment]]
                    [:value [:vector [:ref ::kindly]]]])]
      ::value
      ;; a value is a kindly or plain Clojure value
      [:or [:ref ::kindly] :any]}} ::kindly]))

(def example-nested-value
  ^{:kind :fragment}
  [^{:kind :hiccup}
   [:div {:class "example"}
    "An example Hiccup div containing a nested kindly element"
    (kind/md "Nested **markdown** text in a Hiccup element")]
   ^{:kind :code} '(+ 3 4 5 6)
   {:kind :md :value "_More_ markdown text inside of a Kindly map."}
   ^{:kind :edn} {:a 1 :b 2 :description "EDN map"}])
