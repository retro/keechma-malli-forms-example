(ns app.ui.main
  (:require [keechma.next.helix.core :refer [with-keechma use-sub use-meta-sub dispatch]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            [helix.dom :as d]
            [helix.impl.props]
            [helix.children :refer [children only map] :rename {map map-children}]
            [keechma.next.controllers.malli-form.ui :as mfui]
            [cljs-bean.core :as bean]
            ["react" :refer [cloneElement]]))

(defn get-element-props
  [default-props props]
  (let [element-props (into {} (filter (fn [[k v]] (simple-keyword? k)) props))]
    (reduce-kv
     (fn [m k v]
       (let [prev-v (get k m)
             val (cond (and (fn? prev-v) (fn? v))
                       (fn [& args] (apply prev-v args) (apply v args))
                       (and (= :class k) (:class m)) (flatten [v (:class m)])
                       :else v)]
         (assoc m k val)))
     default-props
     element-props)))


(defnc Input [{:keechma.form/keys [controller] :input/keys [attr] :as props}]
  (let [value         (mfui/use-get-in-data props controller attr)
        default-props {:value (str value)
                       :onChange #(mfui/on-partial-change props controller attr (.. % -target -value))
                       :onBlur #(mfui/on-commit-change props controller attr)
                       :class "border border-black"}
        element-props (get-element-props default-props props)]
    (d/input {& element-props})))

(defn render-options [options]
  (map
   (fn [{:keys [value label]}]
     (d/option {:key value :value value} label))
   options))

(defnc Select [{:keechma.form/keys [controller]
                :input/keys [attr]
                :select/keys [options optgroups placeholder]
                :as props}]
  (let [value         (mfui/use-get-in-data props controller attr)
        default-props {:value (or value "")
                       :onChange #(mfui/on-atomic-change props controller attr (.. % -target -value))
                       :class "border border-black"}
        element-props (get-element-props default-props props)]
    (d/select {& element-props}
              (when placeholder
                (<>
                 (d/option {:value ""} placeholder)
                 (d/option {:value ""} "-")))
              (if optgroups
                (map
                 (fn [{:keys [label options]}]
                   (d/optgroup {:key label :label label}
                               (render-options options)))
                 optgroups)
                (render-options options)))))

(defnc InputErrors [{:keechma.form/keys [controller] :input/keys [attr] :as props}]
  (let [errors (mfui/use-get-in-errors props controller attr)]
    (when (seq errors)
      (d/ul
       {:class "text-xs text-red-700"}
       (map-indexed
        (fn [idx e]
          (d/li {:key idx} e))
        errors)))))

(defnc WrapInput [{:keys [label] :as props}]
  (let [wrapped-children
        (->> props
             children
             (map-children
              (fn [c]
                (cloneElement c (helix.impl.props/props {:keechma.form/controller (:keechma.form/controller props)
                                                         :input/attr (:input/attr props)
                                                         :keechma/app (:keechma/app props)})))))]
    ;; (println wrapped-children)
    (d/div
     (d/b label)
     (d/div wrapped-children))))

(defnc IterateFormAttr [{:input/keys [attr] :keechma.form/keys [controller] :as props}]
  (let [children-count (mfui/use-get-in-data props controller attr count)
        child (-> props children only)]
    (<>
     (map
      (fn [i]
        (cloneElement child (helix.impl.props/props {:keechma.form/controller (:keechma.form/controller props)
                                                     :input/attr (conj (:input/attr props) i)
                                                     :keechma/app (:keechma/app props)
                                                     :key i})))
      (range 0 children-count)))))

(defnc Profile [{:input/keys [attr] :keechma/keys [app] :as props}]
  (let [username-props (merge props {:label "Username" :input/attr (conj attr :username)})
        network-props (merge props {:label "Network" :input/attr (conj attr :network)})]
    (d/div
     {:class "border-black border p-4"}
     ($ WrapInput
        {& network-props}
        ($ Select
           {:select/placeholder "Select Network"
            :select/options [{:label "Facebook" :value :facebook}
                             {:label "Twitter" :value :twitter}]})
        ($ InputErrors))
     ($ WrapInput
        {& username-props}
        ($ Input)
        ($ InputErrors)))))

(defnc MainRenderer [props]
  (d/form
   {:onSubmit (fn [e]
                (.preventDefault e)
                (dispatch props :registration :on-submit))
    :class "p-4"}

   ($ WrapInput
      {:label "Email"
       :keechma.form/controller :registration
       :input/attr :email
       & props}
      ($ Input
         {:class "border border-black"
          :placeholder "email@example.com"})
      ($ InputErrors))
   ($ WrapInput
      {:label "Password"
       :keechma.form/controller :registration
       :input/attr :password
       & props}
      ($ Input
         {:class "border border-black"
          :placeholder "*********"})
      ($ InputErrors))
   ($ WrapInput
      {:label "Password Confirmation"
       :keechma.form/controller :registration
       :input/attr :password-confirmation
       & props}
      ($ Input
         {:placeholder "*********"})
      ($ InputErrors))
   (d/div
    (d/button
     {:type "button"
      :on-click #(dispatch props :registration :add-profile)}
     "+ Add profile")
    (d/div
     {:class "border border-grey-400 p-4"}
     ($ IterateFormAttr {:keechma.form/controller :registration
                         :input/attr [:profiles]
                         & props}
        ($ Profile)))
    ($ InputErrors {:keechma.form/controller :registration
                    :input/attr [:profiles]
                    & props}))
   (d/div
    (d/button
     {:type "button"
      :on-click #(dispatch props :registration :add-phone-number)}
     "+ Add phone number")
    (d/div
     {:class "border border-grey-400 p-4"}
     ($ IterateFormAttr {:keechma.form/controller :registration
                         :input/attr [:phone-numbers]
                         & props}
        ($ WrapInput
           {:label "Phone number"}
           ($ Input
              {:placeholder "123456789"})
           ($ InputErrors))))
    ($ InputErrors {:keechma.form/controller :registration
                    :input/attr [:phone-numbers]
                    & props}))
   (d/button "Submit")))

(def Main (with-keechma MainRenderer))