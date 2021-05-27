(ns app.controllers.registration
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.pipelines.core :as pp :refer-macros [pipeline!]]
            [keechma.malli-forms.core :as mf]
            [keechma.next.controllers.malli-form :as mfc]
            [malli.core :as m]))

(derive :registration ::pipelines/controller)

(def schema-registry
  (merge
   (m/default-schemas)
   {::email [:and
             :string
             [:re {:error/message "must be a valid email"}
              #"^.+@.+$"]]
    ::password [:string {:min 8}]

    ::profile [:map
               [:network :string]
               [:username :string]]

    ::phone-number [:and
                    :string
                    [:re {:error/message "must be a valid phone"}
                     #"^[(]{0,1}[0-9]{3}[)]{0,1}[-\s\.]{0,1}[0-9]{3}[-\s\.]{0,1}[0-9]{4}$"]]

    :app/registration [:and
                       [:map
                        [:email ::email]
                        [:password ::password]
                        [:password-confirmation ::password]
                        [:profiles [:vector {:min 1} ::profile]]
                        [:phone-numbers [:vector {:min 1} ::phone-number]]]
                       [:fn {:error/message "password confirmation must match password"
                             :error/path [:password-confirmation]}
                        (fn [{:keys [password password-confirmation]}]
                          (= password password-confirmation))]]}))

(def form (mf/make-form schema-registry :app/registration nil))

(def vec-conj (fnil conj []))

(defn add-profile [form]
  (let [attr [:profiles]]
    (-> form
        (mf/update-in-data attr vec-conj {})
        (mf/validate-in attr))))

(defn add-phone-number [form]
  (let [attr [:phone-numbers]]
    (-> form
        (mf/update-in-data attr vec-conj "")
        (mf/validate-in attr))))

(def pipelines
  (merge
   mfc/pipelines
   {:keechma.on/start (pipeline! [value {:keys [meta-state*]}]
                                 (pp/swap! meta-state* mfc/init-form form))
    :add-profile (pipeline! [value {:keys [meta-state*]}]
                            (pp/swap! meta-state* update mfc/form-key add-profile))
    :add-phone-number (pipeline! [value {:keys [meta-state*]}]
                                 (pp/swap! meta-state* update mfc/form-key add-phone-number))
    :on-submit (-> (pipeline! [value {:keys [meta-state*]}]
                              (println "SUBMITTING" value))
                   mfc/wrap-submit)}))

(defmethod ctrl/prep :registration [ctrl] (pipelines/register ctrl pipelines))