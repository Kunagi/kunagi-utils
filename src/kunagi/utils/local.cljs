(ns kunagi.utils.local
  (:require
    [clojure.string :as str]))

;;; language

(defonce LANG (atom :de))
(defonce FALLBACK_LANG (atom :de))

(defn lang []
  @LANG)

(defn fallback-lang []
  @FALLBACK_LANG)

(defn set-lang! [language fallback-language-key]
  (let [k (cond
            (str/blank? language) fallback-language-key
            (-> language (.startsWith "de")) :de
            (-> language (.startsWith "en")) :en
            :else fallback-language-key)]
    (reset! LANG k)
    (reset! FALLBACK_LANG fallback-language-key)))

;;; utils

(defn ->js-locale-string [lang]
  (cond
    (string? lang) lang
    (= lang :de) "de-DE"
    (= lang :en) "en-US"
    :else "de-DE"))

;;; numbers

(defn format-decimal
  ([v]
   (when v (format-decimal @LANG v nil)))

  ([v fraction-digits]
   (when v (format-decimal @LANG v fraction-digits)))

  ([lang v fraction-digits]
   (when v
     (-> js/Intl
         (.NumberFormat (->js-locale-string lang)
                        (clj->js {:style "decimal"
                                  :minimumFractionDigits (when (int? fraction-digits)
                                                           fraction-digits)
                                  :maximumFractionDigits (when (int? fraction-digits)
                                                           fraction-digits)}))
         (.format (cond
                    (number? v) v
                    :else v))))))



;;; text

(def texts--de
  {:yes "Ja"
   :no "Nein"
   :ok "OK"
   :cancel "Abbrechen"
   :continue "Weiter"
   :error "Fehler"
   :delete "Löschen"


   :delete-image? "Bild löschen?"

   :authentication-required "Anmeldung erforderlich"
   :sign-in "Anmelden"

   :invalid-input "Ungültige Eingabe"
   :form-field-input-required "Eingabe erforderlich"})

(defonce TEXTS (atom {:de texts--de}))

(defn text
  ([k]
   (text @LANG k nil))

  ([k opts]
   (text @LANG k opts))

  ([lang k opts]
   (when k
     (let [v (get-in @TEXTS [lang k])]
       (cond
         (nil? v) nil
         (fn? v) (v opts)
         (string? v) v
         :else (str "?:" v ":?"))))))

(comment
 (text :yes)
 (text :continue)
 (text nil))

(defonce ON_TEXTC (atom nil))

(defn textc
  ([texts]
   (textc @LANG nil texts))

  ([opts texts]
   (textc @LANG opts texts))

  ([lang opts texts]
   (cond

     (nil? texts) nil

     (vector? texts) (if (= lang :de)
                       (second texts)
                       (text lang (-> texts first name keyword) opts))

     (map? texts)
     (let [v (or (get texts lang)
                 (text lang (:id texts) opts)
                 (get texts @FALLBACK_LANG))]
       (cond
         (nil? v) nil
         (fn? v) (v opts)
         (string? v) v
         :else (str v)))

     (fn? texts)
     (texts opts)

     (string? texts)
     (do
       #_(js/console.warn "TEXT string" texts)
       texts)

     (keyword? texts) (text lang texts opts)

     (sequential? texts) (->> texts (str/join " "))

     :else (str texts))))

;;; boolean

(defn format-yes-no
  ([b]
   (format-yes-no @LANG b))
  ([lang b]
   (when-not (nil? b)
     (text lang (if b :yes :no) nil))))

(comment
 "nil" (format-yes-no :de nil) := nil
 "true" (format-yes-no :de true) := "Ja"
 "false" (format-yes-no :de false) := "Nein")
