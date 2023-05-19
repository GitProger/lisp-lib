
(defmacro defrec [name fields]
  `(defn ~name [& vals#]
    (let [mapping# (zipmap '~fields vals#)]
      #(get mapping# %))))

(defmacro getfield [obj name]
  `(~obj '~name))

(defmacro setfield [obj name val]
  `(fn [key#] 
    (if (= key# '~name) ~val
      (~obj key#))))

