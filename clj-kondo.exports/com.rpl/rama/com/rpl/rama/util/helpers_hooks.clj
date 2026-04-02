(ns com.rpl.rama.util.helpers-hooks
    "Shim namespace for backwards compatibility.
  Delegates to com.rpl.rama-hooks/defrecord+-hook."
    (:require [com.rpl.rama-hooks :as hooks]))

(def defrecord+-hook hooks/defrecord+-hook)
