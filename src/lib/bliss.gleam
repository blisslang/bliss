pub const prelude = "
  (defmacro def [name &exprs]
    (let name (fn [] &exprs)))

  (defmacro defn [name params &exprs]
    (let name (fn params &exprs)))

  (defmacro when [pred &exprs]
    (cond (pred &exprs)))

  (defmacro unless [pred &exprs]
    (when (not pred) &exprs))

  (defmacro if [pred thenb elseb]
    (cond (pred thenb)
          (else elseb)))
"
