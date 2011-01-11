con pattern :: Type -> Type -> Type

con tagInternal :: {Unit} -> Type

val null : pattern unit (variant [])

val tag : attrs ::: {Unit} -> folder attrs -> string -> $(mapU string attrs)
          -> pattern (tagInternal attrs) {Attrs : $(mapU string attrs), Cdata : option string}

val tagA : attrs ::: {Unit} -> folder attrs -> string -> $(mapU string attrs)
          -> pattern (tagInternal attrs) $(mapU string attrs)
val tagC : string -> pattern (tagInternal []) string

con childrenInternal :: Type -> {Type} -> Type

val children : parentI ::: Type -> parent ::: Type -> children ::: {(Type * Type)}
               -> pattern parentI parent -> $(map (fn (i, d) => pattern i d) children) -> folder children
               -> pattern (childrenInternal parentI (map fst children)) (parent * $(map snd children))

con treeInternal :: Type -> Type -> Type

val tree : parentI ::: Type -> parent ::: Type -> childI ::: Type -> child ::: Type
           -> pattern parentI parent -> pattern childI child
           -> pattern (treeInternal parentI childI) (parent * child)

val app : internal ::: Type -> data ::: Type -> pattern internal data -> (data -> transaction {}) -> string -> transaction {}
