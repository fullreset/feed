(* This module implements imperative processing of XML feeds.
 *
 * Module author: Adam Chlipala
 *)

con pattern :: Type -> Type -> Type
(* A pattern describes a set of XML subtrees, mapping each element of the set to
 * a data value.  A value of type [pattern internal result] uses values of type
 * [internal] internally, but this API exposes no details of that usage.  The
 * type [result] gives the type used in mappings of matched subtrees. *)

val null : pattern unit (variant [])
(* A null pattern matches nothing, returning a value of the impossible empty
 * type if it ever does match. *)

con tagInternal :: {Unit} -> Type

val tag : attrs ::: {Unit} -> folder attrs -> string -> $(mapU string attrs)
          -> pattern (tagInternal attrs) {Attrs : $(mapU string attrs), Cdata : option string}
(* A basic [tag] pattern matches a single tag with a number of required
 * attributes.  A result value gives the attribute values and an optional
 * CDATA value for the text content of the tag.  The [string] argument is the
 * tag name, and the following argument gives attribute names. *)

val tagA : attrs ::: {Unit} -> folder attrs -> string -> $(mapU string attrs)
          -> pattern (tagInternal attrs) $(mapU string attrs)
(* A version of [tag] that ignores CDATA *)

val tagAV : attrs ::: {Unit} -> folder attrs -> string -> $(mapU (string * option string) attrs)
            -> pattern (tagInternal attrs) $(mapU string attrs)
(* Extension of tagA with optional specification of values which attributes must
 * bear in order to count as a match. *)

val tagAO : attrs ::: {Unit} -> folder attrs -> string -> $(mapU string attrs)
            -> pattern (tagInternal attrs) $(mapU (option string) attrs)
(* A version of [tagA] that makes each attribute optional *)

val tagC : string -> pattern (tagInternal []) string
(* A version of [tag] that only matches tags with nonempty CDATA and returns
 * only that text *)

con childrenInternal :: Type -> {Type} -> Type

val children : parentI ::: Type -> parent ::: Type -> children ::: {(Type * Type)}
               -> pattern parentI parent -> $(map (fn (i, d) => pattern i d) children) -> folder children
               -> pattern (childrenInternal parentI (map fst children)) (parent * $(map snd children))
(* A combinator that takes in a pattern for a parent node and a set of patterns
 * that must be matched against children of the parent.  This combinator will
 * find at most one match per matching parent node. *)

val childrenO : parentI ::: Type -> parent ::: Type -> children ::: {(Type * Type)}
                -> pattern parentI parent -> $(map (fn (i, d) => pattern i d) children) -> folder children
                -> pattern (childrenInternal parentI (map fst children)) (parent * $(map (fn (i, d) => option d) children))
(* A version of [children] where each child pattern need not be matched *)

con treeInternal :: Type -> Type -> Type

val tree : parentI ::: Type -> parent ::: Type -> childI ::: Type -> child ::: Type
           -> pattern parentI parent -> pattern childI child
           -> pattern (treeInternal parentI childI) (parent * child)
(* A combinator that takes in a pattern for a parent node and another pattern to
 * be matched at any depth within the parent's subtree.  Unlike [children],
 * [tree] finds as many subtree matches per parent node as possible. *)

type document
val show_document : show document
(* Type of uninterpreted XML documents *)

val fetch : string -> transaction document
(* Retrieve a document by URL. *)

val app' : internal ::: Type -> data ::: Type -> acc ::: Type -> pattern internal data 
           -> (data -> acc -> transaction acc) -> document -> acc -> transaction acc
(* Find all matches of a pattern in a document, running an imperative function
 * on the data returned by each match while threading through some state. *)

val app : internal ::: Type -> data ::: Type -> pattern internal data -> (data -> transaction {}) -> document -> transaction {}
(* Find all matches of a pattern in a document, running an imperative function
 * on the data returned by each match. *)
