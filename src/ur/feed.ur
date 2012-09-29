task initialize = fn () => FeedFfi.init

con pattern internal output = {Initial : internal,
                               EnterTag : {Tag : string, Attrs : list (string * string), Cdata : option string} -> internal -> option internal,
                               ExitTag : internal -> option internal,
                               Finished : internal -> option (output * bool)}

val null : pattern unit (variant []) =
    {Initial = (),
     EnterTag = fn _ () => Some (),
     ExitTag = fn () => Some (),
     Finished = fn () => None}

con tagInternal (attrs :: {Unit}) = option {Attrs : $(mapU (option string) attrs), Cdata : option string}

fun tagG [attrs ::: {Unit}] [t ::: Type] (fl : folder attrs) (accept : {Attrs : $(mapU (option string) attrs), Cdata : option string} -> option t)
         (name : string) (attrs : $(mapU string attrs))
    : pattern (tagInternal attrs) t =
    {Initial = None,
     EnterTag = fn tinfo _ =>
                   if tinfo.Tag <> name then
                       None
                   else
                       let
                           val v = {Attrs = @mp [fn _ => string] [fn _ => option string]
                                             (fn [u] aname => List.assoc aname tinfo.Attrs)
                                             fl attrs,
                                    Cdata = tinfo.Cdata}
                       in
                           case accept v of
                               None => None
                             | Some _ => Some (Some v)
                       end,
     ExitTag = fn _ => None,
     Finished = fn state => case state of
                                None => None
                              | Some state =>
                                case accept state of
                                    None => None
                                  | Some v => Some (v, False)}

fun allPresent [attrs ::: {Unit}] (fl : folder attrs) (attrs : $(mapU (option string) attrs)) : option $(mapU string attrs) =
    @foldUR [option string] [fn attrs => option $(mapU string attrs)]
    (fn [nm ::_] [r ::_] [[nm] ~ r] os acc =>
        case (os, acc) of
            (Some s, Some acc) => Some ({nm = s} ++ acc)
          | _ => None)
    (Some {}) fl attrs

fun allPresentE [attrs ::: {Unit}] (fl : folder attrs) (vs : $(mapU (option string) attrs)) (attrs : $(mapU (option string) attrs))
    : option $(mapU string attrs) =
    @foldUR2 [option string] [option string] [fn attrs => option $(mapU string attrs)]
    (fn [nm ::_] [r ::_] [[nm] ~ r] os os' acc =>
        case (os, os', acc) of
            (Some s, Some s', Some acc) => if s = s' then Some ({nm = s'} ++ acc) else None
          | (None, Some s', Some acc) => Some ({nm = s'} ++ acc)
          | _ => None)
    (Some {}) fl vs attrs

fun tag [attrs ::: {Unit}] (fl : folder attrs) (name : string) (attrs : $(mapU string attrs))
    : pattern (tagInternal attrs) {Attrs : $(mapU string attrs), Cdata : option string} =
    @tagG fl (fn r =>
                 case @allPresent fl r.Attrs of
                     None => None
                   | Some attrs => Some (r -- #Attrs ++ {Attrs = attrs}))
     name attrs

fun tagA [attrs ::: {Unit}] (fl : folder attrs) (name : string) (attrs : $(mapU string attrs))
    : pattern (tagInternal attrs) $(mapU string attrs) =
    @tagG fl (fn r => @allPresent fl r.Attrs) name attrs

fun tagAV [attrs ::: {Unit}] (fl : folder attrs) (name : string) (attrs : $(mapU (string * option string) attrs))
    : pattern (tagInternal attrs) $(mapU string attrs) =
    let
        val as = @mp [fn _ => (string * option string)] [fn _ => string] (fn [u] (x, _) => x) fl attrs
        val vs = @mp [fn _ => (string * option string)] [fn _ => option string] (fn [u] (_, x) => x) fl attrs
    in
        @tagG fl (fn r => @allPresentE fl vs r.Attrs) name as
    end

fun tagAO [attrs ::: {Unit}] (fl : folder attrs) (name : string) (attrs : $(mapU string attrs))
    : pattern (tagInternal attrs) $(mapU (option string) attrs) =
    @tagG fl (fn r => Some r.Attrs) name attrs

fun tagAOR [optional ::: {Unit}] [required ::: {Unit}] [optional ~ required]
           (ofl : folder optional) (rfl : folder required)
           (name : string) (required : $(mapU string required)) (optional : $(mapU string optional))
    : pattern (tagInternal (optional ++ required)) $(mapU string required ++ mapU (option string) optional) =
    @tagG (@Folder.concat ! ofl rfl)
     (fn r => case @allPresent rfl (r.Attrs --- mapU (option string) optional) of
                  None => None
                | Some req => Some (r.Attrs --- mapU (option string) required ++ req))
     name (required ++ optional)

fun tagC (name : string) : pattern (tagInternal []) string =
    tagG (fn r => r.Cdata) name {}

datatype status a = Initial | Pending of a | Matched of a

con childrenInternal (parent :: Type) (children :: {Type}) = option (parent * int * $(map status children))

fun childrenG [parentI ::: Type] [parent ::: Type] [children ::: {(Type * Type)}] [t ::: Type]
              (ready : $(map (fn (i, d) => option d) children) -> option t)
              (parent : pattern parentI parent) (children : $(map (fn (i, d) => pattern i d) children)) (fl : folder children)
    : pattern (childrenInternal parentI (map fst children)) (parent * t) =
      {Initial = None,
       EnterTag = fn tinfo state =>
                     case state of
                         None =>
                         (case parent.EnterTag tinfo parent.Initial of
                              None => None
                            | Some pstate => Some (Some (pstate, 1, @map0 [status] (fn [t ::_] => Initial)
                                                                     (@@Folder.mp [fst] [_] fl))))
                       | Some (pstate, depth, cstates) =>
                         if depth = 0 then
                             case parent.EnterTag tinfo parent.Initial of
                                 None => None
                               | Some pstate => Some (Some (pstate, 1, @map0 [status] (fn [t ::_] => Initial)
                                                                        (@@Folder.mp [fst] [_] fl)))
                         else
                             Some (Some (pstate,
                                         depth+1,
                                         @map2 [fn (i, d) => pattern i d] [fn (i, d) => status i] [fn (i, d) => status i]
                                          (fn [p] (ch : pattern p.1 p.2) (cstate : status p.1) =>
                                              case cstate of
                                                  Initial =>
                                                  (case ch.EnterTag tinfo ch.Initial of
                                                       None => Initial
                                                     | Some v =>
                                                       case ch.Finished v of
                                                           None => Pending v
                                                         | _ => Matched v)
                                                | Pending cstate =>
                                                  (case ch.EnterTag tinfo cstate of
                                                       None => Initial
                                                     | Some v =>
                                                       case ch.Finished v of
                                                           None => Pending v
                                                         | _ => Matched v)
                                                | v => v)
                                          fl children cstates)),
       ExitTag = fn state =>
                    case state of
                        None => None
                      | Some (pstate, 1, cstates) => Some (Some (pstate, 0, cstates))
                      | Some (pstate, depth, cstates) =>
                        Some (Some (pstate, depth-1,
                                    @map2 [fn (i, d) => pattern i d] [fn (i, d) => status i] [fn (i, d) => status i]
                                     (fn [p] (ch : pattern p.1 p.2) (cstate : status p.1) =>
                                         case cstate of
                                             Pending cstate =>
                                             (case ch.ExitTag cstate of
                                                  None => Initial
                                                | Some cstate' =>
                                                  case ch.Finished cstate' of
                                                      None => Pending cstate'
                                                    | _ => Matched cstate')
                                           | _ => cstate)
                              fl children cstates)),
       Finished = fn state =>
                     case state of
                         Some (pstate, 0, cstates) =>
                         (case parent.Finished pstate of
                              None => None
                            | Some (pdata, pcont) =>
                              case ready (@map2 [fn (i, d) => status i] [fn (i, d) => pattern i d] [fn (i, d) => option d]
                                           (fn [p] (cstate : status p.1) (ch : pattern p.1 p.2) =>
                                               case cstate of
                                                   Matched v => Option.mp (fn p => p.1) (ch.Finished v)
                                                 | _ => None) fl cstates children) of
                                  None => None
                                | Some cdata => Some ((pdata, cdata), pcont))
                       | _ => None}

fun children [parentI ::: Type] [parent ::: Type] [children ::: {(Type * Type)}]
             (parent : pattern parentI parent) (children : $(map (fn (i, d) => pattern i d) children)) (fl : folder children)
    : pattern (childrenInternal parentI (map fst children)) (parent * $(map snd children)) =
      @childrenG (@foldR [fn (i, d) => option d] [fn cs => option $(map snd cs)]
                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (cstate : option p.2) acc =>
                       case (cstate, acc) of
                           (Some cstate, Some acc) => Some ({nm = cstate} ++ acc)
                         | _ => None)
                   (Some {}) fl) parent children fl

fun childrenO [parentI ::: Type] [parent ::: Type] [children ::: {(Type * Type)}]
             (parent : pattern parentI parent) (children : $(map (fn (i, d) => pattern i d) children)) (fl : folder children)
    : pattern (childrenInternal parentI (map fst children)) (parent * $(map (fn (i, d) => option d) children)) =
      @childrenG Some parent children fl

datatype required t = Required of t | Optional of t

fun childrenO' [parentI ::: Type] [parent ::: Type] [children ::: {(Type * Type)}]
              (parent : pattern parentI parent) (children : $(map (fn (i, d) => required (pattern i d)) children)) (fl : folder children)
    : pattern (childrenInternal parentI (map fst children)) (parent * $(map (fn (i, d) => option d) children)) =
      let
        val os = @mp [fn (i, d) => required (pattern i d)] [fn (i, d) => bool] 
                  (fn [u] pat => case pat of
                                     Required _ => False
                                   | Optional _ => True) fl children
        val vs = @mp [fn (i, d) => required (pattern i d)] [fn (i, d) => pattern i d]
                  (fn [u] pat => case pat of
                                     Required pat' => pat'
                                   | Optional pat' => pat') fl children
      in
          @childrenG (@foldR2 [fn _ => bool] [fn (i, d) => option d] [fn r => option $(map (fn (i, d) => option d) r)]
                       (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (isO : bool) (cstate : option p.2) acc =>
                           case acc of
                               None => None
                             | Some acc =>
                               if isO then
                                   Some ({nm = cstate} ++ acc)
                               else
                                   case cstate of
                                       None => None
                                     | Some _ => Some ({nm = cstate} ++ acc))
                       (Some {}) fl os) parent vs fl
      end
          
con treeInternal (parent :: Type) (child :: Type) = option (parent * int * option child)

fun tree [parentI ::: Type] [parent ::: Type] [childI ::: Type] [child ::: Type]
    (parent : pattern parentI parent) (child : pattern childI child)
    : pattern (treeInternal parentI childI) (parent * child) =
    {Initial = None,
     EnterTag = fn tinfo state =>
                   case state of
                       None =>
                       (case parent.EnterTag tinfo parent.Initial of
                            None => None
                          | Some pstate => Some (Some (pstate, 1, None)))
                     | Some (pstate, depth, cstate) =>
                       Some (Some (pstate,
                                   depth+1,
                                   child.EnterTag tinfo (Option.get child.Initial cstate))),
     ExitTag = fn state =>
                  case state of
                      None => None
                    | Some (_, 1, _) => None
                    | Some (pstate, depth, cstate) =>
                      Some (Some (pstate, depth-1, Option.bind child.ExitTag cstate)),
     Finished = fn state =>
                   case state of
                       None => None
                     | Some (pstate, _, cstate) =>
                       case parent.Finished pstate of
                           None => None
                         | Some (pdata, _) =>
                           case cstate of
                               None => None
                             | Some cstate =>
                               case child.Finished cstate of
                                   None => None
                                 | Some (cdata, _) => Some ((pdata, cdata), True)}

con gatherInternal (parent :: Type) (child :: Type) (data :: Type) = option (parent * bool * int * option child * list data)

fun gather [parentI ::: Type] [parent ::: Type] [childI ::: Type] [child ::: Type]
    (parent : pattern parentI parent) (child : pattern childI child)
    : pattern (gatherInternal parentI childI child) (parent * list child) =
    {Initial = None,
     EnterTag = fn tinfo state =>
                   case state of
                       None =>
                       (case parent.EnterTag tinfo parent.Initial of
                            None => None
                          | Some pstate => Some (Some (pstate, False, 1, None, Nil)))
                     | Some (pstate, return, depth, cstate, clist) =>
                       let
                           val cstate' = child.EnterTag tinfo (Option.get child.Initial cstate)
                       in
                           case child.Finished (Option.get child.Initial cstate') of
                               None =>
                               Some (Some (pstate, return, depth+1, cstate', clist))
                             | Some (cdata, _) =>
                               Some (Some (pstate, return, depth+1, None, cdata :: clist))
                       end,
     ExitTag = fn state =>
                  case state of
                      None => None
                    | Some (pstate, _, 1, cstate, clist) =>
                      Some (Some (pstate, True, 1, cstate, clist))
                    | Some (pstate, return, depth, cstate, clist) =>
                      let
                          val cstate' = child.ExitTag (Option.get child.Initial cstate)
                      in
                          case child.Finished (Option.get child.Initial cstate') of
                              None =>
                              Some (Some (pstate, return, depth-1, cstate', clist))
                            | Some (cdata, _) =>
                              Some (Some (pstate, return, depth-1, None, cdata :: clist))
                      end,
     Finished = fn state =>
                   case state of
                       None => None
                     | Some (pstate, return, _, _, clist) =>
                       case parent.Finished pstate of
                           None => None
                         | Some (pdata, _) =>
                           if return then
                               Some ((pdata, List.rev clist), False)
                           else
                               None}

type document = string
val show_document = _

val fetch = FeedFfi.fetch

fun app' [internal ::: Type] [data ::: Type] [acc ::: Type] (p : pattern internal data) (f : data -> acc -> transaction acc) 
        (doc : document) (acc : acc) : transaction acc =
    let
        fun recur xml acc state =
            case String.seek xml #"<" of
                None => return acc
              | Some xml =>
                if xml <> "" && String.sub xml 0 = #"/" then
                    case String.seek xml #"\x3E" of
                        None => return acc
                      | Some xml =>
                        case p.ExitTag state of
                            None => recur xml acc p.Initial
                          | Some state =>
                            case p.Finished state of
                                 None => recur xml acc state 
                               | Some (data, cont) =>
                                 acc <- f data acc;
                                 recur xml acc (if cont then state else p.Initial)
                else if xml <> "" && String.sub xml 0 = #"?" then
                    case String.seek xml #"\x3E" of
                        None => return acc
                      | Some xml => recur xml acc state
                else if xml <> "" && String.sub xml 0 = #"!" then
                    if String.lengthGe xml 3 && String.sub xml 1 = #"-" && String.sub xml 2 = #"-" then
                        let
                            fun skipper xml =
                                case String.seek xml #"-" of
                                    None => xml
                                  | Some xml =>
                                    if String.lengthGe xml 2 && String.sub xml 0 = #"-" && String.sub xml 1 = #"\x3E" then
                                        String.suffix xml 2
                                    else
                                        skipper xml
                        in
                            recur (skipper (String.suffix xml 3)) acc state
                        end
                    else
                        case String.seek xml #"]" of
                            None => return acc
                          | Some xml =>
                            case String.seek xml #"\x3E" of
                                None => return acc
                              | Some xml => recur xml acc state
                else
                    case String.msplit {Needle = " >/", Haystack = xml} of
                        None => return acc
                      | Some (tagName, ch, xml) =>
                        let
                            fun readAttrs ch xml acc =
                                case ch of
                                    #"\x3E" => (xml, acc, False)
                                  | #"/" =>
                                    (case String.seek xml #"\x3E" of
                                         None => (xml, acc, True)
                                       | Some xml => (xml, acc, True))
                                  | _ =>
                                    if String.lengthGe xml 2 && Char.isSpace (String.sub xml 0) then
                                        readAttrs (String.sub xml 0) (String.suffix xml 1) acc
                                    else if xml <> "" && String.sub xml 0 = #"\x3E" then
                                        (String.suffix xml 1, acc, False)
                                    else if xml <> "" && String.sub xml 0 = #"/" then
                                        (case String.seek xml #"\x3E" of
                                             None => (xml, acc, True)
                                           | Some xml => (xml, acc, True))
                                    else
                                        case String.split xml #"=" of
                                            None => (xml, acc, False)
                                          | Some (aname, xml) =>
                                            if xml = "" || (String.sub xml 0 <> #"\"" && String.sub xml 0 <> #"'") then
                                                (xml, (aname, "") :: acc, False)
                                            else
                                                case String.split (String.suffix xml 1) (String.sub xml 0) of
                                                    None => (xml, (aname, "") :: acc, False)
                                                  | Some (value, xml) =>
                                                    if xml = "" then
                                                        (xml, (aname, value) :: acc, False)
                                                    else
                                                        readAttrs (String.sub xml 0) (String.suffix xml 1) ((aname, value) :: acc)

                            val (xml, attrs, ended) = readAttrs ch xml []

                            fun skipSpaces xml =
                                if xml <> "" && Char.isSpace (String.sub xml 0) then
                                    skipSpaces (String.suffix xml 1)
                                else
                                    xml

                            val xml = skipSpaces xml

                            val (xml, cdata) =
                                if ended then
                                    (xml, None)
                                else if String.isPrefix {Prefix = "<![CDATA[", Full = xml} then
                                    let
                                        fun skipper xml acc =
                                            case String.split xml #"]" of
                                                None => (acc ^ xml, None)
                                              | Some (pre, xml) =>
                                                if String.lengthGe xml 2 && String.sub xml 0 = #"]" && String.sub xml 1 = #"\x3E" then
                                                    (String.suffix xml 2, Some (acc ^ pre))
                                                else
                                                    skipper xml (acc ^ "]" ^ pre)
                                    in
                                        skipper (String.suffix xml 9) ""
                                    end
                                else
                                    case String.split' xml #"<" of
                                        None => (xml, None)
                                      | Some (cdata, xml) => (xml, Some cdata)
                        in
                            case p.EnterTag {Tag = tagName, Attrs = attrs, Cdata = cdata} state of
                                None => recur xml acc p.Initial
                              | Some state =>
                                case p.Finished state of
                                     None =>
                                     (case (if ended then p.ExitTag state else Some state) of
                                          None => recur xml acc p.Initial
                                        | Some state =>
                                          case p.Finished state of
                                              None => recur xml acc state
                                            | Some (data, cont) =>
                                              acc <- f data acc;
                                              recur xml acc (if cont then state else p.Initial))
                                   | Some (data, cont) =>
                                     acc <- f data acc;
                                     state <- return (if ended then
                                                          Option.get p.Initial (p.ExitTag state)
                                                      else
                                                          state);
                                                   
                                     recur xml acc (if cont then state else p.Initial)
                        end
    in
        recur doc acc p.Initial
    end

fun app [internal ::: Type] [data ::: Type] (p : pattern internal data) (f : data -> transaction {}) (doc : document) : transaction {} =
    app' p (fn data acc => f data) doc ()
