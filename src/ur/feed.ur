task initialize = fn () => FeedFfi.init

datatype pattern internal output =
         Transducer of {Initial : internal,
                        EnterTag : {Tag : string, Attrs : list (string * string), Cdata : option string} -> internal -> option internal,
                        ExitTag : internal -> option internal,
                        Finished : internal -> option output}

con tagInternal (attrs :: {Unit}) = option {Attrs : $(mapU string attrs), Cdata : option string}

fun tag [attrs ::: {Unit}] (fl : folder attrs) (name : string) (attrs : $(mapU string attrs))
    : pattern (tagInternal attrs) {Attrs : $(mapU string attrs), Cdata : option string} =
    Transducer {Initial = None,
                EnterTag = fn tinfo state =>
                              case state of
                                  Some _ => None
                                | None =>
                                  if tinfo.Tag <> name then
                                      None
                                  else
                                      case @foldUR [string] [fn r => option $(mapU string r)]
                                            (fn [nm ::_] [r ::_] [[nm] ~ r] aname ro =>
                                                case ro of
                                                    None => None
                                                  | Some r =>
                                                    case List.assoc aname tinfo.Attrs of
                                                        None => None
                                                      | Some v => Some ({nm = v} ++ r))
                                            (Some {}) fl attrs of
                                          None => None
                                        | Some vs => Some (Some {Attrs = vs, Cdata = tinfo.Cdata}),
                ExitTag = Some,
                Finished = fn x => x}

datatype status a = Initial | Failed | Matched of a

con childrenInternal (parent :: Type) (children :: {Type}) = option (parent * int * $(map status children))

fun children [parentI ::: Type] [parent ::: Type] [children ::: {(Type * Type)}]
             ((Transducer parent) : pattern parentI parent) (children : $(map (fn (i, d) => pattern i d) children)) (fl : folder children)
    : pattern (childrenInternal parentI (map fst children)) (parent * $(map snd children)) =
    Transducer {Initial = None,
                EnterTag = fn tinfo state =>
                              case state of
                                  None =>
                                  (case parent.EnterTag tinfo parent.Initial of
                                       None => None
                                     | Some pstate => Some (Some (pstate, 1, @map0 [status] (fn [t ::_] => Initial)
                                                                              (@@Folder.mp [fst] [_] fl))))
                                | Some (pstate, depth, cstates) =>
                                  Some (Some (pstate,
                                              depth+1,
                                              @map2 [fn (i, d) => pattern i d] [fn (i, d) => status i] [fn (i, d) => status i]
                                               (fn [p] ((Transducer ch) : pattern p.1 p.2) (cstate : status p.1) =>
                                                   case cstate of
                                                       Failed => Failed
                                                     | Initial =>
                                                       (case ch.EnterTag tinfo ch.Initial of
                                                            None => Failed
                                                          | Some v => Matched v)
                                                     | v => v)
                                               fl children cstates)),
                ExitTag = fn state =>
                             case state of
                                 None => None
                               | Some (pstate, depth, cstates) =>
                                 case (if depth = 1 then
                                           parent.ExitTag pstate
                                       else
                                           Some pstate) of
                                     None => None
                                   | Some pstate =>
                                     if depth = 1 then
                                         Some (Some (pstate, 0, cstates))
                                     else
                                         case @foldR2 [fn (i, d) => pattern i d] [fn (i, d) => status i]
                                               [fn cs => option $(map (fn (i, d) => status i) cs)]
                                               (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] ((Transducer ch) : pattern p.1 p.2) (cstate : status p.1) acc =>
                                                   case acc of
                                                       None => None
                                                     | Some acc =>
                                                       case cstate of
                                                           Matched cstate =>
                                                           (case ch.ExitTag cstate of
                                                                None => None
                                                              | Some cstate' => Some ({nm = Matched cstate'} ++ acc))
                                                         | _ => Some ({nm = Initial} ++ acc))
                                               (Some {}) fl children cstates of
                                             None => None
                                           | Some cstates =>
                                             Some (Some (pstate, depth-1, cstates)),
                Finished = fn state =>
                              case state of
                                  Some (pstate, 0, cstates) =>
                                  (case parent.Finished pstate of
                                       None => None
                                     | Some pdata =>
                                       case @foldR2 [fn (i, d) => pattern i d] [fn (i, d) => status i] [fn cs => option $(map snd cs)]
                                             (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] ((Transducer ch) : pattern p.1 p.2) (cstate : status p.1) acc =>
                                                 case acc of
                                                     None => None
                                                   | Some acc =>
                                                     case cstate of
                                                         Initial => None
                                                       | Failed => None
                                                       | Matched cstate =>
                                                         case ch.Finished cstate of
                                                             None => None
                                                           | Some cdata => Some ({nm = cdata} ++ acc))
                                             (Some {}) fl children cstates of
                                           None => None
                                         | Some cdata => Some (pdata, cdata))
                                | _ => None}

fun app [internal ::: Type] [data ::: Type] ((Transducer p) : pattern internal data) (f : data -> transaction {}) (url : string) : transaction {} =
    let
        fun recur xml state =
            case String.split xml #"<" of
                None => return ()
              | Some (_, xml) =>
                if xml <> "" && String.sub xml 0 = #"/" then
                    case String.split xml #"\x3E" of
                        None => return ()
                      | Some (_, xml) =>
                        case p.ExitTag state of
                            None => recur xml p.Initial
                          | Some state =>
                            case p.Finished state of
                                 None => recur xml state
                               | Some data =>
                                 f data;
                                 recur xml p.Initial
                else if xml <> "" && String.sub xml 0 = #"?" then
                    case String.split xml #"\x3E" of
                        None => return ()
                      | Some (_, xml) => recur xml state
                else if xml <> "" && String.sub xml 0 = #"!" then
                    if String.lengthGe xml 3 && String.sub xml 1 = #"-" && String.sub xml 2 = #"-" then
                        let
                            fun skipper xml =
                                case String.split xml #"-" of
                                    None => xml
                                  | Some (_, xml) =>
                                    if String.lengthGe xml 2 && String.sub xml 0 = #"-" && String.sub xml 1 = #"\x3E" then
                                        String.suffix xml 2
                                    else
                                        skipper xml
                        in
                            recur (skipper (String.suffix xml 3)) state
                        end
                    else
                        case String.split xml #"]" of
                            None => return ()
                          | Some (_, xml) =>
                            case String.split xml #"\x3E" of
                                None => return ()
                              | Some (_, xml) => recur xml state
                else
                    case String.msplit {Needle = " >/", Haystack = xml} of
                        None => return ()
                      | Some (tagName, ch, xml) =>
                        let
                            fun readAttrs ch xml acc =
                                case ch of
                                    #"\x3E" => (xml, acc, False)
                                  | #"/" =>
                                    (case String.split xml #"\x3E" of
                                         None => (xml, acc, True)
                                       | Some (_, xml) => (xml, acc, True))
                                  | _ =>
                                    if String.lengthGe xml 2 && Char.isSpace (String.sub xml 0) then
                                        readAttrs (String.sub xml 0) (String.suffix xml 1) acc
                                    else if xml <> "" && String.sub xml 0 = #"\x3E" then
                                        (String.suffix xml 1, acc, False)
                                    else if xml <> "" && String.sub xml 0 = #"/" then
                                        (case String.split xml #"\x3E" of
                                             None => (xml, acc, True)
                                           | Some (_, xml) => (xml, acc, True))
                                    else
                                        case String.split xml #"=" of
                                            None => (xml, acc, False)
                                          | Some (aname, xml) =>
                                            if xml = "" || String.sub xml 0 <> #"\"" then
                                                (xml, (aname, "") :: acc, False)
                                            else
                                                case String.split (String.suffix xml 1) #"\"" of
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
                                    case String.split xml #"<" of
                                        None => (xml, None)
                                      | Some (cdata, xml) => ("<" ^ xml, Some cdata)
                        in
                            case p.EnterTag {Tag = tagName, Attrs = attrs, Cdata = cdata} state of
                                None => recur xml p.Initial
                              | Some state =>
                                case (if ended then p.ExitTag state else Some state) of
                                    None => recur xml p.Initial
                                  | Some state =>
                                    case p.Finished state of
                                         None => recur xml state
                                       | Some data =>
                                         f data;
                                         recur xml p.Initial
                        end
    in
        xml <- FeedFfi.fetch url;
        recur xml p.Initial
    end
