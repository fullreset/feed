fun main () =
    Feed.app (Feed.children
                  (Feed.tag "item" {1 = "rdf:about"})
                  (Feed.tag "title" {}, Feed.tag "content:encoded" {}))
             (fn ({Attrs = {1 = about}, ...},
                  ({Cdata = title, ...}, {Cdata = content, ...})) =>
                 debug ("URL: " ^ about);
                 (case title of
                      None => return ()
                    | Some title => debug ("Title: " ^ title));
                  case content of
                      None => return ()
                    | Some content => debug ("Content: " ^ content))
             "http://feeds.feedburner.com/marginalrevolution/hCQh";
    return <xml>
      See stdout.
    </xml>
