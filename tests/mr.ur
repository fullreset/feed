fun main () =
    Feed.app (Feed.children
                  (Feed.tagA "item" {1 = "rdf:about"})
                  (Feed.tagC "title", Feed.tagC "content:encoded"))
             (fn (item, props) =>
                 debug ("URL: " ^ item.1);
                 debug ("Title: " ^ props.1);
                 debug ("Content: " ^ props.2))
             "http://feeds.feedburner.com/marginalrevolution/hCQh";
    return <xml>
      See stdout.
    </xml>
