fun main () =
    doc <- Feed.fetch "http://marginalrevolution.com/feed";

    Feed.app (Feed.children
                  (Feed.tagA "item" ())
                  (Feed.tagC "link", Feed.tagC "title", Feed.tagC "content:encoded"))
             (fn ((), (link, title, content)) =>
                 debug ("URL: " ^ link);
                 debug ("Title: " ^ title);
                 debug ("Content: " ^ content))
             doc;
    return <xml>
      See stdout.
    </xml>
