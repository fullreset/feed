fun main () =
    doc <- Feed.fetch "http://www.reddit.com/?limit=30";

    ls <- Feed.app' (Feed.tree (Feed.tagAOR "div" {DataUps = "data-ups", Class = "class"} {Style = "style"})
                               (Feed.tagAOR "a" {Href = "href", Class = "class"} {Style = "style"}))
                    (fn (div, a) ls =>
                        return (if not (String.isPrefix {Full = div.Class, Prefix = " thing "})
                                   || Option.isSome (String.sindex {Haystack = div.Class, Needle = "promoted"})
                                   || div.Style = Some "display:none"
                                   || a.Class <> "title "
                                   || a.Style = Some "display:none" then
                                    ls
                                else
                                    a.Href :: ls))
           doc [];

    return <xml><body>
      <h1>Top 30 Reddit Links</h1>

      <ol>
        {List.mapX (fn url => <xml><li>{[url]}</li></xml>) (List.rev ls)}
      </ol>
    </body></xml>
