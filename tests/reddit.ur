fun main () =
    doc <- Feed.fetch "http://www.reddit.com/?limit=30";

    acc <- Feed.app' (Feed.tree (Feed.tagAOR "div" {DataUps = "data-ups", Class = "class"} {Style = "style"})
                                (Feed.tagAOR "a" {Href = "href", Class = "class"} {Style = "style"}))
           (fn (div, a) (count, list) =>
               return (if count >= 30
                          || not (String.isPrefix {Full = div.Class, Prefix = " thing "})
                          || Option.isSome (String.sindex {Haystack = div.Class, Needle = "promoted"})
                          || div.Style = Some "display:none"
                          || a.Class <> "title "
                          || a.Style = Some "display:none" then
                           (count, list)
                       else
                           (count + 1, a.Href :: list)))
           doc (0, []);

    return <xml><body>
      <h1>Top 30 Reddit Links</h1>

      <ol>
        {List.mapX (fn url => <xml><li>{[url]}</li></xml>) (List.rev acc.2)}
      </ol>
    </body></xml>
