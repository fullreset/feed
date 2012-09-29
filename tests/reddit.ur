fun main () =
    doc <- Feed.fetch "http://www.reddit.com/?limit=30";

    acc <- Feed.app' (Feed.tree (Feed.tagAOR "div" {DataUps = "data-ups", Class = "class"} {Style = "style"})
                                (Feed.tagAO "a" {Class = "class", Style = "style", Href = "href"}))
           (fn (div, a) (count, list) =>
               return (case a.Href of
                           None => (count, list)
                         | Some link =>
                           if count >= 30
                              || not (String.isPrefix {Full = div.Class, Prefix = " thing "})
                              || Option.isSome (String.sindex {Haystack = div.Class, Needle = "promoted"})
                              || div.Style = Some "display:none"
                              || a.Class <> Some "title "
                              || a.Style = Some "display:none" then
                               (count, list)
                           else
                               (count + 1, link :: list)))
           doc (0, []);

    return <xml><body>
      <h1>Top 30 Reddit Links</h1>

      <ol>
        {List.mapX (fn url => <xml><li>{[url]}</li></xml>) (List.rev acc.2)}
      </ol>
    </body></xml>
