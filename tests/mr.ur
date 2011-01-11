fun main () =
    txt <- Feed.fetch "http://feeds.feedburner.com/marginalrevolution/hCQh";
    return <xml>
      {[txt]}
    </xml>
