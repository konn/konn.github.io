$(document).ready(function(){
    var urlBody = location.href.replace(location.protocol, "");
    var protos =  ["http", "https"];
    var query = "?";
    for (i = 0; i < 2 ; i+=1) {
        query += `url=${encodeURIComponent(protos[i] + ":" + urlBody)};`
    }
    var url = "https://b.hatena.ne.jp/entry.count" + query;
    $.ajax({
        url: url,
        type: "GET",
        crossDomain: true,
        dataType: 'jsonp'
    }).done(function(data) {
        count = Number(data);
        if (count > 0) {
            var li = $("<li>");
            var span = $('<span>').attr("id", "old-bookmark-numbers");
            var msg = "";
            span.append(data + " user");
            if (count > 1) {
                span.append("s");
            }
            span.append(" (with ");
            var link = $("<a>").text("HTTP ver.")
                               .attr('href', "http://b.hatena.ne.jp/entry/http:" + urlBody);
            span.append(link);
            span.append(")");
            $("<li>").append(span).insertAfter("#social #hatena-bookmarks");
       }
    });
});
