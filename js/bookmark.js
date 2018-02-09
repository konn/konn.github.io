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
        if (Number(data) > 0) {
            var li = $("<li>");
            var msg = "";
            msg += data;
            msg += " user(s) including ";
            var link = $("<a>").text("HTTP version").attr('href', "http://b.hatena.ne.jp/entry/http:" + urlBody);
            $('<span>').attr("id", "old-bookmark-numbers").text(msg).append(link).appendTo(li);
            li.insertAfter("#social #hatena-bookmarks")
       }
    });
});
