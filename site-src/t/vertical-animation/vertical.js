function randInt(min, max) {
  return Math.floor( Math.random() * (max - min + 1) ) + min;
}

function randomChunk(str, max) {
    var arr = []
    while (str.length > 0) {
        var i = randInt(1, Math.min(max, str.length));
        arr = arr.concat(str.slice(0, i));
        str = str.slice(i, str.length);
    }
    return arr;
}

var paras;
var kagis = "（「『〈《【〔";
$(document).ready(function(){
    paras = $("#screen > p").toArray();
    $("#screen > p").each(function(){
        if (kagis.includes($(this).text()[0])) {
            $(this).css('text-indent', '0');
        } else {
            $(this).css('text-indent', '1em');
        }
    });
    $("#screen").click(function(){
        if (i = paras.shift()) {
            $(i).fadeIn(500);
        } else {
            for (var i = 0; i < 3; i++) {
                $("#screen").animate({
                    marginLeft: '-=10px'
                }, randInt(25,50)).animate({
                    marginLeft: '+=10px'
                }, randInt(25,50)).animate({
                    marginLeft: '+=10px'
                }, randInt(25,50)).animate({
                    marginLeft: '-=10px'
                }, randInt(25,50));
            }
            var ps = $("#screen > p").filter(function() {
                return $(this).contents().filter(function(i) {
                    return this.nodeType == 3 || ! $(this).hasClass("cleared")
                }).length > 0})
            console.log("ps: " + ps.toArray());
            if (ps.length > 0) {
                var at = randInt(0, ps.length-1);
                var p  = ps.eq(at);
                var cs = p.contents().filter(function(){
                    return this.nodeType == 3 || ! $(this).hasClass("cleared")
                });
                console.log("cs: " + cs.toArray());
                var targ = cs.eq(randInt(0, cs.length-1));
                if (targ.get(0).nodeType == 3) {
                    var src = randomChunk(targ.text(), 5)
                              .map(function(i){return $("<span>").text(i).get(0).outerHTML}).join("");
                    var splitted = $(src);
                    targ.before(splitted);
                    targ.remove();
                    targ = splitted.eq(0);
                    console.log("Current target: " + targ.get(0).outerHTML); 
                    targ = targ.eq(randInt(0, targ.children().length));
                } else {
                    console.log("processing: " + targ.get(0).nodeType);
                }
                var dist = $("#screen").height() - targ.position().top;
                console.log("shifting: " + dist);
                var animated = targ.clone().addClass('animated');
                animated.appendTo($("#screen"));
                animated.offset(targ.offset());
                var dummy = $("<span>").addClass("cleared");
                var w = targ.outerWidth(true);
                var h = targ.outerHeight(true);
                console.log("margin: " + Number(targ.outerWidth(true)) + " x " + Number(targ.outerHeight(true)));
                console.log("inserted: " + dummy.get(0).outerHTML)
                targ.replaceWith(dummy);
                dummy.height(h).width(w).css('position', 'relative');
                animated.animate(
                    {marginTop: '+=' + dist + 'px'},
                    500,
                    'easeOutQuad',
                    function () {
                        $(this).remove();
                    }
                );
            } else {
                console.log("All paras done");
            }
        }
    })
});
