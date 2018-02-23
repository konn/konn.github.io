$(document).ready(function(){
    var count = 0;
    var sections = $('section').map(function (i) {
        var ident = $(this).attr('id');
        var title = $(this).children(':header').text();
        if (!ident) {
            count += 1;
            ident = "--head-" + count;
            $(this).attr('id', ident);
        }
        return {title: title, ident: ident};
    });
    var list = $('<ul>');
    list.addClass('nav flex-column');
    sections.toArray().slice(0,-1).forEach(function(i) {
        $('<li>').addClass('nav-item').append(
            $('<a>').addClass('nav-link').text(i.title).attr('href', '#'+i.ident)
        ).appendTo(list);
    });
    $('div#article-toc').append(list);
    $('body').scrollspy({ target: '#navbar-example' });
});
