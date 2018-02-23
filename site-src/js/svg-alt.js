$(function(){
  if (!Modernizr.svg){
    $('img').each(function() {
      $(this).attr('src', $(this).attr('src').replace(/\.svg/gi,'.png'));
    });
  }
});
