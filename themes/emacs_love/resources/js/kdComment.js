//(require 'jquery)

$('.disqus_label').click(function(){
    $('#disqus_thread').show();
    $('.ds-thread').hide();
});

$('.ds-label').click(function(){
    $('#disqus_thread').hide();
    $('.ds-thread').show();
});
