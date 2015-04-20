//(require 'jquery)

/* comments */
var kd_disqus_thread=$('#disqus_thread');
var ds_label=$('.ds-thread');

$('.disqus_label').click(function(){
    kd_disqus_thread.show();
    ds_label.hide();
});

$('.ds-label').click(function(){
    kd_disqus_thread.hide();
    ds_label.show();
});

/* navigate */
var kd_toc = $('#text-table-of-contents ul li');
var kd_n = 2;
var kd_tmp = kd_n/2;
var kd_str = '';
var kd_head = $('div[id*=\'orgheadline\']');
$(window).scroll(function () {
    //kd_str="#orgheadline" + kd_n.toString();
    //var top1=kd_head.find(kd_str).offset().top;
    var top1 = kd_head.eq(kd_n-2).offset().top;
    var top2 = kd_head.eq(kd_n).offset().top;
    if (window.pageYOffset <= (top1-30)) {
        kd_str = '#orgheadline' + kd_tmp.toString();
        kd_toc.children('a[href="' + kd_str + '"]').css('color', '#ffff00');
        if (kd_n > 2) {
            kd_n = kd_n - 2;
            kd_tmp = kd_n/2;
            kd_str = '#orgheadline' + kd_tmp.toString();
            kd_toc.children('a[href="' + kd_str + '"]').css('color', '#22ff22');
        }
        //kd_n = parseInt(kd_str.slice(-1));

    }
    else if (window.pageYOffset >= (top2-30)) {
        kd_str = '#orgheadline' + kd_tmp.toString();
        kd_toc.children('a[href="' + kd_str + '"]').css('color', '#ffff00');
        if (kd_n < (kd_head.length-2)) {
            kd_n = kd_n + 2;
            kd_tmp = kd_n/2;
            kd_str = '#orgheadline' + kd_tmp.toString();
            kd_toc.children('a[href="' + kd_str + '"]').css('color', '#22ff22');
        }
    }
});

/* floating card */
function popupActivate (evt) {
    var boundBox = evt.target.getBoundingClientRect();
    var coordX = boundBox.left;
    var coordY = boundBox.top;
    balloon.style.position="fixed";
    balloon.style.left= (coordX + 30).toString() + "px";
    balloon.style.top= (coordY + 30).toString() + "px";

    if(evt.target.firstChild.parentNode.nextSibling.tagName == "SUP"){
        var footRef = evt.target.nextSibling.childNodes[0].id;
        var docNode = document.getElementById("fn."+footRef.slice(-1));
        var nodeNew = docNode.parentNode.parentNode.lastChild.cloneNode(true);
        balloon.replaceChild(nodeNew,balloon.lastChild);
        balloon.style.visibility="visible";
    }
}

function popupOff(evt) {
    balloon.style.visibility="hidden";
}

function ls_init () {

    // create balloon element, insert as first child of refNode
    function createBalloon (refNode) {
        // create balloon element to display info
        balloon = document.createElement("div");
        balloon.style.visibility="hidden";
        balloon.style.position="fixed";
        balloon.style.top=".5ex";
        balloon.style.left=".5ex";
        balloon.style.padding=".5ex";
        balloon.style.textAlign="left";
        balloon.style.border="solid thin green";
        balloon.style.borderRadius="1ex";
        balloon.style.backgroundColor="hsla(240,80%,50%,0.8)";
        balloon.style.boxShadow="3px 3px 8px black";
        balloon.style.zIndex="341";
        balloon.innerHTML="<p>tips</p>";
        // insert into DOM
        refNode.insertBefore(balloon, refNode.firstChild);
    }

    var myList = document.querySelectorAll(".underline");

    // assign handler to hot hoover elements
    if ( myList.length > 0 ) {
        for (var ii = 0; ii < myList.length; ii++) {
            var myNode = myList[ii];
            myNode.addEventListener("mouseover", popupActivate , false);
            myNode.addEventListener("mouseout", popupOff , false);
        }
    }

    createBalloon(document.body);
}

var balloon;

ls_init();

/* editable */


/* backgroundImage */
