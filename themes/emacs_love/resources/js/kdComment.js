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
var kd_n = 1;
var kd_tmp = kd_n;
var kd_head = $('div[id*=\'text-orgheadline\']');
var topArray = [];
while(kd_n <= kd_head.length){
    topArray.push(kd_head.eq(kd_n-1).offset().top);
    kd_n++;
}
$(window).scroll(function () {
    //kd_str="#orgheadline" + kd_n.toString();
    //var top1=kd_head.find(kd_str).offset().top;
    var startPoint=0;
    var endPoint=topArray.length-1;
    var offsetValue=window.pageYOffset+60;
    if(topArray[kd_tmp]>offsetValue || offsetValue>topArray[kd_tmp+1]){
        while((startPoint+1) < endPoint){
            if(topArray[Math.floor((startPoint+endPoint)/2)] > offsetValue){
                endPoint = Math.floor((startPoint+endPoint)/2);
            }
            else if(topArray[Math.floor((startPoint+endPoint)/2)] < offsetValue){
                startPoint = Math.floor((startPoint+endPoint)/2);
            }
            else{
                break;
            }
        }
        if(offsetValue>topArray[topArray.length-1]){
            kd_n=topArray.length-1;
        }
        else if(offsetValue>topArray[topArray.length-2]){
            kd_n=topArray.length-2;
        }
        else{
            kd_n = startPoint;
        }

        kd_toc.eq(kd_tmp).children('a').css('color', '#ffff00');
        kd_tmp = kd_n;
        kd_toc.eq(kd_tmp).children('a').css('color', '#22ff22');
        //kd_n = parseInt(kd_str.slice(-1));
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
