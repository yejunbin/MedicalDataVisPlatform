
window.onload=function(){
  var odiv=document.getElementByClassName('main-sidebar');
  odiv.onmouseover=function (){startmove(0,10);}
  odiv.onmouseout=function (){startmove(-150,-10);}
}
var timer=null;
function startmove(target,speed){
  var odiv=document.getElementByClassName('main-sidebar');
  clearInterval(timer);
  timer=setInterval(function (){
    if(odiv.offsetLeft==target){
      clearInterval(timer);
    }
    else{    
      odiv.style.left=odiv.offsetLeft+speed+'px';
    }
  },30)
}
