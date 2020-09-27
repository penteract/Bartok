
function allOf(l,f){
    for (c of l) if (!f(c)) return false;
    return true;
}

function joincards(cs){//TODO: debug
    if(cs.length==0)return "0"
    topc = cs[0];
    rest=cs.slice(1);
    if (rest.length==0) return showCard(topc)
    if (! allOf(cs,c=>c.tag=="CardBack")) return showCard(topc)+" "+joincards(rest)
    return (showCard(topc)+cs.length)
}

function displayHand(h){
    s = "<div class=hand>"
    s+="<span class=name>" + h[0] + "</span>"
    s+="<p class=cards>"+joincards(h[1])+"</p>"
    s+="</div>"
    return s
}
function displayOwnHand(h){
    s = "<div class=hand>"
    s+="<span class=name>" + h[0]+ "</span><br />"
    s+="<p class=cards>" + h[1].map(tagCard).join(" ") + "</p>"
    s+="</div>"
    return s
}

function tagCard(c,i){
    //let x = showCard(c)
    //console.log(c)
    sh = showCard(c)
    c = c.contents
    return "<a onclick=sendC("+i+")>"+sh+"</a>"
}

function sendC(card){
    lastMoveIdx = card
    sendMove("ReqPlay",card)
}

function draw(){
    n=$("#drawN").val() - 0
    sendMove("ReqDraw",n)
    $("#drawN").val(1)
}

function sendMove(type,dat){
    $.post("",JSON.stringify({
        tag:type,
        contents:[name,tok,count,dat,$("#mmsg").val()]
    }),display(count),"json")
    $("#mmsg").val("")
}

function sendLeave(){
    $.post("",JSON.stringify({
        tag:"ReqLeave",
        contents:[name,tok,count]
    }),display(count),"json")
    clearInterval(poller)
    $("#mmsg").val("you have left")
}


function display(n){
    return function(obj){
    if (obj.tag=="NewData" && count==n){
        count = obj.contents[0]
        obj = obj.contents[1]
        if(obj._messagesV.length) $("#msg").prepend(obj._messagesV.join("<br />")+"<br />")
        displayCards(obj, function() { lastm = obj })
    }
    else if (obj.tag=="Redirect") location.assign(obj.contents)
}}

function displayCards(obj, callback) {
  // TODO: maybe animate penalties too? 
  for (m of obj._messagesV) {
    var matches = m.match(/^(.*) draws (\d*) cards?\.?$/)
    if (matches) {
      var name = matches[1]
      var amt = Number(matches[2])
      if (amt > 0) {
        var hand = handOf(name)
        if (hand.length) { // checks whether the hand exists

          var contents = showCard({tag:"CardBack"})
          var from = $("#deck").offset()
          var to = hand.children(".cards").offset()
          if (amt > 1) { contents += amt }

          displayDeckAndPile(obj)
          animate(contents, from, to, function(){
            displayHands(obj)
            callback()
          })
          return
        }
      }
    }

    var matches = m.match(/^(.*) plays (..)\.?$/) 
    if (matches) {
      var name = matches[1]
      var card = readCard(matches[2])
      if (card) {
        var hand = handOf(name)
        if (hand.length) {
 
          var contents = showCard(card)
          var from = hand.children(".cards")
          if (name === window.name) from = from.children().eq(lastMoveIdx)
          from = from.offset()
          var to = $("#pile").offset()

          displayHands(obj)
          animate(contents, from, to, function(){
            displayDeckAndPile(obj)
            callback()
          })
          return
        }
      }
    }
  }

  // if we couldn't find any message explaining what's going on we just display stuff normally
  displayDeckAndPile(obj)
  displayHands(obj)
  callback()
}

function handOf(name) {
  return $(".hand").filter(function(i, h) { return $(h).children(".name").html() == name }).first()
}

function animate(contents, from, to, callback){
  //console.log("animating" + contents)

  var elt = $("#animation")
  elt.html(contents)
  elt.css({top: from.top, left: from.left})
  elt.show()
  elt.animate({top: to.top, left: to.left}, 250, function() { 
      elt.hide()
      elt.html("")
      callback() 
  })
}

function displayDeckAndPile(obj){
    $("#deck").html(joincards(obj._deckV))
    $("#pile").html(joincards(obj._pileV))
}

function displayHands(obj){
    var first=true
    $("#others").html("")
    for (h of obj._handsV){
        if(first){
          if (!window.lastm || h != lastm._handsV[0])
            $("#self").html(displayOwnHand(h))
          name = h[0]
          first=false
        }
        else{
          $("#others").append(displayHand(h))
        }
    }
  
}

function submitRule(){
    data=JSON.stringify({
        "imports":$('#imports :checked').map((_,y)=>y.value).toArray(),
        "ruleType":$('#submissiontypes :checked').val(),
        "code":editor.getValue()
    })
    console.log(data)
    $.post("", data, process, "json")
}

function showCard(c){
    if (c.tag=="CardBack") return String.fromCodePoint(0x1f0a0)
    else if (c.tag=="CardFace") return "<span class="+c.contents[1]+">"+String.fromCodePoint(
        0x1f0a0 + suitNums[c.contents[1]]*16 + rankNums[c.contents[0]])+"</span>"
    else console.log("unrecognised card", c)
}

function readCard(c){
  var cp = c.codePointAt(0)
  cp -= 0x1f0a0
  if (cp === 0) return {tag:"CardBack"}
  var rank = cp % 16
  var suit = cp >> 4
  if (0 <= suit && suit <= 3 && 1 <= rank && rank <= 14){
     return {tag:"CardFace", contents:[ranks[rank], suits[suit]]}
  }
  return null
}

rankNums = {"Ace": 1,
"Two": 2,
"Three": 3,
"Four": 4,
"Five": 5,
"Six": 6,
"Seven": 7,
"Eight": 8,
"Nine": 9,
"Ten": 10,
"Jack": 11,
"Knight": 12,
"Queen": 13,
"King":14}

suitNums = {
    "Spades" : 0,
    "Hearts" : 1,
    "Diamonds" : 2,
    "Clubs" : 3}

ranks = [null, "Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Knight", "Queen", "King"]

suits = ["Spades", "Hearts", "Diamonds", "Clubs"]

//https://stackoverflow.com/a/901144/1779797
function getParameterByName(name, url) {
    if (!url) url = window.location.href;
    var name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}


function randstr(n) {
  var text = "";
  for (var i = 0; i < n; i++)
    text += String.fromCharCode(Math.floor(Math.random()*(127-32)+32));
  return text;
}

function allowCookies(){
  localStorage.setItem("allowCookies", "true")
}

function clearData(){
  localStorage.clear() //Also unsets allowCookies
}
