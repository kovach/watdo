var t = function(t, cl) {
  var div = d(cl ? cl : "text");
  div.appendChild(document.createTextNode(t));
  return div;
}

var d = function(cl) {
  var div = document.createElement("div");
  set(div, "tabindex", 0);
  if (cl)
    set(div, "class", cl);
  return div;
}

var set = function(a, b, c) {
  a.setAttribute(b, c);
}

var mk = function(x) {
  document.body.appendChild(x);
}

var ul = function() {
  return document.createElement("ul");
}

var li = function(x) {
  var elem = document.createElement("li");
  child(elem, x);
  return elem;
}

var ta = function() {
  return document.createElement("textarea");
}

var child = function(a, b) {
  a.appendChild(b);
}

var get = function(a, b) {
  return a.getAttribute(b);
}

var addH = function(div) {
  var name = get(div, "name");
  div.onclick = function(ev) {
    sock.send("click " + name);
  };

  div.addEventListener('keypress', function(ev) {
    var c = ev.keyCode;
    console.log(ev.keyCode);
    switch (c) {
      case 100:
        // do delete
        console.log('D!');
        break;
      case 13:
        // make child?
        console.log('RETURN!');
        var o = ta();

        o.addEventListener('keypress', function(ev) {
          ev.stopPropagation();
          if (ev.keyCode == 13) {
            var val = o.value;
            console.log("msg received: ", val);
            div.removeChild(o);
            ev.stopPropagation();

            // make thing
            var thing = {};
            thing.msg = val;
            thing.to = name;
            sock.send(JSON.stringify(thing));
          }
        });
        div.appendChild(o);
        setTimeout(function() {
          o.focus()
        }, 0);
        o.value = "";
        break;
    }
  });
}

var renderThing = function(thing) {
  var list = ul();
  var div = d("head");
  child(list, li(div));
  child(div, t(thing.head));
  for (var i = 0; i < thing.tags.length; i++) {
    child(div, t(thing.tags[i], "tag"));
  };
  set(div, "name", thing.name);

  for (var i = 0; i < thing.children.length; i++) {
    child(list, li(renderThing(thing.children[i])));
  };

  addH(div);

  return list;
}
