// can we just not ask about this?
// it makes me sad.
// The issue here is that dash loads
// all it's components as the last part
// of the dom render, probably something
// to do with React more than dash.
// That said, we need the dom to be fully
// loaded to attach js handlers to the tabs
// so they hide content appropriately.
// .
// .
// which brings us to this little hack
// which is wait an extra second after the dom
// has "fully loaded" and then attach
// the tab handlers.
//
// #sadpanda
$(window).bind("load", function() {
  setTimeout(function() {
    $('.menu.tabular .item').tab();
  }, 1000);
})
