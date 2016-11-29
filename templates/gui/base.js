
function hover(evt, params) {
  for(k in params) {
    evt.target.setAttributeNS(null, k, params[k]);
  }
  return true;
}

function hout(evt, params) {
  evt.target.setAttributeNS(null, "opacity", "1");
}
