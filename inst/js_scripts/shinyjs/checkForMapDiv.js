shinyjs.checkForMapDiv = function (id) {
  var map_ph = `${id}-${id}-map_ph`;
  var input_name = `${id}-${id}-mapboxDivExists`;
  var parentDiv = document.getElementById(map_ph);

  if (parentDiv) {
    var childDivs = parentDiv.querySelectorAll(".map-container");
    var found = false;

    for (let i = 0; i < childDivs.length; i++) {
      if (childDivs[i].classList.contains('mapboxgl-map')) {
        found = true;
        break;
      }
    }

    Shiny.setInputValue(input_name, found);
  } else {
    Shiny.setInputValue(input_name, false);
  }
}
