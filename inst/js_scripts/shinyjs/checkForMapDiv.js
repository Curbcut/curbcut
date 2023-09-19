shinyjs.checkForMapDiv = function (id) {
  var map_ph = `${id}-${id}-map_ph`;
  var input_name = `${id}-${id}-mapboxDivExists`;
  var parentDiv = document.getElementById(map_ph);

  if (parentDiv) {
    var childDivs = parentDiv.querySelectorAll(".map-container.mapboxgl-map");
    console.log(childDivs);

    if (childDivs.length > 0) {
      Shiny.setInputValue(input_name, childDivs[0].classList.contains("mapboxgl-map"));
  } else {
    Shiny.setInputValue(input_name, false);
  }
  } else {
    Shiny.setInputValue(input_name, false);
  }
}
