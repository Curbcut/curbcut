// Global variable to store the picker open status
var isPickerOpen = false;

// Function that hides or show the right-panel-hidden divs depending on window size + if a dropdown in .sus-map-panel is opened
$(document).ready(function() {
function updateRightPanelVisibility() {
  var windowWidth = $(window).width();
  var rightPanelHiddenDivs = $(".right-panel-hidden");
  var susMapPanel = $(".sus-map-panel");
  var hrElements = susMapPanel.find("hr");

  // Add hover effect
  susMapPanel.on("mouseenter", function () {
    hrElements.css({
      "margin-top": "20px",
      "margin-bottom": "20px",
    });
  });

  susMapPanel.on("mouseleave", function () {
    if (rightPanelHiddenDivs.is(":visible")) {
      hrElements.css("margin", "");
    } else {
      hrElements.css("margin", ".7rem 0 .7rem 0");
    }
  });

  // Update panel visibility
  if (windowWidth < 1400) {
    if (isPickerOpen) {
      rightPanelHiddenDivs.show();
      hrElements.css("margin", "");
    } else {
      rightPanelHiddenDivs.hide();
      hrElements.css("margin", ".7rem 0 .7rem 0");
    }
  } else {
    rightPanelHiddenDivs.show();
    hrElements.css("margin", "");
  }
}
  // Run the update function initially
  updateRightPanelVisibility();

  // Update the visibility when the window is resized
  $(window).on("resize", function() {
    updateRightPanelVisibility();
  });

  // Update the global variable when the pickerInput is opened
  $(document).on("show.bs.dropdown", ".sus-map-panel .bootstrap-select", function(e) {
    isPickerOpen = true;
    $(document).trigger("pickerOpenStatusChanged");
  });

  // Update the global variable when the pickerInput is closed
  $(document).on("hide.bs.dropdown", ".sus-map-panel .bootstrap-select", function(e) {
    isPickerOpen = false;
    $(document).trigger("pickerOpenStatusChanged");
  });

  // Listen for the custom event when isPickerOpen changes
  $(document).on("pickerOpenStatusChanged", function() {
    updateRightPanelVisibility();
  });
});