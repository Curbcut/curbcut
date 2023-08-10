// Use to stop the propagation of lenis (the landing page).
$(document).ready(function () {
	$(document).on('wheel', '.scrollable-div, .dropdown, #shiny-modal-wrapper', function (e) {
		e.stopPropagation()
	})
})
