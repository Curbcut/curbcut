// Open and close menus. When the menu is clicked, add the `open` class to the panels.
$(document).ready(function () {
	$('.mobile-sidebar-menu').on('click', function () {
		$('.sus-map-sidebar').toggleClass('open')
	})
})

$(document).ready(function () {
	$('.mobile-panel-menu').on('click', function () {
		$('.sus-map-panel').toggleClass('open')
	})
})
