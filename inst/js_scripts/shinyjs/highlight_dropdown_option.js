shinyjs.highlightOptions = function (params) {
	var optionsAndColor = params.toString().split(';;')
	var dropdownId = optionsAndColor[0]
	var optionsToHighlight = optionsAndColor.slice(
		1,
		optionsAndColor.length - 1
	)
	var color = optionsAndColor[optionsAndColor.length - 1]

	$(document).on('shown.bs.select', function (event) {
		if (
			$(event.target).parent().find('select').data('identifier') ===
			dropdownId
		) {
			var options = $(event.target)
				.parent()
				.find('.dropdown-menu.inner li')
			options.each(function () {
				if (optionsToHighlight.includes($(this).text().trim())) {
					$(this).css('background-color', color)
				} else {
					$(this).css('background-color', '')
				}
			})
		}
	})
	$('.selectpicker[data-identifier=' + dropdownId + ']').selectpicker(
		'refresh'
	)
}
