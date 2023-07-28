// Theme colors attribution used for the dataTable. Depending on data-theme attribute of the parent div,
// the hover and active color changes.
$(document).ready(function () {
	$('div[data-theme]').each(function () {
		var themeColor = getComputedStyle(this)
			.getPropertyValue('--theme-color')
			.trim()
		$(this).find('.dataTable').css('--dt-row-selected', themeColor)
	})
})
