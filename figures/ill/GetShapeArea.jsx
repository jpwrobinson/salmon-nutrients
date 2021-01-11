/* Save this file with a jsx extension and place in your
Illustrator/Presets/en_US/Scripts folder. You can then
access it from the File > Scripts menu */

var decimalPlaces = 3;

if (app.documents.length > 0) {

	if (app.activeDocument.selection.length < 1) {
		alert('Select a path');
	} else if (app.activeDocument.selection[0].area) {
		// Individual Items
		var objects =  app.activeDocument.selection;
	} else if (app.activeDocument.selection[0].pathItems) {
		// Group/Compound Shape
		var objects = app.activeDocument.selection[0].pathItems;
	} else {
		alert('Please select a path or group.');
	}

	// Collect info
	var totalArea = 0;
	for (var i=0; i<objects.length; i++) {
		if (objects[i].area) {
			var totalArea = totalArea + objects[i].area;
		}
	}

	// Conversions
	var ppi = 72;
	var areaIn = totalArea / ppi / ppi;
	if (areaIn < 0) var areaIn = -areaIn;
	var areaCm = areaIn * 6.4516;

	// Display
	alert('Shape Area\
	' + areaIn.toFixed(decimalPlaces) + ' in² \
	' + areaCm.toFixed(decimalPlaces) + ' cm² \n\
	' + i + ' shapes');

}
