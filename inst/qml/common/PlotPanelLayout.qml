import QtQuick
import JASP.Controls

RadioButtonGroup
{
	property string baseName
	name: baseName + "Layout"
	title: qsTr("Layout")
	info: qsTr("Layout used for both plots in this section.")

	RadioButton
	{
		value: "multiplePanels"
		label: qsTr("One plot per metric")
		checked: true
	}
	RadioButton
	{
		value: "singlePanel"
		label: qsTr("All metrics in one plot")
	}
}
