//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls


Group
{
	property string baseName
	property bool multiplePanels: true
	property bool allowNegativeY: false

	RadioButtonGroup
	{
		name:		baseName + "Axes"
		title:		qsTr("Axes")
		info:		qsTr("Axis settings used for both plots in this section.")
		id:			axes

		RadioButton
		{
			value: "free"
			label: qsTr("Automatic")
			checked: true
		}
		RadioButton
		{
			value: "fixed"
			label: qsTr("Identical across panels")
			enabled: multiplePanels
		}
		RadioButton
		{
			value: "custom"
			label: qsTr("Custom axes")
		}
	}

	Group
	{

		title: qsTr("Custom axes")
		enabled: axes.value === "custom"
		visible: axes.value === "custom"

		GridLayout
		{
			columns: 5
			columnSpacing: 2
			rowSpacing: jaspTheme.rowGridSpacing / 3
			id: customAxesLayout
			property int dbWidth: 50

			// Row 0: Headers
			Label
			{
				text: qsTr("Axis")
			}
			Item { }
			Label
			{
				text: qsTr("Min")
			}
			Item { }
			Label
			{
				text: qsTr("Max")
			}

			// Row 1: x axis
			Label
			{
				text: qsTr("x axis")
			}
			Item { }
			DoubleField
			{
				name: baseName + "CustomAxesXMin"
				id: customAxesXMin
				fieldWidth: customAxesLayout.dbWidth
				defaultValue: 0.00
				negativeValues: true
				max: customAxesXMax.value
			}
			Item { }
			DoubleField
			{
				name: baseName + "CustomAxesXMax"
				id: customAxesXMax
				fieldWidth: customAxesLayout.dbWidth
				defaultValue: 1.00
				negativeValues: true
				min: customAxesXMin.value
			}

			// Row 2: y axis
			Label
			{
				text: qsTr("y axis")
			}
			Item { }
			DoubleField
			{
				name: baseName + "CustomAxesYMin"
				id: customAxesYMin
				fieldWidth: customAxesLayout.dbWidth
				defaultValue: 0.00
				negativeValues: allowNegativeY
				max: customAxesYMax.value
			}
			Item { }
			DoubleField
			{
				name: baseName + "CustomAxesYMax"
				id: customAxesYMax
				fieldWidth: customAxesLayout.dbWidth
				defaultValue: 1.00
				negativeValues: allowNegativeY
				min: customAxesYMin.value
			}
		}
	}
}
