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
	id: root
	property	string	baseName: "priorAndPosteriorInferencePosteriorDistribution"
	property	string	baseLabel: qsTr("Posterior distribution")
	property bool hasMode: true
	property bool pointEstimateChecked: false
	property	bool	hasPrior: 		true
	property	bool	hasEstimate: 	true
	property	bool	hasCi: 			true
	property	bool	hasInterval: 		false

	readonly property alias checked:	mainCheckBox.checked

	CheckBox
	{
		id:							mainCheckBox
		name: 						baseName
		label: 						baseLabel
		info: qsTr("Display the distribution or sequential estimates, with the selected annotations.")

		// Keep plot-specific options together below the plot checkbox
		Group
		{

			columns: 					1

			// Group so point estimate and CI options are shown in a single column
			Group
			{
				enabled:			hasEstimate || hasCi
				visible:			hasEstimate || hasCi

				CheckBox
				{
					enabled:			hasEstimate
					visible:			hasEstimate
					label:				qsTr("Point estimate")
					info: qsTr("Show the selected point estimate on the plot.")
					name:				baseName + "PointEstimate"
					checked: root.pointEstimateChecked
					childrenOnSameRow:	true

					DropDown
					{
						name:	baseName + "PointEstimateType"
						label:	""
						values: root.hasMode
							? [{label: qsTr("mean"), value: "mean"}, {label: qsTr("median"), value: "median"}, {label: qsTr("mode"), value: "mode"}]
							: [{label: qsTr("mean"), value: "mean"}, {label: qsTr("median"), value: "median"}]
					}
				}

				// Group so CI checkbox and options are shown in a single column (with subgroup so CI options are indented)
				Group
				{
					enabled:			hasCi
					visible:			hasCi

					columns: 1
					CheckBox
					{
						name:				baseName + "Ci"
						label:				qsTr("CI")
						info: qsTr("Show a central, highest posterior density (HPD), or custom interval.")
						id:					posteriorPlotIndividualCI
						childrenOnSameRow:	true

						DropDown
						{
							name:		baseName + "CiType"
							label:		""
							id:			posteriorPlotIndividualType
							values:		[
								{label: qsTr("central"),				value: "central"},
								{label: qsTr("HPD"),					value: "HPD"},
								{label: qsTr("custom"),					value: "custom"}
							]
						}
					}

					Group
					{
						columns: 2
						indent: true
						enabled:		posteriorPlotIndividualCI.checked

						CIField
						{
							visible:		posteriorPlotIndividualType.currentValue === "central" || posteriorPlotIndividualType.currentValue === "HPD"
							name:			baseName + "CiMass"
							label:			qsTr("Mass")
							info: qsTr("Probability contained in the interval.")
							fieldWidth:		50
						}

						DoubleField
						{
							visible:		posteriorPlotIndividualType.currentValue === "custom"
							name:			baseName + "CiLower"
							label:			qsTr("Lower")
							id:				plotsPosteriorLower
							max: plotsPosteriorUpper.value
							fieldWidth:		50
							defaultValue:	0
							negativeValues: true
							inclusive:		JASP.MinMax
						}

						DoubleField
						{
							visible:		posteriorPlotIndividualType.currentValue === "custom"
							name:			baseName + "CiUpper"
							label:			qsTr("Upper")
							id:				plotsPosteriorUpper
							min: plotsPosteriorLower.value
							fieldWidth:		50
							defaultValue:	1
							negativeValues: true
							inclusive:		JASP.MinMax
						}

					}
				}
			}

			Group
			{
				enabled:			hasInterval
				visible:			hasInterval

				title:		qsTr("Interval")
				info: qsTr("Calculate the probability that each capability metric lies between these bounds.")

				columns: 2
				FormulaField
				{
					name:				baseName + "IntervalLower"
					label:				qsTr("Lower")
					id:					intervalLower
					fieldWidth:			50
					defaultValue:		0.0
					max:				intervalUpper.realValue
				}

				FormulaField
				{
					name:				baseName + "IntervalUpper"
					label:				qsTr("Upper")
					id:					intervalUpper
					fieldWidth:			50
					defaultValue:		1.0
					min:				intervalLower.realValue

				}
			}

			CheckBox
			{
				enabled:	hasPrior
				visible:	hasPrior
				name:		baseName + "ShowPriorDistribution"
				label:		qsTr("Show prior distribution")
				info: qsTr("Overlay the prior distribution for comparison with the posterior distribution.")
				checked:	false
			}
		}
	}
}
