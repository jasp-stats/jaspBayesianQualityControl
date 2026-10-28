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
import JASP.Controls
import JASP

ColumnLayout
{
	id: priorLayout
	Layout.fillWidth: true
	spacing: 						0
	property string priorType:		"normalModel"
	property bool hasTruncation:	false
	property bool hasParameters:	true

	property var nameMap: {
		"mean": 	qsTr("Mean"),
		"sigma":	qsTr("Sigma"),
		"df": 		"df - 2"
	}
	property var defaultDistributionMap: {
		"mean": 	"normal",
		"sigma": 	"invgamma",
		"df": 	 	"invgamma"
	}
	property var truncationLowerMap: {
		"mean": 	-Infinity,
		"sigma": 	0,
		"df": 	 	0
	}
	property var allPriors : [
		{ label: qsTr("Normal(μ,σ)"),			value: "normal"},
		{ label: qsTr("Student-t(μ,σ,v)"),		value: "t"},
		{ label: qsTr("Cauchy(x₀,θ)"),			value: "cauchy"},
		{ label: qsTr("Jeffreys"),				value: "jeffreys"},
		{ label: qsTr("Gamma(α,β)"),			value: "gammaAB"},
		{ label: qsTr("Gamma(k,θ)"),			value: "gammaK0"},
		{ label: qsTr("Inverse-Gamma(α,β)"),	value: "invgamma"},
		{ label: qsTr("Log-Normal(μ,σ)"),		value: "lognormal"},
		{ label: qsTr("Beta(α,β)"),				value: "beta"},
		{ label: qsTr("Uniform(a,b)"),			value: "uniform"}
	]
	property var priorTruncationMap: {
		"normal" : 		[-Infinity, Infinity],
		"t"      : 		[-Infinity, Infinity],
		"cauchy" : 		[-Infinity, Infinity],
		"jeffreys": 	[-Infinity, Infinity],
		"gammaAB": 		[0, 		Infinity],
		"gammaK0": 		[0, 		Infinity],
		"invgamma": 	[0, 		Infinity],
		"lognormal": 	[0, 		Infinity],
		"beta": 		[0, 		1		],
		"uniform": 		[-Infinity, Infinity]
	}
	property var defaultDropDownValuesMap: {
		"mean": 	allPriors,
		"sigma": 	allPriors,
		"df": 		allPriors.filter(p => p.value !== "jeffreys")
	}
	property var dropDownValuesMap: undefined
	property var activeDropDownValuesMap: dropDownValuesMap !== undefined ? dropDownValuesMap : defaultDropDownValuesMap
	property var currentComponentValues: {
		switch (priorType) {
			case "normalModel":
					return [ "mean", "sigma" ];
			case "tModel":
				return [ "mean", "sigma", "df" ];
		}
	}


	property double width1: 70 * preferencesModel.uiScale;
	property double width2: 140 * preferencesModel.uiScale;
	property double width3: 155 * preferencesModel.uiScale;
	property double width4: 160 * preferencesModel.uiScale;


	RowLayout
	{
		Layout.preferredWidth: priorList.availableWidth
		Layout.leftMargin: jaspTheme.contentMargin
		Label { text: qsTr("Parameter"); 	Layout.preferredWidth: width1 }
		Label { text: qsTr("Distribution"); Layout.preferredWidth: width2 }
		Label { text: qsTr("Parameters");	Layout.preferredWidth: width3 ; Layout.fillWidth: true; visible: hasParameters }
		Label { text: qsTr("Truncation"); 	Layout.preferredWidth: width4 ; visible: hasTruncation }
	}


	ComponentsList
	{
		id: priorList
		Layout.fillWidth: true
		Layout.minimumHeight: Math.ceil(implicitHeight)
		preferredWidth: priorLayout.width
		name:					"priorDistributionsCustomIndependent" + (priorType === "normalModel" ? "Normal" : "StudentT")
		optionKey:				"parameter"
		info: qsTr("Sigma priors are restricted to positive values. Uniform lower bounds must be at least zero, and upper bounds must exceed the lower bound.")
			+ (priorType === "tModel" ? "\n\n" + qsTr("The prior is placed on df - 2. The Student-t degrees of freedom equal this parameter plus 2, so df is greater than 2.") : "")

		addItemManually:		false

		values:					currentComponentValues

		rowComponent: 			RowLayout
		{
			width: Math.max(implicitWidth, priorList.availableWidth)
			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width1
				Label 	{ text: nameMap[rowValue] }
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width2

				DropDown
				{
					visible:			activeDropDownValuesMap[rowValue].length > 1
					id: 				typeItem
					name: 				"distribution"
					useExternalBorder: 	true
					startValue: 		defaultDistributionMap[rowValue]
					values:				activeDropDownValuesMap[rowValue]
				}

				Label
				{
					visible: activeDropDownValuesMap[rowValue].length === 1
					text: 	 activeDropDownValuesMap[rowValue][0].label
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width3
				Layout.fillWidth:		true
				visible:				hasParameters

				FormulaField
				{
					label:				"μ"
					name:				"mu"
					visible:			typeItem.currentValue === "normal"		||
										typeItem.currentValue === "lognormal"	||
										typeItem.currentValue === "t"
					defaultValue:		"0"
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true

				}
				FormulaField
				{
					label:				"x₀"
					name:				"x0"
					visible:			typeItem.currentValue === "cauchy"	||
										typeItem.currentValue === "spike"
					defaultValue:		"0"
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"σ"
					name:				"sigma"
					id:					sigma
					visible:			typeItem.currentValue === "normal"		||
										typeItem.currentValue === "lognormal"	||
										typeItem.currentValue === "t"
					defaultValue:		"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"k "
					name:				"k"
					visible:			typeItem.currentValue === "gammaK0"
					defaultValue:		"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
				}
				FormulaField
				{
					label:				"θ"
					name:				"theta"
					visible:			typeItem.currentValue === "cauchy"	||
										typeItem.currentValue === "gammaK0"
					defaultValue:		"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"ν"
					name:				"nu"
					visible:			typeItem.currentValue === "t"
					defaultValue:		"2"
					min:				1
					inclusive:			JASP.MinOnly
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"α "
					name:				"alpha"
					visible:			typeItem.currentValue === "gammaAB"	 ||
										typeItem.currentValue === "invgamma" ||
										typeItem.currentValue === "beta"
					defaultValue:		"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"β"
					name:				"beta"
					visible:			typeItem.currentValue === "gammaAB"	 ||
										typeItem.currentValue === "invgamma" ||
										typeItem.currentValue === "beta"
					defaultValue:		"0.15"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"a "
					name:				"a"
					id:					a
					min: truncationLowerMap[rowValue]
					visible:			typeItem.currentValue === "uniform"
					defaultValue:		"0"
					max:				b.realValue
					inclusive:			JASP.MinOnly
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"b"
					name:				"b"
					id:					b
					visible:			typeItem.currentValue === "uniform"
					defaultValue:		"1"
					min:				Math.max(a.realValue, truncationLowerMap[rowValue])
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width4

				FormulaField
				{
					id:					truncationLower
					label: 				qsTr("lower")
					name: 				"truncationLower"
					visible:			hasTruncation && typeItem.currentValue !== "spike" && typeItem.currentValue !== "uniform" && typeItem.currentValue !== "jeffreys"
					defaultValue:		Math.max((priorTruncationMap[typeItem.currentValue] || [-Infinity, Infinity])[0], truncationLowerMap[rowValue])
					min:				Math.max((priorTruncationMap[typeItem.currentValue] || [-Infinity, Infinity])[0], truncationLowerMap[rowValue])
					max: 				truncationUpper.realValue
					inclusive: 			JASP.MinOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
				FormulaField
				{
					id:					truncationUpper
					label: 				qsTr("upper")
					name: 				"truncationUpper"
					visible:			hasTruncation && typeItem.currentValue !== "spike" && typeItem.currentValue !== "uniform" && typeItem.currentValue !== "jeffreys"
					defaultValue:		(priorTruncationMap[typeItem.currentValue] || [-Infinity, Infinity])[1]
					max:				(priorTruncationMap[typeItem.currentValue] || [-Infinity, Infinity])[1]
					min: 				Math.max(truncationLower.realValue, truncationLowerMap[rowValue])
					inclusive: 			JASP.MaxOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
			}
		}
	}

}
