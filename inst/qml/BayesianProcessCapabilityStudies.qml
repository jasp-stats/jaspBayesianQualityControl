// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP.Controls

import "./common" as Common

Form
{
	columns:	 2

	VariablesForm
	{
		id:									variablesFormLongFormat

		AvailableVariablesList
		{
			name:							"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:							"measurementLongFormat"
			title:							qsTr("Measurement")
			id:								measurementLongFormat
			allowedColumns:					["scale"]
			singleVariable:					true
		}

	}


	// Section
	// {
	// 	title: qsTr("Process capability options")

		Group
		{
			title:					qsTr("Type of data distribution")


			RadioButtonGroup
			{
				name: 					"capabilityStudyType"
				id: 					capabilityStudyType

				RadioButton
				{
					name: 				"normalCapabilityAnalysis"
					id : 				normalCapabilityAnalysis
					label: 				qsTr("Normal distribution")
					checked: 			true
				}

				RadioButton
				{
					name: 				"tCapabilityAnalysis"
					id : 				tCapabilityAnalysis
					label: 				qsTr("Student's t-distribution")
					// checked: 			true
				}

			}
		}

		Group
		{
			columns: 2
			title: qsTr("Metrics")
			info: qsTr("Select the process capability metrics to report.")
			CheckBox { name: "Cp";   label: qsTr("Cp");  checked: true }
			CheckBox { name: "Cpu";	 label: qsTr("Cpu"); checked: true }
			CheckBox { name: "Cpl";	 label: qsTr("Cpl"); checked: true }
			CheckBox { name: "Cpk";	 label: qsTr("Cpk"); checked: true }
			CheckBox { name: "Cpc";	 label: qsTr("Cpc"); checked: true }
			CheckBox { name: "Cpm";	 label: qsTr("Cpm"); checked: true }
		}

		Group
		{
			title: 							qsTr("Capability Study")

			CheckBox
			{
				name: 						"lowerSpecificationLimit"
				label: 						qsTr("Lower specification limit")
				id:							lowerSpecificationLimit
				childrenOnSameRow:			true
				enableChildrenOnChecked:	false

				DoubleField
				{
					name: 					"lowerSpecificationLimitValue"
					id:						lowerSpecificationLimitValue
					negativeValues:			true
					defaultValue:			-1
					decimals:				9
				}

			}

			CheckBox
			{
				name: 						"target"
				label: 						qsTr("Target value")
				id:							target
				childrenOnSameRow:			true
				enableChildrenOnChecked:	false

				DoubleField
				{
					name: 					"targetValue"
					id:						targetValue
					negativeValues:			true
					defaultValue:			0
					decimals:				9
				}
			}

			CheckBox
			{
				name: 						"upperSpecificationLimit"
				label: 						qsTr("Upper specification limit")
				id:							upperSpecificationLimit
				childrenOnSameRow:			true
				enableChildrenOnChecked:	false

				DoubleField
				{
					name: 					"upperSpecificationLimitValue"
					id:						upperSpecificationLimitValue
					negativeValues:			true
					defaultValue:			1
					decimals:				9
				}

			}

			CheckBox
			{
				name: "timeSeriesPlot"
				label: qsTr("Time series plot")
				checked: false
			}
		}

		Group
		{
			title: qsTr("Process Criteria")
			Layout.columnSpan: 2
			Layout.fillWidth: true
			preferredWidth: form.availableWidth

			ComponentsList
			{
				name: "processCriteria"
				id: processCriteria

				preferredWidth: form.availableWidth - jaspTheme.groupContentPadding

				// Rows get their own key: the right bound and label must come from their controls,
				// with "upper" or "label" as the key the row key would replace the control values.
				optionKey: "region"

				addItemManually: true
				minimumItems: 2
				// JASP restores the cached controls of a deleted row when a new row gets the same key,
				// so every added row gets a fresh key
				newItemValue: "region" + Date.now()

				defaultValues:
				[
					{ label: qsTr("Incapable"),    upper: 1.00 },
					{ label: qsTr("Capable"),      upper: 1.33 },
					{ label: qsTr("Satisfactory"), upper: 1.50 },
					{ label: qsTr("Excellent"),    upper: 2.00 },
					{ label: qsTr("Super"),        upper: Infinity }
				]

				headerLabels:
				[
					{
						lower: qsTr("Left bound"),
						label: qsTr("Classification"),
						upper: qsTr("Right bound")
					}
				]

				// Only the right bounds define the regions: each left bound shows the right bound of the row
				// above, the first left bound and the last right bound are open-ended. When a row is deleted
				// JASP neither updates rowIndex nor destroys that row's controls, and a deleted row whose
				// controls still change writes a stray entry into the option. So positions are looked up in
				// the list, and values are only written explicitly to the rows that are still in the list.
				property int rowRevision: 0
				property int thresholdRevision: 0

				function safeRowAt(index)
				{
					if (index < 0 || index >= count)
						return null

					try			{ return rowAt(index) }
					catch (e)	{ return null }
				}

				function rowIndexOf(row)
				{
					for (var i = 0; i < count; ++i)
					{
						if (safeRowAt(i) === row)
							return i
					}

					return -1
				}

				function interiorBoundaries()
				{
					var values = []

					for (var i = 0; i < count - 1; ++i)
					{
						var row = safeRowAt(i)

						if (row)
							values.push(Number(row.upperValue))
					}

					return values
				}

				function overviewThresholdValues()
				{
					return interiorBoundaries().filter(function(value) {
						return isFinite(value)
					}).map(function(value) {
						return {
							label: String(value),
							value: String(value)
						}
					})
				}

				function refreshThresholds()
				{
					var oldIndex = processOverviewThreshold.currentIndex

					thresholdRevision++

					Qt.callLater(function() {
						var newCount = overviewThresholdValues().length

						if (newCount > 0)
							processOverviewThreshold.currentIndex = Math.min(Math.max(oldIndex, 0), newCount - 1)
					})
				}

				// After rows are added or deleted: every right bound except the last must be finite and the
				// last one open-ended. A row that used to be last gets a boundary above its left bound.
				function normalizeBoundaries()
				{
					rowRevision++

					var previous = -Infinity

					for (var i = 0; i < count; ++i)
					{
						var row = safeRowAt(i)

						if (!row)
							continue

						if (i === count - 1)
						{
							if (isFinite(Number(row.upperValue)))
								row.upperValue = Infinity
						}
						else if (!isFinite(Number(row.upperValue)))
						{
							row.upperValue = isFinite(previous) ? previous + 1 : 1
						}

						previous = Number(row.upperValue)
					}

					synchronizeLeftBounds()
					refreshThresholds()
					validateCriteria()
				}

				function synchronizeLeftBounds()
				{
					for (var i = 1; i < count; ++i)
					{
						var row = safeRowAt(i), previous = safeRowAt(i - 1)

						if (row && previous && Number(row.lowerValue) !== Number(previous.upperValue))
							row.lowerValue = previous.upperValue
					}
				}

				// Editing a right bound keeps the boundaries sorted, the labels stay in place.
				function boundaryEdited()
				{
					var values = interiorBoundaries()

					values.sort(function(a, b) {
						return a - b
					})

					for (var i = 0; i < values.length; ++i)
					{
						var row = safeRowAt(i)

						if (row && Number(row.upperValue) !== values[i])
							row.upperValue = values[i]
					}

					synchronizeLeftBounds()
					refreshThresholds()
					validateCriteria()
				}

				// Equal boundaries and empty labels are rejected here; a control error prevents
				// the analysis from running until the criteria are valid again.
				function validateCriteria()
				{
					for (var i = 0; i < count; ++i)
					{
						var row = safeRowAt(i)

						if (!row)
							continue

						var boundaryError = ""
						var previous = safeRowAt(i - 1)
						if (i > 0 && i < count - 1 && previous && !(Number(row.upperValue) > Number(previous.upperValue)))
							boundaryError = qsTr("Each boundary must be larger than the previous one.")

						var labelError = ""
						if (String(row.labelValue).trim() === "")
							labelError = qsTr("Each region needs a classification label.")

						row.showErrors(boundaryError, labelError)
					}
				}

				onCountChanged:
				{
					newItemValue = "region" + Date.now()
					Qt.callLater(normalizeBoundaries)
				}

				Component.onCompleted: Qt.callLater(normalizeBoundaries)

				rowComponent: Row
				{
					id: criterionRow

					property alias lowerValue: lowerBound.value
					property alias upperValue: upperBound.value
					property alias labelValue: labelField.value
					property alias upperField: upperBound
					property alias labelField: labelField

					property int position:
					{
						var revision = processCriteria.rowRevision
						return processCriteria.rowIndexOf(criterionRow)
					}
					property bool isFirstRow: position === 0
					property bool isLastRow:  position === processCriteria.count - 1

					function showErrors(boundaryError, labelError)
					{
						if (boundaryError !== "")
							upperBound.addControlErrorPermanent(boundaryError)
						else
							upperBound.clearControlError()

						if (labelError !== "")
							labelField.addControlErrorPermanent(labelError)
						else
							labelField.clearControlError()
					}

					// Left bound: shows the right bound of the row above (set by the list), hidden but kept
					// for the header alignment in the first row
					DoubleField
					{
						id: lowerBound
						name: "lower"
						defaultValue: -Infinity
						opacity: criterionRow.isFirstRow ? 0 : 1
						enabled: false

						negativeValues: true
						decimals: 9
						fieldWidth: 80
					}

					Label
					{
						text: criterionRow.isFirstRow ? "" : "<"
						width: 10
					}

					TextField
					{
						id: labelField
						name: "label"
						startValue: qsTr("Region %1").arg(rowIndex + 1)
						fieldWidth: 120
						// deferred: the typed text is only stored in value after this handler
						onEditingFinished: Qt.callLater(processCriteria.validateCriteria)
					}

					Label
					{
						text: criterionRow.isLastRow ? "" : "≤"
						width: 10
					}

					// Right bound, hidden for the last row
					DoubleField
					{
						id: upperBound
						name: "upper"

						opacity: criterionRow.isLastRow ? 0 : 1
						enabled: !criterionRow.isLastRow

						defaultValue: Infinity
						negativeValues: true
						decimals: 9
						fieldWidth: 80

						onEditingFinished: Qt.callLater(processCriteria.boundaryEdited)
					}
				}
			}

			CheckBox
			{
				name: "processOverview"
				id: processOverview
				label: qsTr("Process overview")
				info: qsTr("Show a four-panel overview of the process.")

				DropDown
				{
					name: "processOverviewMetric"
					label: qsTr("Capability metric")
					values:
					[
						{ label: "Cp",  value: "Cp"  },
						{ label: "Cpu", value: "Cpu" },
						{ label: "Cpl", value: "Cpl" },
						{ label: "Cpk", value: "Cpk" },
						{ label: "Cpc", value: "Cpc" },
						{ label: "Cpm", value: "Cpm" }
					]
					indexDefaultValue: 3
				}

				DropDown
				{
					name: "processOverviewThreshold"
					id: processOverviewThreshold
					label: qsTr("Threshold")
					indexDefaultValue: 1

					values:
					{
						var revision = processCriteria.thresholdRevision
						return processCriteria.overviewThresholdValues()
					}
				}

				DropDown
				{
					name: "processOverviewReferencePrior"
					label: qsTr("Reference prior")
					info: qsTr("Prior used for the sensitivity analysis. The DCSI and unit information priors are only available for the normal distribution.")
					// qc supports the DCSI and unit information priors only for the normal distribution
					values: capabilityStudyType.value === "normalCapabilityAnalysis"
						? [
							{ label: qsTr("DCSI"),             value: "DCSI" },
							{ label: qsTr("Jeffreys"),         value: "Jeffreys" },
							{ label: qsTr("Unit information"), value: "unit_information" }
						]
						: [
							{ label: qsTr("Jeffreys"),         value: "Jeffreys" }
						]
					indexDefaultValue: 0
				}

				RadioButtonGroup
				{
					name: "processOverviewBinning"
					title: qsTr("Sequential updates")
					info: qsTr("Each update refits the model to all observations up to that point. The first update always uses the first 3 observations and the last update uses all observations.")

					RadioButton
					{
						value: "noBins"
						label: qsTr("No. bins")
						checked: true
						childrenOnSameRow: true
						IntegerField
						{
							name: "processOverviewNumberOfBins"
							defaultValue: 5
							min: 1
						}
					}

					RadioButton
					{
						value: "observationsPerBin"
						label: qsTr("No. observations per bin")
						info: qsTr("Number of observations added between consecutive updates, starting after the first 3 observations.")
						childrenOnSameRow: true
						IntegerField
						{
							name: "processOverviewObservationsPerBin"
							defaultValue: 10
							min: 1
						}
					}

					RadioButton
					{
						value: "perObservation"
						label: qsTr("Per observation")
					}
				}
			}
		}

		// }



	Section
	{
		title: qsTr("Tables")
		CheckBox
		{
			name: "intervalTable"
			label: qsTr("Interval table")
			info: qsTr("Show posterior probabilities for the process criteria defined above.")
		}
		CIField
		{
			name: "credibleIntervalWidth"
			label: qsTr("Credible interval")
			info: qsTr("Width of the credible interval used for the posterior distribution in the Capability table.")
		}
	}

	Section
	{

		title: qsTr("Prior and Posterior Inference")

		Common.PlotLayout {}

		Common.PlotLayout
		{
			baseName: "priorDistributionPlot"
			baseLabel: qsTr("Prior distribution")
			hasPrior: false
		}

	}

	Section
	{
		title: qsTr("Sequential Analysis")

		Common.PlotLayout
		{
			id: sequentialAnalysisPointEstimatePlot
			baseName: "sequentialAnalysisPointEstimatePlot"
			baseLabel: qsTr("Point estimate plot")
			hasPrior: false
		}

		Common.PlotLayout
		{
			id: sequentialAnalysisIntervalEstimatePlot
			baseName: "sequentialAnalysisPointIntervalPlot"
			baseLabel: qsTr("Interval estimate plot")
			hasPrior: false
			hasEstimate: false
			hasCi: false
			hasType: true
		}

		Group
		{
			CheckBox
			{
				enabled:	sequentialAnalysisPointEstimatePlot.checked || sequentialAnalysisIntervalEstimatePlot.checked
				id:			sequentialAnalysisAdditionalInfo
				name:		"sequentialAnalysisPlotAdditionalInfo"
				label:		qsTr("Show process criteria")
				checked:	true
				info:		qsTr("Add a secondary right axis with condition bounds for the process")
			}

			CheckBox
			{
				// TODO:
				enabled:	sequentialAnalysisPointEstimatePlot.checked || sequentialAnalysisIntervalEstimatePlot.checked
				name:		"sequentialAnalysisUpdatingTable"
				label:		qsTr("Posterior updating table")
				checked:	false
				info:		qsTr("Show the data from the sequential analysis in a table. Will show both the information for the point estimate and interval estimate plots, if both are selected.")
			}
		}
	}

	Section
	{

		title: qsTr("Prior and Posterior Predictive Plots")

		Common.PlotLayout
		{
			baseName: "posteriorPredictiveDistributionPlot"
			baseLabel: qsTr("Posterior predictive distribution")
			hasPrior: false
			hasAxes: false
			hasPanels: false
		}

		Common.PlotLayout
		{
			baseName: "priorPredictiveDistributionPlot"
			baseLabel: qsTr("Prior predictive distribution")
			hasPrior: false
			hasAxes: false
			hasPanels: false
		}

	}


	Section
	{
		title: qsTr("Prior Distributions")

		// TODO: this dropdown should just show the same GUI as the custom one
		// but disable e.g., the DropDown itself and instead show the prior
		// also disable all truncation for non-custom ones
		// NOTE: the above is done, but default values cannot be set yet.

		DropDown
		{
			id: priorSettings
			name: "priorSettings"
			label: qsTr("Prior distributions")
			values:
			[
				{label: qsTr("Default"),					value: "default"},
				{label: qsTr("Informed conjugate"),			value: "conjugate"},
				// {label: qsTr("Informed conjugate"),			value: "weaklyInformativeConjugate"},
				{label: qsTr("Informed uniform"),			value: "weaklyInformativeUniform"},
				{label: qsTr("Custom informative"),			value: "customInformative"},
			]
		}

		Common.Priors
		{

			// visible: priorSettings.currentValue === "customInformative"
			priorType: capabilityStudyType.value === "normalCapabilityAnalysis" ? "normalModel" : "tModel"

			hasTruncation: priorSettings.currentValue === "customInformative"
			hasParameters: priorSettings.currentValue !== "default"
			visible:       priorSettings.currentValue !== "default"

			dropDownValuesMap: {
				switch (priorSettings.currentValue) {
					case "default":
						return {
							"mean": 	[{ label: qsTr("Jeffreys"),				value: "jeffreys"}],
							"sigma": 	[{ label: qsTr("Jeffreys"),				value: "jeffreys"}],
							"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
						}
					case "conjugate":
						return {
							"mean": 	[{ label: qsTr("Normal(μ,σ)"),			value: "normal"}],
							"sigma": 	[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }],
							"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
						};
					// case "weaklyInformativeConjugate":
					// 	return {
					// 		"mean": 	[{ label: qsTr("Normal(μ,σ)"),			value: "normal"}],
					// 		"sigma": 	[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }],
					// 		"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
					// 	}
					case "weaklyInformativeUniform":
						return {
							"mean": 	[{ label: qsTr("Uniform(a,b)"),			value: "uniform"}],
							"sigma": 	[{ label: qsTr("Uniform(a,b)"),			value: "uniform"}],
							"df": 		[{ label: qsTr("Gamma(α,β)"),			value: "gammaAB" }]
						}
					case "customInformative":
						return undefined;
				}
			}
		}
	}

	Section
	{
		title: qsTr("Advanced Options")

		Group
		{
			title: qsTr("MCMC Settings")
			info: qsTr("Adjust the Markov Chain Monte Carlo (MCMC) settings for estimating the posterior distribution.")
			IntegerField
			{
				name: "noIterations"
				label: qsTr("No. iterations")
				defaultValue: 5000
				min: 100
				max: 100000000
				info: qsTr("Number of MCMC iterations used for estimating the posterior distribution.")
			}
			IntegerField
			{
				name: "noWarmup"
				label: qsTr("No. warmup samples")
				defaultValue: 1000
				min: 0
				max: 100000000
				info: qsTr("Number of initial MCMC samples to discard.")
			}
			IntegerField
			{
				name: "noChains"
				label: qsTr("No. chains")
				defaultValue: 1
				min: 1
				max: 128
				info: qsTr("Number of MCMC chains to run.")
			}
		}
	}
}
