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
import JASP
import JASP.Controls

import "./common" as Common

Form
{
	columns:	 2

	VariablesForm
	{
		preferredHeight: jaspTheme.defaultVariablesFormHeight / 2
		id:									availableVariables

		AvailableVariablesList
		{
			name:							"availableVariables"
		}

		AssignedVariablesList
		{
			name:							"measurement"
			title:							qsTr("Measurement")
			id:								measurement
			allowedColumns:					["scale"]
			singleVariable:					true
		}

	}


	Group
	{
		Layout.alignment: Qt.AlignTop

		Group
		{
			title: qsTr("Capability Specification")

			FormulaField
			{
				name: "capabilitySpecificationLowerLimit"
				label: qsTr("Lower limit")
				defaultValue: ""
				inputType: "string"
				parseDefaultValue: false
				info: qsTr("Lower acceptable process value. Leave empty until the specification is known.")
			}

			FormulaField
			{
				name: "capabilitySpecificationTargetValue"
				label: qsTr("Target value")
				defaultValue: ""
				inputType: "string"
				parseDefaultValue: false
				info: qsTr("Desired process value. Leave empty until the target is known.")
			}

			FormulaField
			{
				name: "capabilitySpecificationUpperLimit"
				label: qsTr("Upper limit")
				defaultValue: ""
				inputType: "string"
				parseDefaultValue: false
				info: qsTr("Upper acceptable process value. Leave empty until the specification is known.")
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
		Layout.alignment: Qt.AlignTop
		Layout.leftMargin: jaspTheme.contentMargin

		Group
		{
			DropDown
			{
				name: "dataDistribution"
				id: dataDistribution
				label: qsTr("Data distribution")
				info: qsTr("Distribution of the process measurements.")
				values: [
					{ label: qsTr("Normal"),	value: "normal" },
					{ label: qsTr("Student-t"), value: "studentT" }
				]
				onCurrentValueChanged:
				{
					if (currentValue !== "normal")
						priorDistributionsCustomSpecification.currentIndex = 0
					if (currentValue === "studentT" && priorDistribution.currentValue === "unitInformation")
					priorDistribution.currentIndex = 0
				}
			}

			DropDown
			{
				name: "priorDistribution"
				id: priorDistribution
				label: qsTr("Prior distribution")
				info: qsTr("Use the package default, a unit information prior, Jeffreys priors, or custom priors. The default is DCSI for numerical integration and Jeffreys for MCMC. Unit information is available with numerical integration.")
				values: [
					{ label: advancedEstimationMethod.value === "integration" ? qsTr("Default (DCSI)") : qsTr("Default (Jeffreys)"), value: "default" },
					{ label: qsTr("Unit information"),		value: "unitInformation" },
					{ label: qsTr("Jeffreys"),				value: "jeffreys" },
					{ label: qsTr("Custom"), value: "custom" }
				]
				enabledOptions: [true, advancedEstimationMethod.value === "integration", true, true]
			}

			DoubleField
			{
				name: "priorDistributionUnitInformationValue"
				visible: priorDistribution.currentValue === "unitInformation"
				label: qsTr("Units of information")
				defaultValue: 1
				min: 0
				inclusive: JASP.None
				decimals: 3
				info: qsTr("Information weight for the unit information prior. One reproduces the unit information prior; larger values strengthen it and values between zero and one weaken it.")
			}
		}

		Group
		{
			title: qsTr("Process Criteria")

			ComponentsList
			{
				name: "processCriteria"
				id: processCriteria
				preferredWidth: implicitWidth

				addBorder: false

				// Rows get their own key: the right bound and label must come from their controls,
				// with "rightBound" or "classification" as the key the row key would replace the control values.
				optionKey: "region"

				addItemManually: true
				minimumItems: 2
				// JASP restores the cached controls of a deleted row when a new row gets the same key,
				// so every added row gets a fresh key
				newItemValue: "region" + Date.now()

				defaultValues:
				[
					{ classification: qsTr("Incapable"),    rightBound: 1.00 },
					{ classification: qsTr("Capable"),      rightBound: 1.33 },
					{ classification: qsTr("Satisfactory"), rightBound: 1.50 },
					{ classification: qsTr("Excellent"),    rightBound: 2.00 },
					{ classification: qsTr("Super"),        rightBound: Infinity }
				]

				headerLabels:
				[
					{
						leftBound: qsTr("Left bound"),
						classification: qsTr("Classification"),
						rightBound: qsTr("Right bound")
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
					var oldIndex = statisticsProcessOverviewThreshold.currentIndex

					thresholdRevision++

					Qt.callLater(function() {
						var newCount = overviewThresholdValues().length

						if (newCount > 0)
							statisticsProcessOverviewThreshold.currentIndex = Math.min(Math.max(oldIndex, 0), newCount - 1)
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
							boundaryError = qsTr("Each Right bound must be larger than the previous one.")

						var labelError = ""
						if (String(row.labelValue).trim() === "")
							labelError = qsTr("Each region needs a Classification.")

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
						name: "leftBound"
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
						name: "classification"
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
						name: "rightBound"

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
		}
	}

	Section
	{
		title:	qsTr("Statistics")


		Group
		{

			Group
			{
				columns: 2
				title: qsTr("Metrics")
				info: qsTr("Select the process capability metrics to report.")
				CheckBox { name: "statisticsMetricsCp";   label: qsTr("Cp");  checked: false }
				CheckBox { name: "statisticsMetricsCpu";	 label: qsTr("Cpu"); checked: false }
				CheckBox { name: "statisticsMetricsCpl";	 label: qsTr("Cpl"); checked: false }
				CheckBox { name: "statisticsMetricsCpk";	 label: qsTr("Cpk"); checked: true }
				CheckBox { name: "statisticsMetricsCpc";	 label: qsTr("Cpc"); checked: false }
				CheckBox { name: "statisticsMetricsCpm";	 label: qsTr("Cpm"); checked: false }
			}

			CheckBox
			{
				name: "statisticsIntervalTable"
				label: qsTr("Interval table")
				info: qsTr("Show posterior probabilities for the process criteria defined above.")
			}

			CheckBox
			{
				name: "statisticsParameterEstimates"
				label: qsTr("Parameter estimates")
				enabled: advancedEstimationMethod.value === "mcmc" || priorPredictive.checked || posteriorPredictive.checked
				info: qsTr("Show parameter estimates, credible intervals, effective sample sizes, R-hat, and sampler diagnostics for MCMC, including additional predictive MCMC fits.")
			}

			CIField
			{
				name: "statisticsCredibleInterval"
				label: qsTr("Credible interval")
				info: qsTr("Width of the credible intervals in the Capability Table and Parameter estimates table.")
			}
		}

		CheckBox
		{
			name: "statisticsProcessOverview"
			id: statisticsProcessOverview
			label: qsTr("Process overview")
			info: qsTr("Show a four-panel overview of the process.")

			DropDown
			{
				name: "statisticsProcessOverviewCapabilityMetric"
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
				name: "statisticsProcessOverviewThreshold"
				id: statisticsProcessOverviewThreshold
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
				name: "statisticsProcessOverviewSensitivityAnalysisPrior"
				label: qsTr("Sensitivity analysis prior")
				info: qsTr("Prior used for the sensitivity analysis. The DCSI and unit information priors are only available with numerical integration.")
				values: advancedEstimationMethod.value === "integration"
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
				name: "statisticsProcessOverviewSequentialUpdates"
				title: qsTr("Sequential updates")
				info: qsTr("Each update refits the model to all observations up to that point. With one bin, there is one update using all observations. With multiple bins or observations added per bin, updates start with the first 3 observations and always include all observations in the final update.")

				RadioButton
				{
					value: "bins"
					label: qsTr("Bins")
					checked: true
					childrenOnSameRow: true
					IntegerField
					{
						name: "statisticsProcessOverviewSequentialUpdatesBinsValue"
						defaultValue: 5
						min: 1
					}
				}

				RadioButton
				{
					value: "observationsPerBin"
					label: qsTr("Observations per bin")
					info: qsTr("Number of observations added between consecutive updates, starting after the first 3 observations.")
					childrenOnSameRow: true
					IntegerField
					{
						name: "statisticsProcessOverviewSequentialUpdatesObservationsPerBinValue"
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


	Section
	{

		title: qsTr("Prior and Posterior Inference")

		Common.PlotLayout
		{
			id: priorAndPosteriorInferencePriorDistribution
			baseName: "priorAndPosteriorInferencePriorDistribution"
			baseLabel: qsTr("Prior distribution")
			hasPrior: false
		}

		Common.PlotLayout
		{
			id: priorAndPosteriorInferencePosteriorDistribution
		}

		Common.PlotPanelLayout
		{
			id: inferenceLayout
			baseName: "priorAndPosteriorInference"
			enabled: priorAndPosteriorInferencePosteriorDistribution.checked || priorAndPosteriorInferencePriorDistribution.checked
		}

		Common.PlotAxes
		{
			baseName: "priorAndPosteriorInference"
			enabled: priorAndPosteriorInferencePosteriorDistribution.checked || priorAndPosteriorInferencePriorDistribution.checked
			multiplePanels: inferenceLayout.value === "multiplePanels"
		}

		Group
		{
			CheckBox
			{
				enabled:	priorAndPosteriorInferencePosteriorDistribution.checked || priorAndPosteriorInferencePriorDistribution.checked
				name:		"priorAndPosteriorInferenceShowProcessCriteria"
				label:		qsTr("Show process criteria")
				checked:	true
				info:		qsTr("Shade the plot background by the process criteria regions defined above.")
			}
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
			hasMode: false
			pointEstimateChecked: true
		}

		Common.PlotLayout
		{
			id: sequentialAnalysisIntervalEstimatePlot
			baseName: "sequentialAnalysisIntervalEstimatePlot"
			baseLabel: qsTr("Interval estimate plot")
			hasPrior: false
			hasEstimate: false
			hasCi: false
			hasInterval: true
		}

		Common.PlotPanelLayout
		{
			id: sequentialLayout
			baseName: "sequentialAnalysis"
			enabled: sequentialAnalysisPointEstimatePlot.checked || sequentialAnalysisIntervalEstimatePlot.checked
		}

		Common.PlotAxes
		{
			baseName: "sequentialAnalysis"
			allowNegativeY: true
			enabled: sequentialAnalysisPointEstimatePlot.checked || sequentialAnalysisIntervalEstimatePlot.checked
			multiplePanels: sequentialLayout.value === "multiplePanels"
		}

		Group
		{
			CheckBox
			{
					enabled:	sequentialAnalysisPointEstimatePlot.checked
				id:			sequentialAnalysisAdditionalInfo
				name:		"sequentialAnalysisShowProcessCriteria"
				label:		qsTr("Show process criteria")
				checked:	true
				info:		qsTr("Label the secondary right axis of the Point estimate plot with the Process Criteria classifications and boundaries.")
			}

		}
	}

	Section
	{

		title: qsTr("Prior and Posterior Predictive Plots")

		Common.PlotLayout
		{
			baseName: "priorAndPosteriorPredictivePlotsPriorPredictiveDistribution"
			id: priorPredictive
			baseLabel: qsTr("Prior predictive distribution")
			hasPrior: false
		}

		Common.PlotLayout
		{
			baseName: "priorAndPosteriorPredictivePlotsPosteriorPredictiveDistribution"
			id: posteriorPredictive
			baseLabel: qsTr("Posterior predictive distribution")
			hasPrior: false
		}
	}


	Section
	{
		title: qsTr("Prior Distributions (Custom)")
		columns: 1
		enabled: priorDistribution.currentValue === "custom"
		onEnabledChanged: expanded = enabled

		DropDown
		{
			name: "priorDistributionsCustomSpecification"
			id: priorDistributionsCustomSpecification
			label: qsTr("Specification")
			values: [
				{ label: qsTr("Independent"), value: "independent" },
				{ label: qsTr("Dependent"), value: "dependent" }
			]
			enabledOptions: [true, dataDistribution.currentValue === "normal" && advancedEstimationMethod.value === "integration"]
			info: qsTr("Independent priors specify a separate distribution for each parameter. The dependent Normal–Inverse-Gamma prior specifies the mean conditional on the process variance. It is available for normal data with numerical integration.")
		}

		Common.Priors
		{
			id: independentPriors
			visible: priorDistributionsCustomSpecification.currentValue === "independent"
			width1: Math.max(70 * preferencesModel.uiScale, dependentMeanLabel.implicitWidth)
			priorType: dataDistribution.currentValue === "normal" ? "normalModel" : "tModel"
			hasTruncation: true
		}

		ColumnLayout
		{
			id: dependentPriors
			visible: priorDistributionsCustomSpecification.currentValue === "dependent"
			Layout.fillWidth: true
			spacing: 0
			property real parameterWidth: independentPriors.width1 + 12 * preferencesModel.uiScale

			RowLayout
			{
				Layout.fillWidth: true
				Layout.leftMargin: jaspTheme.contentMargin
				Layout.rightMargin: jaspTheme.contentMargin
				Label { text: qsTr("Parameter"); Layout.preferredWidth: dependentPriors.parameterWidth; Layout.minimumWidth: dependentPriors.parameterWidth }
				Label { text: qsTr("Distribution"); Layout.preferredWidth: independentPriors.width2 }
				Label { text: qsTr("Parameters"); Layout.preferredWidth: independentPriors.width3; Layout.fillWidth: true }
			}

			Rectangle
			{
				Layout.fillWidth: true
				implicitHeight: dependentPriorRows.implicitHeight + 2 * jaspTheme.contentMargin
				implicitWidth: dependentPriorRows.implicitWidth + 2 * jaspTheme.contentMargin
				color: jaspTheme.analysisBackgroundColor
				border.color: jaspTheme.borderColor
				radius: jaspTheme.borderRadius

				GridLayout
				{
					id: dependentPriorRows
					anchors.fill: parent
					anchors.margins: jaspTheme.contentMargin
					columns: 3
					rowSpacing: 1
					columnSpacing: 5

					Label
					{
						id: dependentMeanLabel
						text: qsTr("Mean | Sigma")
						Layout.preferredWidth: dependentPriors.parameterWidth
						Layout.minimumWidth: dependentPriors.parameterWidth
					}
					Label { text: qsTr("Normal(μ,σ/√κ)"); Layout.preferredWidth: independentPriors.width2 }
					Row
					{
						Layout.preferredWidth: independentPriors.width3
						Layout.fillWidth: true
						spacing: 4 * preferencesModel.uiScale
						DoubleField
						{
							name: "priorDistributionsCustomDependentMeanMu"
							label: "μ"
							defaultValue: 0
							negativeValues: true
							fieldWidth: 40 * preferencesModel.uiScale
							useExternalBorder: false
							showBorder: true
							info: qsTr("The conditional mean prior is Normal with location μ and variance σ² / κ. Its variance increases with the process variance.")
						}
						DoubleField
						{
							name: "priorDistributionsCustomDependentMeanKappa"
							label: "κ"
							defaultValue: 1
							min: 0
							inclusive: JASP.None
							fieldWidth: 40 * preferencesModel.uiScale
							useExternalBorder: false
							showBorder: true
						}
					}

					Label { text: qsTr("Sigma²"); Layout.preferredWidth: dependentPriors.parameterWidth; Layout.minimumWidth: dependentPriors.parameterWidth }
					Label { text: qsTr("Inverse-Gamma(α,β)"); Layout.preferredWidth: independentPriors.width2 }
					Row
					{
						Layout.preferredWidth: independentPriors.width3
						Layout.fillWidth: true
						spacing: 4 * preferencesModel.uiScale
						DoubleField
						{
							name: "priorDistributionsCustomDependentSigmaSquaredAlpha"
							label: "α"
							defaultValue: 1
							min: 0
							inclusive: JASP.None
							fieldWidth: 40 * preferencesModel.uiScale
							useExternalBorder: false
							showBorder: true
							info: qsTr("σ² ~ Inverse-Gamma(α, β), with density proportional to (σ²)^(−α−1) exp(−β/σ²).")
						}
						DoubleField
						{
							name: "priorDistributionsCustomDependentSigmaSquaredBeta"
							label: "β"
							defaultValue: 0.15
							min: 0
							inclusive: JASP.None
							fieldWidth: 40 * preferencesModel.uiScale
							useExternalBorder: false
							showBorder: true
						}
					}
				}
			}
		}
	}

	Section
	{
		title: qsTr("Advanced")

		Group
		{
			RadioButtonGroup
			{
				name: "advancedEstimationMethod"
				id: advancedEstimationMethod
				title: qsTr("Estimation method")
				info: qsTr("Method used to estimate the posterior distribution. Numerical integration is available for the normal distribution; Student-t requires MCMC.")
				onValueChanged:
				{
					if (value !== "integration")
						priorDistributionsCustomSpecification.currentIndex = 0
					if (value === "mcmc" && priorDistribution.currentValue === "unitInformation")
						priorDistribution.currentIndex = 0
				}

				RadioButton
				{
					value: "integration"
					label: qsTr("Numerical integration")
					checked: dataDistribution.currentValue === "normal"
					enabled: dataDistribution.currentValue === "normal"
				}

				RadioButton
				{
					value: "mcmc"
					label: qsTr("MCMC")
					checked: dataDistribution.currentValue === "studentT"
				}
			}

			Group
			{
				title: qsTr("MCMC Settings")
				enabled: advancedEstimationMethod.value === "mcmc" || priorPredictive.checked || posteriorPredictive.checked
				info: qsTr("Adjust MCMC settings for estimation and for predictive sampling when numerical integration requires an additional MCMC fit.")
				IntegerField
				{
					name: "advancedMcmcSettingsIterations"
					id: samplingIterations
					label: qsTr("Iterations")
					defaultValue: 5000
					min: 100
					max: 100000000
					info: qsTr("Number of sampling iterations per chain after warmup and before thinning. Warmup iterations are added to this number.")
				}
				IntegerField
				{
					name: "advancedMcmcSettingsWarmup"
					label: qsTr("Warmup")
					defaultValue: 1000
					min: 100
					max: 100000000
					info: qsTr("Number of warmup iterations per chain, performed before the sampling iterations.")
				}
				IntegerField
				{
					name: "advancedMcmcSettingsChains"
					label: qsTr("Chains")
					defaultValue: 1
					min: 1
					max: 128
					info: qsTr("Number of MCMC chains to run.")
				}
				IntegerField
				{
					name: "advancedMcmcSettingsThin"
					label: qsTr("Thin")
					defaultValue: 1
					min: 1
					max: Math.floor(samplingIterations.value / 10)
					info: qsTr("Keep every nth MCMC sample. Thinning is limited to one tenth of the sampling iterations.")
				}
				DoubleField
				{
					name: "advancedMcmcSettingsAdaptDelta"
					label: qsTr("Adapt delta")
					defaultValue: 0.80
					min: 0
					max: 1
					inclusive: JASP.None
					decimals: 4
					info: qsTr("Target acceptance probability for the MCMC sampler.")
				}
				IntegerField
				{
					name: "advancedMcmcSettingsMaximumTreedepth"
					label: qsTr("Maximum treedepth")
					defaultValue: 15
					min: 1
					max: 100
					info: qsTr("Maximum tree depth used by the MCMC sampler.")
				}
			}
		}

		SetSeed { }

		IntegerField
		{
			name: "advancedPriorSamples"
			label: qsTr("Prior samples")
			fieldWidth: 1.5 * jaspTheme.numericFieldWidth
			defaultValue: 200000
			min: 1000
			max: 100000000
			enabled: advancedEstimationMethod.value === "integration"
			info: qsTr("Number of Monte Carlo draws for prior-only summaries that require simulation with numerical integration.")
		}
	}
}
