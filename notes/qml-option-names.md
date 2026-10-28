# QML option names

Current option contract for the unreleased Bayesian process capability study. No upgrade mappings or R aliases are provided.

Names follow the visible label in camelCase and inherit their section/group/parent control prefix. Anonymous layouts add no prefix. Unlabelled companion fields use their parent choice plus `Value` or `Type`. Scoped ComponentsList row keys stay local to their list.

The shared JASP `SetSeed` control retains the standard `setSeed` and `seed` keys required by jaspBase. Distribution selector values are `normal` and `studentT`; sequential update choices are `bins`, `observationsPerBin`, and `perObservation`. qc argument names retain their statistical API.

## Main controls

| Option |
| --- |
| `availableVariables` |
| `measurement` |
| `capabilitySpecificationLowerLimit` |
| `capabilitySpecificationTargetValue` |
| `capabilitySpecificationUpperLimit` |
| `timeSeriesPlot` |
| `dataDistribution` |
| `priorDistribution` |
| `priorDistributionUnitInformationValue` |
| `processCriteria` |
| `statisticsMetricsCp` |
| `statisticsMetricsCpu` |
| `statisticsMetricsCpl` |
| `statisticsMetricsCpk` |
| `statisticsMetricsCpc` |
| `statisticsMetricsCpm` |
| `statisticsIntervalTable` |
| `statisticsCredibleInterval` |
| `statisticsProcessOverview` |
| `statisticsProcessOverviewCapabilityMetric` |
| `statisticsProcessOverviewThreshold` |
| `statisticsProcessOverviewSensitivityAnalysisPrior` |
| `statisticsProcessOverviewSequentialUpdates` |
| `statisticsProcessOverviewSequentialUpdatesBinsValue` |
| `statisticsProcessOverviewSequentialUpdatesObservationsPerBinValue` |
| `priorAndPosteriorInferenceShowProcessCriteria` |
| `sequentialAnalysisShowProcessCriteria` |
| `priorDistributionsCustomSpecification` |
| `priorDistributionsCustomDependentMeanMu` |
| `priorDistributionsCustomDependentMeanKappa` |
| `priorDistributionsCustomDependentSigmaSquaredAlpha` |
| `priorDistributionsCustomDependentSigmaSquaredBeta` |
| `advancedEstimationMethod` |
| `advancedMcmcSettingsIterations` |
| `advancedMcmcSettingsWarmup` |
| `advancedMcmcSettingsChains` |
| `advancedMcmcSettingsThin` |
| `advancedMcmcSettingsAdaptDelta` |
| `advancedMcmcSettingsMaximumTreedepth` |
| `advancedPriorSamples` |

## Shared plot controls

Each `Common.PlotLayout` instance defines its checkbox at the base name and appends the suffixes below. Hidden controls retain their names; the R dependency helpers include only controls used by that plot.

| Base name |
| --- |
| `priorAndPosteriorInferencePriorDistribution` |
| `priorAndPosteriorInferencePosteriorDistribution` |
| `sequentialAnalysisPointEstimatePlot` |
| `sequentialAnalysisIntervalEstimatePlot` |
| `priorAndPosteriorPredictivePlotsPriorPredictiveDistribution` |
| `priorAndPosteriorPredictivePlotsPosteriorPredictiveDistribution` |

| Suffix | Control |
| --- | --- |
| (empty) | Plot checkbox |
| `PointEstimate` | Point estimate |
| `PointEstimateType` | Point estimate type |
| `Ci` | CI |
| `CiType` | CI type |
| `CiMass` | CI mass |
| `CiLower` | CI lower |
| `CiUpper` | CI upper |
| `IntervalLower` | Interval lower |
| `IntervalUpper` | Interval upper |
| `ShowPriorDistribution` | Show prior distribution |

Layout and axes apply to both plots in their section. Their bases are `priorAndPosteriorInference` and `sequentialAnalysis`; neither inherits a particular plot checkbox.

| Suffix | Control |
| --- | --- |
| `Layout` | Layout |
| `Axes` | Axes |
| `CustomAxesXMin` | Custom axes / x axis / Min |
| `CustomAxesXMax` | Custom axes / x axis / Max |
| `CustomAxesYMin` | Custom axes / y axis / Min |
| `CustomAxesYMax` | Custom axes / y axis / Max |

## Component lists

`processCriteria` rows store `region` (unique row key), `leftBound`, `classification`, and `rightBound`. Defaults and header labels use the same field names. R derives left bounds from the preceding right bound.

Independent custom priors use `priorDistributionsCustomIndependentNormal` and `priorDistributionsCustomIndependentStudentT`. Rows store `parameter` (mean, sigma, or df), `distribution`, and the parameter/truncation fields below. The internal `df` row is displayed as **df - 2**: its prior applies to the positive offset, and the Student-t degrees of freedom equal that offset plus 2.

| Row field |
| --- |
| `distribution` |
| `mu` |
| `x0` |
| `sigma` |
| `k` |
| `theta` |
| `nu` |
| `alpha` |
| `beta` |
| `a` |
| `b` |
| `truncationLower` |
| `truncationUpper` |

The Cauchy location reads the displayed `x0` field. Truncation fields inherit the Truncation column heading. Symbols use their spelled-out names (`mu`, `sigma`, `theta`, `nu`, `alpha`, `beta`).

## Verification

Audit the main QML together with all imported components: jaspTools may not extract names assembled from bindings. Every R option read and dependency must resolve to a current QML name, including dynamic plot suffixes and both prior-list schemas. Preserve the human-owned tests and snapshots.

## Sequential plot behavior

The point-estimate selector offers mean and median; mode remains available in density and predictive plots. The sequential point-estimate checkbox defaults to checked and controls the estimate line independently of the CI ribbon. With both unchecked, no sequential estimate plot is drawn.

Central and HPD intervals use the selected CI mass. Custom intervals use the entered lower and upper bounds. Axis labels report the selected interval type and mass or custom bounds. The interval-estimate plot reports the probability inside its Interval bounds.

Sequential fits and summaries have separate dependency-controlled states, so presentation settings reuse the fitted distributions. The saved sequential fits omit the full Stan sampler; capability metric draws remain available for interval queries. Custom axes apply to both plot types and both layouts, including a single selected metric.

Deselecting every metric leaves the capability table empty.

## Sampling iterations and sequential updates

MCMC Iterations specifies sampling iterations per chain after warmup and before thinning. JASP adds Warmup when passing the total iteration count to qc, including the predictive fallback. For example, 500 sampling iterations and 1,000 warmup iterations request 1,500 total iterations per chain.

One overview bin gives one update using all observations. Multiple bins, observations per bin, and per-observation updates start with the first three observations and include all observations in the final update. Small datasets can yield fewer distinct checkpoints than the requested bin count.

The qc density plots support custom interval bounds with MCMC and numerical integration. The bounds determine the shaded interval; its posterior probability is not set by the CI mass field.

## User-facing messages

Notes and errors refer to the visible controls and groups. Specification inputs are Lower limit, Target value, and Upper limit under Capability Specification. Overview messages use Capability metric, Threshold, Sensitivity analysis prior, and Sequential updates under Process overview. Process Criteria messages identify Right bound and Classification. Custom-prior errors use the displayed parameter labels under Prior Distributions (Custom).

## Bounds and classification identifiers

Sampling Iterations and Warmup both have a GUI minimum of 100. Custom sequential y-axis limits accept negative values because capability metrics can be negative. Density y-axis limits remain nonnegative.

Process Criteria classifications are display labels. Rows use internal numeric keys (`1`, `2`, and so on) for region grouping and colors. Duplicate classifications retain the entered text in tables, legends, and axes; their regions remain distinct internally.

## Prior support, help, and unavailable overlays

The df - 2 explanation is available through the custom-prior menu help and is not an inline interface label. The VariablesForm preferred height is half the default theme height.

Uniform lower bounds for Sigma and df - 2 must be at least zero; upper bounds must be greater than the lower bound. Other distributions retain positive scale and shape parameters and parameter-specific lower truncation bounds. Negative location parameters remain valid when the resulting prior is truncated to positive support.

Unavailable-prior errors are attached to the posterior-distribution output when its prior overlay is requested, and to Process overview when the prior is unavailable. The posterior plot object is retained.
