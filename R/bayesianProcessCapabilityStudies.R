#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#'@importFrom jaspBase jaspDeps %setOrRetrieve% createJaspPlot createJaspState createJaspTable
#'@importFrom rlang .data


#'@export
bayesianProcessCapabilityStudies <- function(jaspResults, dataset, options) {

  jaspBase::.setSeedJASP(options)

  options[["capabilitySpecificationLowerLimit"]] <- .bpcsSpecificationValue(options[["capabilitySpecificationLowerLimit"]], gettext("Lower limit"))
  options[["capabilitySpecificationTargetValue"]] <- .bpcsSpecificationValue(options[["capabilitySpecificationTargetValue"]], gettext("Target value"))
  options[["capabilitySpecificationUpperLimit"]] <- .bpcsSpecificationValue(options[["capabilitySpecificationUpperLimit"]], gettext("Upper limit"))

  rawDataset <- dataset
  nMissing <- if (!is.null(dataset) && ncol(dataset) > 0L) sum(is.na(dataset[[1L]])) else 0L
  if (nMissing > 0L)
    dataset <- dataset[!is.na(dataset[[1L]]), , drop = FALSE]

  fit <- if (.bpcsIsReady(options)) tryCatch({
    if (is.null(dataset) || nrow(dataset) == 0L)
      stop(gettext("No observations remain after excluding missing measurements."), call. = FALSE)
    if (any(!is.finite(dataset[[1L]])))
      stop(gettext("Measurements must be finite. Remove or replace infinite values."), call. = FALSE)
    .bpcsSamplePosteriorOrPrior(jaspResults, dataset, options)
  }, error = function(e) list(error = conditionMessage(e))) else NULL
  if (!is.null(fit$error)) {
    .bpcsCapabilityTable(jaspResults, dataset, options, 1, fit, nMissing, list())
    .bpcsTimeSeriesPlot(jaspResults, rawDataset, options, position = 3)
    return()
  }
  priorFit <- if (.bpcsSpecificationsReady(options) && .bpcsPriorRequested(options))
    .bpcsSamplePosteriorOrPrior(jaspResults, dataset, options, prior = TRUE) else NULL
  predictive <- lapply(c("Posterior", "Prior"), function(type) {
    base <- paste0("priorAndPosteriorPredictivePlots", type, "PredictiveDistribution")
    ready <- if (type == "Prior") .bpcsSpecificationsReady(options) else .bpcsIsReady(options)
    if (ready && options[[base]])
      .bpcsPredictiveSamples(jaspResults, dataset, options, if (type == "Prior") priorFit else fit, base)
  })
  .bpcsCapabilityTable(jaspResults, dataset, options, position = 1, fit = fit,
                       nMissing = nMissing, predictive = predictive)

  .bpcsProcessOverviewPlot(jaspResults, dataset, options, fit, priorFit, position = 2, rawDataset = rawDataset)
  .bpcsTimeSeriesPlot(jaspResults, rawDataset, options, position = 3)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 4)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 5, base = "priorAndPosteriorInferencePriorDistribution")

  .bpcsIntervalTable(jaspResults, options, fit, position = 6)
  .bpcsParameterTable(jaspResults, options, fit, position = 7, predictive = predictive)

  .bpcsSequentialPointEstimatePlot(   jaspResults, dataset, options, fit, position = 8)
  .bpcsSequentialIntervalEstimatePlot(jaspResults, dataset, options, fit, position = 9)

  .bpcsPlotPredictive(jaspResults, dataset, options, fit,      position = 10, base = "priorAndPosteriorPredictivePlotsPosteriorPredictiveDistribution")
  .bpcsPlotPredictive(jaspResults, dataset, options, priorFit, position = 11, base = "priorAndPosteriorPredictivePlotsPriorPredictiveDistribution")

}

.bpcsPriorRequested <- function(options) {
  (.bpcsIsReady(options) && options[["statisticsProcessOverview"]]) ||
    options[["priorAndPosteriorInferencePriorDistribution"]] ||
    (.bpcsIsReady(options) && options[["priorAndPosteriorInferencePosteriorDistribution"]] && options[["priorAndPosteriorInferencePosteriorDistributionShowPriorDistribution"]]) ||
    options[["priorAndPosteriorPredictivePlotsPriorPredictiveDistribution"]]
}

.bpcsSpecificationValue <- function(value, label) {
  if (trimws(as.character(value)) == "")
    return("")

  # Optional formula fields preserve their text so empty inputs remain unset.
  functions <- c("+", "-", "*", "/", "^", "%%", "%/%", "(", "abs", "sqrt", "exp", "log", "log10", "round", "signif")
  valid <- function(node) {
    if (is.numeric(node))
      return(TRUE)
    if (is.name(node))
      return(identical(node, as.name("pi")))
    is.call(node) && is.name(node[[1L]]) && as.character(node[[1L]]) %in% functions &&
      all(vapply(as.list(node)[-1L], valid, logical(1)))
  }
  result <- tryCatch({
    expression <- parse(text = as.character(value))
    if (length(expression) != 1L || !valid(expression[[1L]]))
      stop("Invalid numeric expression")
    eval(expression, envir = baseenv())
  }, error = function(e) NULL)

  if (!is.numeric(result) || is.complex(result) || length(result) != 1L || !is.finite(result))
    jaspBase::.quitAnalysis(gettextf("%s must be a finite numeric value or arithmetic expression.", label))

  result
}

.bpcsIsReady <- function(options) {
  hasData <- length(options[["measurement"]]) > 0L && options[["measurement"]] != ""
  hasData && .bpcsSpecificationsReady(options)
}

.bpcsSpecificationsReady <- function(options) {
  options[["capabilitySpecificationLowerLimit"]] != "" &&
    options[["capabilitySpecificationUpperLimit"]] != "" &&
    options[["capabilitySpecificationTargetValue"]] != ""
}

.bpcsStateDeps <- function(options, prior = FALSE, mcmc = options[["advancedEstimationMethod"]] == "mcmc") {
  c(
      # data
      "measurement",
      # specification
      "capabilitySpecificationTargetValue", "capabilitySpecificationLowerLimit", "capabilitySpecificationUpperLimit",
      # likelihood
      "dataDistribution", "advancedEstimationMethod",
      # prior
      "priorDistribution", "priorDistributionUnitInformationValue", "priorDistributionsCustomIndependentNormal", "priorDistributionsCustomIndependentStudentT",
      "priorDistributionsCustomSpecification", "priorDistributionsCustomDependentMeanMu", "priorDistributionsCustomDependentMeanKappa", "priorDistributionsCustomDependentSigmaSquaredAlpha", "priorDistributionsCustomDependentSigmaSquaredBeta",
      # MCMC settings
      if (mcmc) .bpcsMcmcDeps(),
      "setSeed", "seed",
      if (prior && options[["advancedEstimationMethod"]] == "integration") "advancedPriorSamples"
  )
}

.bpcsDefaultDeps <- function(options, prior = FALSE) {
  c(
      .bpcsStateDeps(options, prior = prior),
      # metrics
      "statisticsMetricsCp", "statisticsMetricsCpu", "statisticsMetricsCpl", "statisticsMetricsCpk", "statisticsMetricsCpc", "statisticsMetricsCpm"
  )
}

.bpcsMcmcDeps <- function() {
  c("advancedMcmcSettingsIterations", "advancedMcmcSettingsWarmup", "advancedMcmcSettingsChains",
    "advancedMcmcSettingsAdaptDelta", "advancedMcmcSettingsMaximumTreedepth", "advancedMcmcSettingsThin")
}

.bpcsPredictiveDeps <- function(options) {
  bases <- c("priorAndPosteriorPredictivePlotsPosteriorPredictiveDistribution",
             "priorAndPosteriorPredictivePlotsPriorPredictiveDistribution")
  c(bases, if (any(vapply(bases, function(base) options[[base]], logical(1)))) .bpcsMcmcDeps(),
    if (options[[bases[2L]]]) .bpcsStateDeps(options, prior = TRUE))
}

.bpcsAllMetrics <- function() {
  # casing must match the metric names qc uses, it errors on CpU / CpL
  c("Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm")
}

.bpcsDistributionFromOptions <- function(options) {
  switch(
    options[["dataDistribution"]],
    "normal" = "normal",
    "studentT" = "t",
    stop(gettextf("Unknown selection for Data distribution: %s.", options[["dataDistribution"]]), call. = FALSE)
  )
}

# Share model settings across posterior, prior, sequential, and predictive fits.
.bpcsFit <- function(x, options, prior = .bpcsPriorHelper(options), samplePriors = FALSE,
                     method = options[["advancedEstimationMethod"]]) {
  jaspBase::.setSeedJASP(options)
  priorMcSamples <- qc::qc.get_option("prior_mc_samples")
  on.exit(qc::qc.options(prior_mc_samples = priorMcSamples), add = TRUE)
  qc::qc.options(prior_mc_samples = options[["advancedPriorSamples"]])

  qc::bpc(
    x,
    distribution  = .bpcsDistributionFromOptions(options),
    method        = method,
    target        = options[["capabilitySpecificationTargetValue"]],
    LSL           = options[["capabilitySpecificationLowerLimit"]],
    USL           = options[["capabilitySpecificationUpperLimit"]],
    prior         = prior,
    chains        = options[["advancedMcmcSettingsChains"]],
    warmup        = options[["advancedMcmcSettingsWarmup"]],
    iter          = options[["advancedMcmcSettingsIterations"]] + options[["advancedMcmcSettingsWarmup"]],
    thin          = options[["advancedMcmcSettingsThin"]],
    control       = qc::set_control(adapt_delta = options[["advancedMcmcSettingsAdaptDelta"]], max_treedepth = options[["advancedMcmcSettingsMaximumTreedepth"]]),
    silent        = TRUE,
    seed          = jaspBase::.getSeedJASP(options),
    sample_priors = samplePriors
  )
}

.bpcsPlotLayoutDeps <- function(base, hasPrior = TRUE, hasEstimate = TRUE, hasCi = TRUE, hasInterval = FALSE) {
  c(
    base,
    if (hasEstimate) .bpcsPlotLayoutEstimateDeps(base),
    if (hasCi)       .bpcsPlotLayoutCiDeps(base),
    if (hasInterval) .bpcsPlotLayoutIntervalDeps(base),
    if (hasPrior)    .bpcsPlotLayoutPriorDeps(base)
  )
}

.bpcsPlotLayoutEstimateDeps <- function(base) { paste0(base, c("PointEstimate", "PointEstimateType")) }
.bpcsPlotLayoutCiDeps       <- function(base) { paste0(base, c("Ci", "CiType", "CiMass", "CiLower", "CiUpper")) }
.bpcsPlotLayoutIntervalDeps <- function(base) { paste0(base, c("IntervalLower", "IntervalUpper")) }
.bpcsPlotLayoutAxesDeps     <- function(base) { paste0(base, c("Layout", "Axes", "CustomAxesXMin", "CustomAxesXMax", "CustomAxesYMin", "CustomAxesYMax")) }
.bpcsPlotLayoutPriorDeps    <- function(base) { paste0(base, "ShowPriorDistribution") }

.bpcsProcessCriteriaDeps <- function() {
  "processCriteria"
}

.bpcsOverviewOptionDeps <- function() {
  c("statisticsProcessOverview", "statisticsProcessOverviewCapabilityMetric", "statisticsProcessOverviewThreshold", "statisticsProcessOverviewSensitivityAnalysisPrior",
    .bpcsOverviewBinningDeps())
}

.bpcsOverviewBinningDeps <- function() {
  c("statisticsProcessOverviewSequentialUpdates", "statisticsProcessOverviewSequentialUpdatesBinsValue", "statisticsProcessOverviewSequentialUpdatesObservationsPerBinValue")
}

.bpcsScalarOption <- function(value, type = c("numeric", "character")) {
  type <- match.arg(type)
  if (length(value) != 1L)
    return(if (type == "numeric") NA_real_ else NA_character_)
  if (type == "numeric") suppressWarnings(as.numeric(value)) else as.character(value)
}

.bpcsProcessCriteria <- function(options) {
  criteria <- options[["processCriteria"]]

  if (!is.list(criteria) || length(criteria) < 2L)
    stop(gettext("Specify at least two regions under Process Criteria."), call. = FALSE)

  n      <- length(criteria)
  upper  <- vapply(criteria, function(region) .bpcsScalarOption(region[["rightBound"]]), numeric(1))
  labels <- vapply(criteria, function(region) .bpcsScalarOption(region[["classification"]], "character"), character(1))

  # The right bounds define the regions: the qml only displays each left bound as the right bound of the
  # row above, so the serialized left bounds are ignored. The outer bounds are always open-ended.
  upper[n] <- Inf
  lower    <- c(-Inf, upper[-n])

  if (anyNA(upper) || !all(is.finite(upper[-n])))
    stop(gettext("Right bound values in Process Criteria must be numeric."), call. = FALSE)
  if (any(lower >= upper))
    stop(gettext("Each Right bound in Process Criteria must be larger than the previous one."), call. = FALSE)
  if (anyNA(labels) || any(!nzchar(trimws(labels))))
    stop(gettext("Each region in Process Criteria needs a Classification."), call. = FALSE)

  list(
    lower  = lower,
    upper  = upper,
    keys   = as.character(seq_len(n)),
    labels = labels,
    values = upper[-n]
  )
}

.bpcsPriorComponentByName <- function(options, name) {
  listName <- switch(
    .bpcsDistributionFromOptions(options),
    "normal" = "priorDistributionsCustomIndependentNormal",
    "t"      = "priorDistributionsCustomIndependentStudentT"
  )
  for (comp in options[[listName]]) {
    if (identical(comp[["parameter"]], name))
      return(comp)
  }
  parameterLabel <- switch(name, mean = gettext("Mean"), sigma = gettext("Sigma"), df = "df - 2")
  stop(gettextf("No prior distribution is specified for %s under Prior Distributions (Custom).", parameterLabel), call. = FALSE)
}

.bpcsActivePriorComponents <- function(options) {
  names <- switch(
    .bpcsDistributionFromOptions(options),
    "normal" = c("mean", "sigma"),
    "t"      = c("mean", "sigma", "df")
  )
  lapply(names, .bpcsPriorComponentByName, options = options)
}

.bpcsPriorFromComponent <- function(optionsPrior, paramName) {
  if (optionsPrior[["distribution"]] == "jeffreys")
    return(paste0("Jeffreys_", paramName))

  arguments <- list()

  arguments[["distribution"]] <- switch(
    optionsPrior[["distribution"]],
    "gammaAB" = "gamma",
    "gammaK0" = "gamma",
    optionsPrior[["distribution"]]
  )

  arguments[["parameters"]] <- switch(
    optionsPrior[["distribution"]],
    "normal"      = list("mean" = optionsPrior[["mu"]], "sd" = optionsPrior[["sigma"]]),
    "t"           = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "cauchy"      = list("location" = optionsPrior[["x0"]], "scale" = optionsPrior[["theta"]]),
    "gammaAB"     = list("shape" = optionsPrior[["alpha"]], "rate" = optionsPrior[["beta"]]),
    "gammaK0"     = list("shape" = optionsPrior[["k"]], "rate" = 1/optionsPrior[["theta"]]),
    "invgamma"    = list("shape" = optionsPrior[["alpha"]], "scale" = optionsPrior[["beta"]]),
    "lognormal"   = list("meanlog" = optionsPrior[["mu"]], "sdlog" = optionsPrior[["sigma"]]),
    "beta"        = list("alpha" = optionsPrior[["alpha"]], "beta" = optionsPrior[["beta"]]),
    "uniform"     = list("a" = optionsPrior[["a"]], "b" = optionsPrior[["b"]]),
    "exponential" = list("rate" = optionsPrior[["lambda"]]),
    "spike"       = list("location" = optionsPrior[["x0"]])
  )

  if (!arguments[["distribution"]] %in% c("spike", "uniform")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  return(do.call(BayesTools::prior, arguments))
}

.bpcsPriorHelper <- function(options) {
  if (options[["priorDistribution"]] == "default") {
    if (options[["advancedEstimationMethod"]] == "integration") {
      return("DCSI")
    }
    return("Jeffreys")
  }
  if (options[["priorDistribution"]] == "unitInformation")
    return(qc::prior_unit_information(n0 = options[["priorDistributionUnitInformationValue"]]))
  if (options[["priorDistribution"]] == "jeffreys")
    return("Jeffreys")
  if (options[["priorDistributionsCustomSpecification"]] == "dependent")
    return(qc::prior_conjugate(
      mu0    = options[["priorDistributionsCustomDependentMeanMu"]],
      k0     = options[["priorDistributionsCustomDependentMeanKappa"]],
      alpha0 = options[["priorDistributionsCustomDependentSigmaSquaredAlpha"]],
      beta0  = options[["priorDistributionsCustomDependentSigmaSquaredBeta"]]
    ))

  args <- list(
    mu    = .bpcsPriorFromComponent(.bpcsPriorComponentByName(options, "mean"), "mu"),
    sigma = .bpcsPriorFromComponent(.bpcsPriorComponentByName(options, "sigma"), "sigma")
  )
  if (options[["dataDistribution"]] == "studentT")
    args$nu <- .bpcsPriorFromComponent(.bpcsPriorComponentByName(options, "df"), "df")

  do.call(qc::prior_independent, args)
}

# Tables ----
.bpcsCapabilityTable <- function(jaspResults, dataset, options, position, fit, nMissing, predictive) {

  # Check if we already have the results cached
  if (!is.null(jaspResults[["bpcsCapabilityTable"]])) {
    if (!.bpcsIsReady(options))
      return(NULL)
    return(fit)
  }

  table <- .bpcsCapabilityTableMeta(jaspResults, options, position = position)
  if (nMissing > 0L)
    table$addFootnote(sprintf(ngettext(nMissing,
      "%d observation with a missing measurement was excluded.",
      "%d observations with missing measurements were excluded."), nMissing))
  if (!is.null(fit$error)) {
    table$setError(gettextf("The process capability model could not be fitted: %s", fit$error))
    return(fit)
  }
  for (i in seq_along(predictive)) {
    if (isTRUE(predictive[[i]]$fallback))
      table$addFootnote(if (i == 1L)
        gettext("Posterior predictive sampling switched to MCMC because predictive sampling was unavailable for the numerical integration fit.") else
        gettext("Prior predictive sampling switched to MCMC because predictive sampling was unavailable for the numerical integration fit."))
  }
  if (!.bpcsIsReady(options)) {

    if (options[["capabilitySpecificationLowerLimit"]] == "" || options[["capabilitySpecificationUpperLimit"]] == "" || options[["capabilitySpecificationTargetValue"]] == "")
      table$addFootnote(gettext(
        "Please specify Lower limit, Target value, and Upper limit under Capability Specification to compute the capability metrics."
      ))
    else
      table$addFootnote(gettext("Assign a measurement variable to compute posterior results."))

    return(NULL)
  }

  resultsObject <- fit

  .bpcsCapabilityTableFill(table, resultsObject, options)
  return(resultsObject)

}

# Returns list(rawfit, summaryObject), assembled from the dependency-controlled fit and summary
# states on every call. For prior = TRUE an unavailable prior gives list(error = <message>) instead,
# so the posterior outputs keep working and the prior-only outputs can explain why they are empty.
.bpcsSamplePosteriorOrPrior <- function(jaspResults, dataset, options, prior = FALSE) {

  base <- if (prior) "bpcsPriors" else "bpcs"
  if (prior && !.bpcsPriorIsProper(options))
    return(list(error = gettext("The selected Prior distribution is improper, so it cannot be sampled.")))

  x <- if (!is.null(dataset) && ncol(dataset) > 0L) dataset[[1L]] else NULL
  if (prior && options[["priorDistribution"]] == "unitInformation" && sum(is.finite(x)) < 2L)
    return(list(error = gettext("The unit information prior requires at least two finite measurements. Assign a measurement variable or choose a fully specified proper prior.")))
  if (prior && options[["priorDistribution"]] != "unitInformation") x <- NULL

  sample <- function() {
    rawfit <- jaspResults[[paste0(base, "State")]] %setOrRetrieve% (
      .bpcsFit(x, options, samplePriors = prior) |>
        createJaspState(jaspDeps(.bpcsStateDeps(options, prior = prior)))
    )

    summaryObject <- jaspResults[[paste0(base, "SummaryState")]] %setOrRetrieve% (
      summary(
        rawfit, ci.level = options[["statisticsCredibleInterval"]]
      ) |>
        createJaspState(jaspDeps(
            options = c(.bpcsStateDeps(options, prior = prior), "statisticsCredibleInterval")
        ))
    )

    list(
      rawfit           = rawfit,
      summaryObject    = summaryObject
    )
  }

  if (!prior)
    return(sample())

  tryCatch(sample(), error = function(e) {
    list(error = gettextf("The selected Prior distribution could not be computed: %s", conditionMessage(e)))
  })
}

.bpcsPriorIsUsable <- function(priorFit) {
  !is.null(priorFit) && is.null(priorFit[["error"]])
}

# Whether the displayed prior is proper. Jeffreys components (and the default Jeffreys prior of the
# MCMC models) are improper, as is a uniform component without finite bounds.
.bpcsPriorIsProper <- function(options) {
  if (options[["priorDistribution"]] == "jeffreys")
    return(FALSE)
  if (options[["priorDistribution"]] == "default")
    return(options[["advancedEstimationMethod"]] == "integration")
  if (options[["priorDistribution"]] == "unitInformation")
    return(TRUE)
  if (options[["priorDistributionsCustomSpecification"]] == "dependent")
    return(TRUE)

  for (comp in .bpcsActivePriorComponents(options)) {
    if (identical(comp[["distribution"]], "jeffreys"))
      return(FALSE)
    if (identical(comp[["distribution"]], "uniform") &&
        !(is.finite(.bpcsScalarOption(comp[["a"]])) && is.finite(.bpcsScalarOption(comp[["b"]]))))
      return(FALSE)
  }
  TRUE
}

.bpcsCapabilityTableMeta <- function(jaspResults, options, position) {

  table <- createJaspTable(title = gettext("Capability Table"), position = position)
  table$addColumnInfo(name = "metric",  title = gettext("Measure"), type = "string")
  table$addColumnInfo(name = "mean",    title = gettext("Mean"),    type = "number")
  table$addColumnInfo(name = "median",  title = gettext("Median"),  type = "number")
  table$addColumnInfo(name = "sd",      title = gettext("Std"),     type = "number")

  overtitle <- gettextf("%s%% Credible Interval", 100 * options[["statisticsCredibleInterval"]])
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  table$dependOn(c(.bpcsDefaultDeps(options), "statisticsCredibleInterval",
    .bpcsPredictiveDeps(options)))

  jaspResults[["bpcsCapabilityTable"]] <- table
  return(table)

}

.bpcsGetSelectedMetrics <- function(options) {
  allMetrics <- .bpcsAllMetrics()
  selectedMetrics <- allMetrics[c(options[["statisticsMetricsCp"]],   options[["statisticsMetricsCpu"]],  options[["statisticsMetricsCpl"]],
                                  options[["statisticsMetricsCpk"]],  options[["statisticsMetricsCpc"]],  options[["statisticsMetricsCpm"]])]
  return(selectedMetrics)
}

.bpcsGetCustomAxisLimits <- function(options, base) {
  keys <- c(paste0(base, "CustomAxesX", c("Min", "Max")), paste0(base, "CustomAxesY", c("Min", "Max")))
  values <- lapply(keys, function(k) options[[k]])
  names(values) <- c("xmin", "xmax", "ymin", "ymax")
  values
}
# end utils

.bpcsCapabilityTableFill <- function(table, resultsObject, options) {

  df <- as.data.frame(resultsObject[["summaryObject"]][["summary"]])

  # Filter metrics based on user selection
  selectedMetrics <- .bpcsGetSelectedMetrics(options)

  df <- df[df$metric %in% selectedMetrics, , drop = FALSE]

  table$setData(df)
  diagnostics <- resultsObject$summaryObject$divergence_diagnostics
  if (!is.null(diagnostics)) {
    for (i in seq_len(nrow(diagnostics))) {
      metric <- diagnostics$metric[i]
      if (!metric %in% selectedMetrics) next
      if (diagnostics$mean_divergent[i])
        table$addFootnote(gettextf("%s: the posterior mean is infinite under the selected prior.", metric))
      if (diagnostics$sd_divergent[i])
        table$addFootnote(gettextf("%s: the posterior standard deviation is infinite under the selected prior.", metric))
    }
  }

}

.bpcsParameterSummary <- function(rawfit, ciMass) {
  isT <- rawfit$distribution == "t"
  estimated <- c(rawfit$stan_priors$estimate_mu, rawfit$stan_priors$estimate_sigma,
                 if (isT) rawfit$stan_priors$estimate_nu) == 1L
  pars <- c("mu", if (isT) "scale" else "sigma", if (isT) "nu")
  draws <- rstan::extract(rawfit$stanfit, pars = pars, permuted = FALSE, inc_warmup = FALSE)
  if (isT) {
    draws[, , "scale"] <- draws[, , "scale"] * sqrt(draws[, , "nu"] / (draws[, , "nu"] - 2))
    dimnames(draws)[[3L]][2L] <- "sigma"
    if (!estimated[2L]) draws[, , "sigma"] <- rawfit$stan_priors$fixed_sigma[1L]
  }
  alpha <- (1 - ciMass) / 2
  sm <- rstan::monitor(draws, warmup = 0, probs = c(alpha, 0.5, 1 - alpha), print = FALSE)
  data <- as.data.frame(sm[, c(1L, 3L, 5L, 4L, 6L, 2L, 7L, 8L), drop = FALSE])
  names(data) <- c("mean", "sd", "median", "lower", "upper", "mcse", "ess", "rhat")
  data[!estimated, c("mcse", "ess", "rhat")] <- NA_real_
  data$parameter <- c(gettext("Mean (\u03bc)"), gettext("SD (\u03c3)"), if (isT) gettext("df (\u03bd)"))
  list(data = data, fixed = !estimated)
}

.bpcsParameterTable <- function(jaspResults, options, fit, position, predictive) {
  if (!options[["statisticsParameterEstimates"]] ||
      !is.null(jaspResults[["parameterEstimates"]])) return()
  table <- createJaspTable(title = gettext("Parameter estimates"), position = position)
  table$dependOn(c(.bpcsStateDeps(options), .bpcsPredictiveDeps(options), "statisticsParameterEstimates", "statisticsCredibleInterval"))
  jaspResults[["parameterEstimates"]] <- table
  table$addColumnInfo(name = "parameter", title = gettext("Parameter"), type = "string")
  table$addColumnInfo(name = "source", title = gettext("Distribution"), type = "string")
  columns <- c(mean = gettext("Mean"), median = gettext("Median"), sd = gettext("SD"),
               lower = gettext("Lower"), upper = gettext("Upper"), mcse = gettext("MCSE"),
               ess = gettext("Effective sample size"), rhat = gettext("R-hat"))
  for (name in names(columns))
    table$addColumnInfo(name = name, title = columns[[name]], type = "number",
      overtitle = if (name %in% c("lower", "upper")) gettextf("%s%% CI", 100 * options[["statisticsCredibleInterval"]]) else "")
  if (is.null(fit) && all(vapply(predictive, function(x) is.null(x$mcmcFit), logical(1)))) return()
  tryCatch({
    fits <- list()
    if (!is.null(fit) && fit$rawfit$method == "mcmc") fits[[gettext("Posterior")]] <- fit$rawfit
    labels <- c(gettext("Posterior predictive MCMC fit"), gettext("Prior predictive MCMC fit"))
    for (i in seq_along(predictive))
      if (!is.null(predictive[[i]]$mcmcFit)) fits[[labels[i]]] <- predictive[[i]]$mcmcFit
    if (length(fits) == 0L) {
      table$addFootnote(gettext("No MCMC fit was required. Parameter estimates and sampler diagnostics are available for MCMC fits only."))
      return()
    }
    rows <- lapply(names(fits), function(label) {
      result <- .bpcsParameterSummary(fits[[label]], options[["statisticsCredibleInterval"]])
      result$data$source <- label
      if (any(result$fixed))
        table$addFootnote(gettext("Parameters fixed by a point prior have no sampling diagnostics."))
      sampler <- do.call(rbind, rstan::get_sampler_params(fits[[label]]$stanfit, inc_warmup = FALSE))
      table$addFootnote(gettextf("%1$s: %2$d post-warmup transitions were divergent; %3$d reached the maximum treedepth (%4$d).",
        label, sum(sampler[, "divergent__"]), sum(sampler[, "treedepth__"] >= options[["advancedMcmcSettingsMaximumTreedepth"]]),
        options[["advancedMcmcSettingsMaximumTreedepth"]]))
      result$data
    })
    table$setData(do.call(rbind, rows))
    table$addFootnote(gettext("MCSE is the Monte Carlo standard error of the mean. Effective sample size and R-hat are computed by Stan using post-warmup draws."))
  }, error = function(e) table$setError(gettextf("Parameter estimates could not be computed: %s", conditionMessage(e))))
}

.bpcsIntervalTable <- function(jaspResults, options, fit, position) {

  if (!options[["statisticsIntervalTable"]] || !is.null(jaspResults[["bpcsIntervalTable"]]))
    return()

  criteria <- tryCatch(.bpcsProcessCriteria(options), error = function(e) e)
  table <- .bpcsIntervalTableMeta(jaspResults, options, position, criteria)
  if (inherits(criteria, "error")) {
    table$setError(gettextf("Invalid Process Criteria: %s", conditionMessage(criteria)))
    return()
  }

  if (!.bpcsIsReady(options) || is.null(fit))
    return()

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  tryCatch({

    intervalSummary <- summary(fit[["rawfit"]], interval_probability = criteria$values)[["interval_summary"]]
    colnames(intervalSummary) <- c("metric", paste0("interval", seq_along(criteria$labels)))
    intervalSummary <- intervalSummary[intervalSummary$metric %in% selectedMetrics, , drop = FALSE]
    table$setData(intervalSummary)

  }, error = function(e) {

    table$setError(gettextf("Unexpected error in Interval table: %s", e$message))

  })

  return()
}

.bpcsIntervalTableMeta <- function(jaspResults, options, position, criteria) {

  table <- createJaspTable(title = gettext("Interval Table"), position = position)
  table$dependOn(c("statisticsIntervalTable", .bpcsDefaultDeps(options), .bpcsProcessCriteriaDeps()))
  jaspResults[["bpcsIntervalTable"]] <- table

  table$addColumnInfo(name = "metric", title = gettext("Capability\nMeasure"), type = "string")

  if (inherits(criteria, "error"))
    return(table)

  intervalBounds <- c(criteria$lower[1L], criteria$upper)
  intervalNames <- criteria$labels
  n <- length(intervalBounds)

  # custom format helper. we don't use e.g., %.3f directly because that adds trailing zeros (2.000 instead of 2)
  fmt <- \(x) formatC(x, digits = 3, format = "f", drop0trailing = TRUE)
  for (i in 1:(n - 1)) {
    j <- i + 1
    lhs <- if (i == 1)     "(" else "["
    rhs <- if (i == n - 1) ")" else "]"
    title <- sprintf("%s %s%s, %s%s", intervalNames[i], lhs, fmt(intervalBounds[i]), fmt(intervalBounds[j]), rhs)
    table$addColumnInfo(name = paste0("interval", i), title = title, type = "number")
  }

  return(table)
}


# Plots ----
.bpcsProcessOverviewPlot <- function(jaspResults, dataset, options, fit, priorFit, position, rawDataset) {

  base <- "statisticsProcessOverview"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = gettext("Process Overview"), width = 1200, height = 800,
    position = position,
    dependencies = jaspDeps(c(
      .bpcsOverviewOptionDeps(), .bpcsStateDeps(options, prior = TRUE), .bpcsProcessCriteriaDeps()
    ))
  )
  jaspResults[[base]] <- plot

  if (!.bpcsIsReady(options) || is.null(fit) || jaspResults$getError())
    return()

  tryCatch({
    criteria <- .bpcsProcessCriteria(options)
    data <- .bpcsOverviewData(jaspResults, dataset, options, fit, criteria)
    plot$plotObject <- .bpcsMakeProcessOverviewPlot(rawDataset, options, fit, priorFit, data, criteria)
  }, error = function(e) {
    plot$setError(gettextf("Unexpected error in Process overview: %s", e$message))
  })
}

# The sequential fits and the reference-prior fit are cached separately, and both store results for
# every metric, so changing the overview metric or threshold reuses all fits. They are keyed on the
# numeric region boundaries instead of the processCriteria option: renaming a region must not refit.
.bpcsOverviewData <- function(jaspResults, dataset, options, fit, criteria) {
  sequential <- .bpcsCachedForBoundaries(
    jaspResults, "statisticsProcessOverviewSequentialData", criteria$values,
    deps    = c(.bpcsOverviewBinningDeps(), .bpcsStateDeps(options)),
    compute = function() .bpcsComputeOverviewSequentialData(dataset, options, criteria)
  )
  reference <- .bpcsCachedForBoundaries(
    jaspResults, "statisticsProcessOverviewReferenceData", criteria$values,
    deps    = c("statisticsProcessOverviewSensitivityAnalysisPrior", .bpcsStateDeps(options)),
    compute = function() .bpcsComputeOverviewReferenceData(dataset, options, fit, criteria)
  )
  list(sequential = sequential, sensitivity = reference)
}

# State that survives option changes (via deps) and is additionally recomputed when the region
# boundaries it was computed for differ from the current ones.
.bpcsCachedForBoundaries <- function(jaspResults, key, boundaries, deps, compute) {
  state <- jaspResults[[key]]
  if (!is.null(state) && identical(unname(state$object[["boundaries"]]), unname(boundaries)))
    return(state$object)

  result <- compute()
  result[["boundaries"]] <- boundaries
  if (is.null(state))
    jaspResults[[key]] <- createJaspState(result, jaspDeps(deps))
  else
    state$object <- result
  result
}

.bpcsOverviewThresholdIndex <- function(options, criteria) {
  threshold <- .bpcsScalarOption(options[["statisticsProcessOverviewThreshold"]])
  index <- if (is.finite(threshold)) which(abs(criteria$values - threshold) <= 1e-9 * max(1, abs(threshold))) else integer()
  if (length(index) != 1L)
    stop(gettext("Select a Threshold from the Right bound values in Process Criteria."), call. = FALSE)
  index
}

.bpcsOverviewRegionColors <- function(criteria) {
  colors <- qc::default_region_colors()
  if (length(criteria$labels) > length(colors))
    colors <- grDevices::hcl.colors(length(criteria$labels), palette = "Set 2")
  else
    colors <- colors[seq_len(length(criteria$labels))]

  names(colors) <- criteria$keys
  colors
}

.bpcsLabelProcessCriteria <- function(plot, criteria) {
  fillScale <- plot$scales$get_scales("fill")
  fillScale$labels <- stats::setNames(criteria$labels, criteria$keys)
  plot
}

# Cumulative-fit checkpoints: each one refits the model on the first k observations.
.bpcsOverviewSampleSizes <- function(n, options) {
  if (!is.finite(n) || n < 3L)
    stop(gettext("Process overview requires at least 3 observations."), call. = FALSE)

  n <- as.integer(n)
  mode <- options[["statisticsProcessOverviewSequentialUpdates"]]
  if (identical(mode, "perObservation"))
    return(seq.int(3L, n))

  if (identical(mode, "observationsPerBin")) {
    size <- options[["statisticsProcessOverviewSequentialUpdatesObservationsPerBinValue"]]
    return(unique(c(seq.int(3L, n, by = size), n)))
  }

  if (identical(mode, "bins")) {
    bins <- options[["statisticsProcessOverviewSequentialUpdatesBinsValue"]]
    if (bins == 1L)
      return(n)
    return(unique(as.integer(round(seq(3, n, length.out = bins)))))
  }

  stop(gettext("Unknown selection for Sequential updates in Process overview."), call. = FALSE)
}

.bpcsPriorLabel <- function(prior) {
  if (inherits(prior, "PriorUnitInformation"))
    return(gettextf("Unit information prior (Units of information = %s)", prior$n0))
  if (is.character(prior) && length(prior) == 1L) {
    return(switch(prior,
      unit_information = gettext("Unit information prior"),
      Jeffreys         = gettext("Jeffreys prior"),
      DCSI             = gettext("DCSI prior"),
      prior
    ))
  }
  gettext("Custom prior")
}

# P(metric > cutoff) for every cutoff, from a qc interval summary whose columns are
# metric, (-Inf, c1], (c1, c2], ..., (ck, Inf).
.bpcsExceedanceProbabilities <- function(intervalSummary, metric, cutoffs) {
  metricRow <- intervalSummary[as.character(intervalSummary$metric) == metric, -1L, drop = FALSE]
  if (nrow(metricRow) != 1L)
    stop(gettextf("The capability metric %s is unavailable.", metric), call. = FALSE)

  intervalProbabilities <- as.numeric(metricRow[1L, ])
  expectedLength <- length(cutoffs) + 1L
  if (length(intervalProbabilities) != expectedLength)
    stop(
      gettextf("Expected %1$d interval probabilities for %2$s, but received %3$d.", expectedLength, metric, length(intervalProbabilities)),
      call. = FALSE
    )

  rev(cumsum(rev(intervalProbabilities[-1L])))
}

.bpcsMetricExceedanceProbabilities <- function(fit, metric, cutoffs) {
  intervals <- summary(fit, interval_probability = cutoffs)[["interval_summary"]]
  .bpcsExceedanceProbabilities(intervals, metric, cutoffs)
}

.bpcsAllExceedanceProbabilities <- function(fit, cutoffs) {
  intervals <- summary(fit, interval_probability = cutoffs)[["interval_summary"]]
  metrics <- .bpcsAllMetrics()
  probabilities <- lapply(stats::setNames(metrics, metrics), .bpcsExceedanceProbabilities,
                          intervalSummary = intervals, cutoffs = cutoffs)
  valid <- vapply(probabilities, function(p) {
    length(p) == length(cutoffs) && all(is.finite(p)) && all(p >= -1e-8 & p <= 1 + 1e-8)
  }, logical(1))
  if (!all(valid))
    stop(gettext("The posterior probabilities are invalid."), call. = FALSE)
  probabilities
}

.bpcsComputeOverviewSequentialData <- function(dataset, options, criteria) {

  sampleSizes <- .bpcsOverviewSampleSizes(nrow(dataset), options)
  metrics <- .bpcsAllMetrics()
  probabilities <- stats::setNames(lapply(metrics, function(.)
    matrix(NA_real_, nrow = length(sampleSizes), ncol = length(criteria$values))), metrics)
  failed <- logical(length(sampleSizes))
  firstError <- NULL
  x <- dataset[[1L]]
  prior <- .bpcsPriorHelper(options)

  jaspBase::startProgressbar(length(sampleSizes), label = gettext("Running process overview"))
  for (i in seq_along(sampleSizes)) {
    # fitting and extracting the probabilities form one checkpoint, it is only stored if both succeed
    checkpoint <- tryCatch(
      .bpcsAllExceedanceProbabilities(.bpcsFit(x[seq_len(sampleSizes[i])], options, prior = prior), criteria$values),
      error = function(e) e
    )

    if (inherits(checkpoint, "error")) {
      failed[i] <- TRUE
      if (is.null(firstError))
        firstError <- list(n = sampleSizes[i], message = .bpcsShortErrorMessage(checkpoint))
    } else {
      for (metricName in metrics)
        probabilities[[metricName]][i, ] <- checkpoint[[metricName]]
    }
    jaspBase::progressbarTick()
  }

  list(
    sampleSizes   = sampleSizes,
    probabilities = probabilities,
    failed        = failed,
    firstError    = firstError
  )
}

# First line of an error message, shortened so it fits in a plot caption.
.bpcsShortErrorMessage <- function(e, maxChars = 150L) {
  lines   <- trimws(strsplit(conditionMessage(e), "\n", fixed = TRUE)[[1L]])
  message <- lines[nzchar(lines)][1L]
  if (is.na(message))
    return(gettext("unknown error"))
  if (nchar(message) > maxChars)
    message <- paste0(substr(message, 1L, maxChars - 3L), "...")
  message
}

# Which sequential update failed first and why, wrapped for a plot caption. Empty without failures.
.bpcsFirstFailureText <- function(firstError, width = 60L) {
  if (is.null(firstError))
    return(character())
  text <- gettextf("First failure at %1$d observations: %2$s", as.integer(firstError$n), firstError$message)
  paste(strwrap(text, width = width), collapse = "\n")
}

.bpcsComputeOverviewReferenceData <- function(dataset, options, fit, criteria) {

  activePrior    <- .bpcsPriorHelper(options)
  referencePrior <- options[["statisticsProcessOverviewSensitivityAnalysisPrior"]]
  activeLabel    <- gettextf("%s (active)", .bpcsPriorLabel(activePrior))
  referenceLabel <- .bpcsPriorLabel(referencePrior)
  metrics        <- .bpcsAllMetrics()

  entries <- stats::setNames(list(fit$rawfit), activeLabel)
  unavailable <- character()

  if (identical(referencePrior, activePrior)) {
    entries[[referenceLabel]] <- fit$rawfit
  } else {
    referenceFit <- tryCatch(.bpcsFit(dataset[[1L]], options, prior = referencePrior), error = function(e) e)
    if (inherits(referenceFit, "error")) {
      unavailable <- gettextf("The Sensitivity analysis prior (%1$s) could not be fitted: %2$s", referenceLabel, conditionMessage(referenceFit))
    } else {
      entries[[referenceLabel]] <- referenceFit
    }
  }

  # threshold-by-prior matrices per metric; cbind keeps the matrix shape when there is only one threshold
  probabilities <- stats::setNames(lapply(metrics, function(.) NULL), metrics)
  densities <- list()
  for (label in names(entries)) {
    result <- tryCatch(list(
      probabilities = .bpcsAllExceedanceProbabilities(entries[[label]], criteria$values),
      density       = qc::extract_density_data(entries[[label]], what = metrics)
    ), error = function(e) e)

    if (inherits(result, "error")) {
      if (identical(label, activeLabel))
        stop(result)
      unavailable <- c(unavailable, gettextf("Results for Sensitivity analysis prior (%1$s) could not be computed: %2$s", label, conditionMessage(result)))
      next
    }

    for (metricName in metrics)
      probabilities[[metricName]] <- cbind(
        probabilities[[metricName]],
        matrix(result$probabilities[[metricName]], ncol = 1L, dimnames = list(NULL, label))
      )

    density <- as.data.frame(result$density)
    density$metric <- as.character(density$metric)
    density$prior <- label
    densities[[label]] <- density
  }

  list(
    probabilities = probabilities,
    densities     = do.call(rbind, unname(densities)),
    unavailable   = paste(unavailable, collapse = "\n")
  )
}

.bpcsUnavailablePanel <- function(title, message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 5) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18))
}

.bpcsTimeSeriesLabels <- function() {
  ggplot2::labs(x = gettext("Observation number"), y = gettext("Measurements"))
}

.bpcsMakeProcessOverviewPlot <- function(dataset, options, fit, priorFit, data, criteria) {

  metric         <- options[["statisticsProcessOverviewCapabilityMetric"]]
  thresholdIndex <- .bpcsOverviewThresholdIndex(options, criteria)
  threshold      <- criteria$values[thresholdIndex]
  regionColors   <- .bpcsOverviewRegionColors(criteria)
  rawData        <- dataset[[1L]]

  timeSeriesPlot <- qc::plot_time_series(
    rawData,
    LSL = options[["capabilitySpecificationLowerLimit"]],
    target = options[["capabilitySpecificationTargetValue"]],
    USL = options[["capabilitySpecificationUpperLimit"]]
  ) +
    .bpcsTimeSeriesLabels() +
    ggplot2::labs(title = gettext("A. Time series")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  densityPlot <- qc::plot_density(
    fit$summaryObject,
    what = metric,
    point_estimate = "none",
    ci = "none",
    single_panel = TRUE,
    show_regions = TRUE,
    textsize = 8,
    region_cutoffs = criteria$values,
    region_colors = regionColors
  )

  densityPlot <- .bpcsLabelProcessCriteria(densityPlot, criteria)

  if (.bpcsPriorIsUsable(priorFit)) {
    priorDensity <- qc::extract_density_data(priorFit$summaryObject, what = metric)
    densityPlot <- densityPlot +
      ggplot2::geom_line(
        data = data.frame(x = priorDensity$x, y = priorDensity$density, type = "prior"),
        mapping = ggplot2::aes(x = .data$x, y = .data$y, linetype = .data$type),
        inherit.aes = FALSE
      ) +
      ggplot2::scale_linetype_manual(
        name = NULL,
        values = c(prior = "dotdash"),
        labels = c(prior = gettext("Prior"))
      )
  }

  densityPlot <- densityPlot +
    ggplot2::labs(title = gettext("B. Prior and posterior distributions"), x = metric, y = gettext("Density"),
                  caption = priorFit[["error"]]) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  overTimeTitle <- gettextf("C. Monitoring P(%1$s > %2$g | data) sequentially", metric, threshold)
  overTimePlot <- .bpcsMakeOverviewSequentialPanel(data$sequential, metric, thresholdIndex, priorFit, criteria, overTimeTitle)

  sensitivityPlot <- .bpcsMakeOverviewSensitivityPanel(data$sensitivity, metric, thresholdIndex, threshold, regionColors)

  panels <- lapply(list(timeSeriesPlot, densityPlot, overTimePlot, sensitivityPlot), function(panel) {
    panel + ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
  })
  jaspGraphs:::jaspGraphsPlot$new(
    subplots = panels,
    layout   = matrix(1:4, ncol = 2, byrow = TRUE),
    widths   = c(1, 1),
    heights  = c(1, 1)
  )
}

.bpcsMakeOverviewSequentialPanel <- function(sequential, metric, thresholdIndex, priorFit, criteria, title) {

  failed <- sequential$failed
  nFailed <- sum(failed)
  nTotal <- length(failed)
  failureText <- .bpcsFirstFailureText(sequential$firstError)
  if (nFailed / nTotal > 0.1)
    return(.bpcsUnavailablePanel(title, paste(c(gettextf(
      "%1$d of %2$d sequential updates failed (more than 10%%),\nso the sequential probabilities cannot be shown.",
      nFailed, nTotal
    ), failureText), collapse = "\n\n")))

  overTimeData <- data.frame(
    observation = sequential$sampleSizes,
    probability = sequential$probabilities[[metric]][, thresholdIndex]
  )[!failed, , drop = FALSE]
  # consecutive successful checkpoints share a segment, so lines never bridge a failed update
  overTimeData$segment <- cumsum(c(TRUE, diff(which(!failed)) != 1L))

  captions <- character()
  if (nFailed > 0L)
    captions <- c(captions, gettextf("%1$d of %2$d sequential updates failed and are omitted.", nFailed, nTotal), failureText)
  if (failed[nTotal])
    captions <- c(captions, gettext("The update with all observations failed, so the final probability is unavailable."))

  priorProbability <- NA_real_
  if (.bpcsPriorIsUsable(priorFit))
    priorProbability <- tryCatch(
      .bpcsMetricExceedanceProbabilities(priorFit$rawfit, metric, criteria$values)[thresholdIndex],
      error = function(e) NA_real_
    )
  priorLine <- if (is.finite(priorProbability))
    ggplot2::geom_hline(yintercept = priorProbability, linetype = "dotdash")

  ggplot2::ggplot(overTimeData, ggplot2::aes(x = .data$observation, y = .data$probability, group = .data$segment)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 3, colour = "black", fill = "grey", shape = 21) +
    priorLine +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = title,
      x = gettext("Number of observations"), y = gettext("Posterior probability"),
      caption = if (length(captions) > 0L) paste(captions, collapse = "\n")
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
}

.bpcsMakeOverviewSensitivityPanel <- function(sensitivity, metric, thresholdIndex, threshold, regionColors) {

  probabilities <- sensitivity$probabilities[[metric]]
  priorLabels <- colnames(probabilities)
  sensitivityDensity <- sensitivity$densities[sensitivity$densities$metric == metric, , drop = FALSE]

  sensitivityDensity$region <- ifelse(
    sensitivityDensity$x <= threshold,
    gettext("Below threshold"),
    gettext("Above threshold")
  )
  sensitivityProbabilityLabels <- sprintf(
    "%s\nP(%s > %g | data) = %.2f",
    priorLabels, metric, threshold,
    probabilities[thresholdIndex, ]
  )
  sensitivityDensity$prior <- factor(
    sensitivityProbabilityLabels[match(sensitivityDensity$prior, priorLabels)],
    levels = sensitivityProbabilityLabels
  )

  ggplot2::ggplot(
    sensitivityDensity,
    ggplot2::aes(x = .data$x, y = .data$density, group = interaction(.data$prior, .data$region), fill = .data$region)
  ) +
    ggplot2::geom_area(alpha = 0.7, color = NA) +
    ggplot2::geom_line(ggplot2::aes(group = .data$prior), color = "grey20", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed") +
    ggplot2::scale_fill_manual(
      values = stats::setNames(
        c(regionColors[[thresholdIndex]], regionColors[[thresholdIndex + 1L]]),
        c(gettext("Below threshold"), gettext("Above threshold")))
    ) +
    ggplot2::facet_wrap(~prior) +
    ggplot2::labs(
      title = gettext("D. Sensitivity analysis"),
      x = metric, y = gettext("Posterior density"), fill = NULL,
      caption = if (nzchar(sensitivity$unavailable)) sensitivity$unavailable
    ) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 14),
      strip.text = ggplot2::element_text(size = 14)
    )
}

.bpcsTimeSeriesPlot <- function(jaspResults, dataset, options, position) {

  base <- "timeSeriesPlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = gettext("Time Series Plot"), width = 600, height = 400,
    position = position,
    dependencies = jaspDeps(c(
      base, "measurement",
      "capabilitySpecificationLowerLimit", "capabilitySpecificationTargetValue", "capabilitySpecificationUpperLimit"
    ))
  )
  jaspResults[[base]] <- plot

  if (is.null(dataset) || ncol(dataset) == 0L || jaspResults$getError())
    return()

  tryCatch({
    plot$plotObject <- qc::plot_time_series(
      dataset[[1L]],
      LSL = if (options[["capabilitySpecificationLowerLimit"]] != "") options[["capabilitySpecificationLowerLimit"]] else NULL,
      target = if (options[["capabilitySpecificationTargetValue"]] != "") options[["capabilitySpecificationTargetValue"]] else NULL,
      USL = if (options[["capabilitySpecificationUpperLimit"]] != "") options[["capabilitySpecificationUpperLimit"]] else NULL
    ) +
      .bpcsTimeSeriesLabels() +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }, error = function(e) {
    plot$setError(gettextf("Unexpected error in Time series plot: %s", e$message))
  })
}

.bpcsCapabilityPlot <- function(jaspResults, options, fit, priorFit, position, base = "priorAndPosteriorInferencePosteriorDistribution") {

  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  # content layout (facetted vs combined into one panel), set by the "Layout" radio button
  singlePanel     <- options[["priorAndPosteriorInferenceLayout"]] != "multiplePanels"
  # Match facet_wrap's grid; a combined plot always has one panel.
  panelLayout     <- rev(grDevices::n2mfrow(if (singlePanel) 1L else max(1L, length(selectedMetrics))))

  isPost <- base == "priorAndPosteriorInferencePosteriorDistribution"
  summaryObject <- if (isPost) fit$summaryObject else priorFit$summaryObject
  # only if the user asked for it and the prior is available
  priorSummaryObject <- if (isPost && options[[paste0(base, "ShowPriorDistribution")]] && .bpcsPriorIsUsable(priorFit)) priorFit$summaryObject else NULL

  plotWidth <- 600 * panelLayout[2L]

  jaspPlt <- createJaspPlot(
    title = if (isPost) gettext("Posterior Distribution") else gettext("Prior Distribution"),
    width  = plotWidth,
    height = 400 * panelLayout[1L],
    position = position,
    dependencies = jaspDeps(
      options = c(
        .bpcsDefaultDeps(options, prior = !isPost || options[[paste0(base, "ShowPriorDistribution")]]),
        .bpcsPlotLayoutDeps(base, hasPrior = base == "priorAndPosteriorInferencePosteriorDistribution"),
        .bpcsPlotLayoutAxesDeps("priorAndPosteriorInference"),
        "priorAndPosteriorInferenceShowProcessCriteria", .bpcsProcessCriteriaDeps()
      )
    )
  )
  jaspResults[[base]] <- jaspPlt

  ready <- if (isPost) .bpcsIsReady(options) else .bpcsSpecificationsReady(options)
  if (!ready || (isPost && is.null(fit)) || (!isPost && is.null(priorFit)))
    return()

  if (!isPost && !is.null(priorFit[["error"]])) {
    jaspPlt$width <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(priorFit[["error"]])
    return()
  }

  tryCatch({

    if (length(selectedMetrics) == 0) {
      NULL
    } else {

      # qc draws the point estimate / ci annotation with ggtext::geom_richtext(size = ...),
      # whose size is in mm and defaults to 18 (~51pt), swamping the panel. Scale it to the
      # width one panel actually gets instead: 3mm is the largest that keeps the longest
      # label ("Mean = x.xxx; xx.x% CI [x.xxx, x.xxx]") inside a 400px panel. Never grow past
      # the standard jasp font size.
      panelWidth     <- plotWidth / panelLayout[2L]
      annotationSize <- min(3 * panelWidth / 400, jaspGraphs::graphOptions("fontsize") / ggplot2::.pt)

      # the regions follow the user's process criteria; only parsed when they are shown, so invalid
      # criteria do not break the plot otherwise (qc's defaults are unused while show_regions is FALSE)
      showRegions <- options[["priorAndPosteriorInferenceShowProcessCriteria"]]
      criteria    <- if (showRegions) .bpcsProcessCriteria(options)

      densityPlot <- qc::plot_density(
        summaryObject,
        what = selectedMetrics,
        point_estimate     = if (options[[paste0(base, "PointEstimate")]]) options[[paste0(base, "PointEstimateType")]] else "none",
        ci                 = if (options[[paste0(base, "Ci")]])            options[[paste0(base, "CiType")]]            else "none",
        ci_level           = options[[paste0(base, "CiMass")]],
        ci_custom_left     = options[[paste0(base, "CiLower")]],
        ci_custom_right    = options[[paste0(base, "CiUpper")]],
        single_panel       = singlePanel,
        axes               = options[["priorAndPosteriorInferenceAxes"]],
        axes_custom        = .bpcsGetCustomAxisLimits(options, "priorAndPosteriorInference"),
        priorSummaryObject = priorSummaryObject,
        textsize           = annotationSize,
        colorScheme        = if (singlePanel && length(selectedMetrics) > 1L) grDevices::hcl.colors(length(selectedMetrics), "Dark 3") else NULL,
        show_regions       = showRegions,
        region_cutoffs     = if (showRegions) criteria$values                    else qc::default_region_cutoffs(),
        region_colors      = if (showRegions) .bpcsOverviewRegionColors(criteria) else qc::default_region_colors()
      )

      if (showRegions)
        densityPlot <- .bpcsLabelProcessCriteria(densityPlot, criteria)

      # Combined plots need metric colours; facetted plots already name each metric.
      # Retain the prior/posterior linetype legend in either layout.
      for (i in seq_along(densityPlot$layers)) {
        if (inherits(densityPlot$layers[[i]]$geom, "GeomLine"))
          densityPlot$layers[[i]]$show.legend <- c(colour = singlePanel && length(selectedMetrics) > 1L, linetype = TRUE)
      }

      # qc only facets (and so only titles the panel with the metric name) when more than one
      # metric is shown; give a single metric a matching title of its own.
      metricTitle <- if (length(selectedMetrics) == 1) selectedMetrics else NULL

      jaspPlt$plotObject <- densityPlot +
        ggplot2::labs(title = metricTitle, fill = gettext("Region"),
                     caption = if (isPost && options[[paste0(base, "ShowPriorDistribution")]]) priorFit[["error"]]) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw(legend.position = "right") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    }
  }, error = function(e) {
    jaspPlt$width  <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(
      if (isPost) gettextf("Unexpected error in Posterior distribution: %s", e$message)
      else gettextf("Unexpected error in Prior distribution: %s", e$message)
    )
  })

}

.bpcsSequentialPointEstimatePlot <- function(jaspResults, dataset, options, fit, position) {

  base <- "sequentialAnalysisPointEstimatePlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()
  if (!options[[paste0(base, "PointEstimate")]] && !options[[paste0(base, "Ci")]])
    return()

  # plot size only shrinks when there is a single metric to show; combining several metrics
  # into one panel (or facetting several) both need the larger canvas
  smallPlot <- length(.bpcsGetSelectedMetrics(options)) <= 1
  plt <- createJaspPlot(title = gettext("Sequential Analysis Point Estimate"),
                        width  = if (smallPlot) 600 else 1200,
                        height = if (smallPlot) 400 else 800,
                        position = position,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(options),
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE),
                          .bpcsPlotLayoutAxesDeps("sequentialAnalysis"),
                          "sequentialAnalysisShowProcessCriteria",
                          if (options[["sequentialAnalysisShowProcessCriteria"]]) .bpcsProcessCriteriaDeps()
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit, custom = base == "sequentialAnalysisIntervalEstimatePlot")

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base)
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in Point estimate plot (Sequential Analysis): %s", e$message))
    }
    )
  }
}

.bpcsSequentialIntervalEstimatePlot <- function(jaspResults, dataset, options, fit, position) {

  base <- "sequentialAnalysisIntervalEstimatePlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis Interval Estimate"), width = 3*w, height = 2*w,
                        position = position,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(options),
                          # mirrors the flags set on this plot's Common.PlotLayout in the qml
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE, hasEstimate = FALSE, hasCi = FALSE, hasInterval = TRUE),
                          .bpcsPlotLayoutAxesDeps("sequentialAnalysis")
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit, custom = base == "sequentialAnalysisIntervalEstimatePlot")

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base, custom = TRUE)
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in Interval estimate plot (Sequential Analysis): %s", e$message))
    }
    )
  }
}

# Model fits survive changes to interval presentation; only their summaries are rebuilt.
.bpcsGetSequentialAnalysis <- function(jaspResults, dataset, options, fit, custom) {
  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  tryCatch({
    sequentialFits <- jaspResults[["SequentialAnalysisFits"]] %setOrRetrieve% (
      .bpcsFitSequentialAnalysis(dataset, options) |>
        createJaspState(dependencies = jaspDeps(.bpcsStateDeps(options)))
    )
    summaryKey <- if (custom) "SequentialIntervalData" else "SequentialPointData"
    pointBase <- "sequentialAnalysisPointEstimatePlot"
    summaryDeps <- if (custom) .bpcsPlotLayoutIntervalDeps("sequentialAnalysisIntervalEstimatePlot") else
      c(paste0(pointBase, "Ci"), if (options[[paste0(pointBase, "Ci")]])
        paste0(pointBase, c("CiType", if (options[[paste0(pointBase, "CiType")]] == "custom") c("CiLower", "CiUpper") else "CiMass")))
    sequentialPlotData <- jaspResults[[summaryKey]] %setOrRetrieve% (
      .bpcsComputeSequentialAnalysis(sequentialFits, options, custom) |>
        createJaspState(dependencies = jaspDeps(c(.bpcsStateDeps(options), summaryDeps)))
    )
    list(data = sequentialPlotData, error = NULL)
  }, error = function(e) {
    list(data = NULL, error = e$message)
  })
}

.bpcsFitSequentialAnalysis <- function(dataset, options) {
  n <- nrow(dataset)
  if (!is.finite(n) || n < 3L)
    stop(gettext("Sequential Analysis requires at least 3 observations."), call. = FALSE)

  nseq <- seq.int(3L, n)
  fits <- vector("list", length(nseq))
  x <- dataset[[1L]]
  prior <- .bpcsPriorHelper(options)
  nFailed <- 0L
  firstError <- NULL

  jaspBase::startProgressbar(length(nseq), label = gettext("Running sequential analysis"))
  for (i in seq_along(nseq)) {
    fits[[i]] <- tryCatch(.bpcsFit(x[seq_len(nseq[i])], options, prior = prior), error = function(e) e)
    if (inherits(fits[[i]], "error")) {
      nFailed <- nFailed + 1L
      if (is.null(firstError))
        firstError <- list(n = nseq[i], message = .bpcsShortErrorMessage(fits[[i]]))
    } else {
      # Metric draws support all interval queries; the full Stan sampler is not needed.
      fits[[i]]$stanfit <- NULL
    }
    jaspBase::progressbarTick()
  }
  if (nFailed > 0L && nFailed / length(nseq) > 0.1) {
    stop(paste(
      gettextf("%1$d of %2$d sequential fits failed (%3$.0f%%). Cannot render plot.",
               nFailed, length(nseq), 100 * nFailed / length(nseq)),
      .bpcsFirstFailureText(firstError, width = 1000L)
    ), call. = FALSE)
  }
  list(fits = fits, nseq = nseq)
}

.bpcsSequentialSummary <- function(fit, options, custom) {
  metrics <- .bpcsAllMetrics()
  distributions <- qc:::.coerce_qc_metric_distributions(fit, what = metrics)
  data <- data.frame(metric = metrics, mean = NA_real_, median = NA_real_,
                     lower = NA_real_, upper = NA_real_, custom = NA_real_)
  if (custom) {
    data$custom <- vapply(distributions, qc:::.qc_metric_distribution_probability, numeric(1),
      lower = options[["sequentialAnalysisIntervalEstimatePlotIntervalLower"]],
      upper = options[["sequentialAnalysisIntervalEstimatePlotIntervalUpper"]])
    return(data)
  }
  base <- "sequentialAnalysisPointEstimatePlot"
  for (i in seq_along(distributions)) {
    stats <- qc:::.qc_metric_distribution_stats(distributions[[i]])
    data$mean[i] <- stats["Mean"]
    data$median[i] <- stats["Median"]
  }
  if (options[[paste0(base, "Ci")]]) {
    ciType <- options[[paste0(base, "CiType")]]
    intervals <- qc:::extract_ci_data(fit, what = metrics, ci = ciType,
      ci_level = options[[paste0(base, "CiMass")]],
      ci_custom_left = options[[paste0(base, "CiLower")]],
      ci_custom_right = options[[paste0(base, "CiUpper")]],
      dfDensity = qc::extract_density_data(fit, what = metrics))$dfCi
    order <- match(metrics, intervals$metric)
    data$lower <- intervals$xmin[order]
    data$upper <- intervals$xmax[order]
  }
  data
}

.bpcsComputeSequentialAnalysis <- function(sequentialFits, options, custom) {
  metrics <- .bpcsAllMetrics()
  keys <- c("mean", "median", "lower", "upper", "custom")
  estimates <- array(NA_real_, c(length(metrics), length(keys), length(sequentialFits$nseq)),
                     dimnames = list(metrics, keys, NULL))
  failed <- logical(length(sequentialFits$fits))
  firstError <- NULL
  for (i in seq_along(sequentialFits$fits)) {
    fit <- sequentialFits$fits[[i]]
    data <- if (inherits(fit, "error")) fit else
      tryCatch(.bpcsSequentialSummary(fit, options, custom), error = function(e) e)
    if (inherits(data, "error")) {
      failed[i] <- TRUE
      if (is.null(firstError)) firstError <- list(n = sequentialFits$nseq[i], message = .bpcsShortErrorMessage(data))
    } else estimates[, , i] <- as.matrix(data[match(metrics, data$metric), keys])
  }
  if (mean(failed) > 0.1)
    stop(paste(gettextf("%1$d of %2$d sequential updates failed (more than 10%%). Cannot render plot.", sum(failed), length(failed)),
               .bpcsFirstFailureText(firstError)), call. = FALSE)
  captions <- character()
  if (any(failed)) captions <- c(gettextf("%1$d of %2$d sequential updates failed and are omitted.", sum(failed), length(failed)), .bpcsFirstFailureText(firstError))
  if (tail(failed, 1L)) captions <- c(captions, gettext("The update with all observations failed, so the final estimate is unavailable."))
  attr(estimates, "nseq") <- sequentialFits$nseq
  attr(estimates, "failed") <- failed
  attr(estimates, "caption") <- if (length(captions)) paste(captions, collapse = "\n") else NULL
  estimates
}
# Secondary y axis that names the process criteria regions: alternating (unlabelled) ticks at the
# region boundaries and labels halfway between them, with the open-ended outer regions placed
# halfway between the outermost boundary and the axis limit.
.bpcsSequentialCriteriaAxis <- function(leftLimits, gridLines, categoryNames) {
  nGrid <- length(gridLines)
  innerPositions <- if (nGrid > 1L) (gridLines[-1L] + gridLines[-nGrid]) / 2 else numeric()
  rightBreaksShown <- c(
    (leftLimits[1L] + gridLines[1L]) / 2,
    innerPositions,
    (leftLimits[2L] + gridLines[nGrid]) / 2
  )
  rightBreaks <- numeric(2L*length(rightBreaksShown) + 1L)
  rightBreaks[1L]                                 <- leftLimits[1L]
  rightBreaks[seq(2, length(rightBreaks), 2)]     <- rightBreaksShown
  rightBreaks[seq(3, length(rightBreaks) - 2, 2)] <- gridLines
  rightBreaks[length(rightBreaks)]                <- leftLimits[2L]

  rightLabels <- character(length(rightBreaks))
  rightLabels[seq(2, length(rightLabels), 2)]   <- categoryNames
  ggplot2::sec_axis(identity, breaks = rightBreaks, labels = rightLabels)
}

.bpcsSequentialYScale <- function(plotted, gridLines, categoryNames, custom, addInfo) {
  if (custom) {
    # probabilities, so the axis is fixed and unrelated to the process criteria
    leftBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  } else {
    observedRange <- range(plotted, na.rm = TRUE)
    if (!all(is.finite(observedRange))) {
      observedRange <- c(0, 1)
    }
    dist <- observedRange[2L] - observedRange[1L]

    if (length(gridLines)) {
      observedRange[1L] <- min(observedRange[1L], gridLines[1L] - 0.1 * dist)
      observedRange[2L] <- max(observedRange[2L], gridLines[length(gridLines)] + 0.1 * dist)
    }

    leftBreaks <- jaspGraphs::getPrettyAxisBreaks(observedRange)
  }
  leftLimits <- range(leftBreaks)

  rightAxis <- if (addInfo) .bpcsSequentialCriteriaAxis(leftLimits, gridLines, categoryNames) else ggplot2::waiver()

  ggplot2::scale_y_continuous(breaks = leftBreaks, limits = leftLimits,
                              minor_breaks = if (custom) ggplot2::waiver() else gridLines,
                              sec.axis = rightAxis)
}

.bpcsMakeSequentialPlot <- function(estimates, options, base, custom = FALSE) {

  singlePanel <- options[["sequentialAnalysisLayout"]] != "multiplePanels"
  axes         <- options[["sequentialAnalysisAxes"]]
  axesCustom  <- .bpcsGetCustomAxisLimits(options, "sequentialAnalysis")

  hasPointEstimate <- custom || options[[paste0(base, "PointEstimate")]]
  pointEstimateName <- if (custom) "custom" else options[[paste0(base, "PointEstimateType")]]
  showProcessCriteria <- options[["sequentialAnalysisShowProcessCriteria"]]

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  if (length(selectedMetrics) == 0L)
    return(NULL)

  hasCi <- !custom && options[[paste0(base, "Ci")]]
  if (!hasPointEstimate && !hasCi)
    return(NULL)

  if (custom) {
    hasCi <- FALSE
    pointEstimateName <- "custom"
    showProcessCriteria <- FALSE
    yTitle <- gettextf("P(%1$.3f \u2264 %2$s \u2264 %3$.3f)",
                       options[["sequentialAnalysisIntervalEstimatePlotIntervalLower"]],
                       if (length(selectedMetrics) == 1L) selectedMetrics else gettext("Metric"),
                       options[["sequentialAnalysisIntervalEstimatePlotIntervalUpper"]])
    gridLines <- numeric()
    categoryNames <- character()
  } else {

    yTitle <- if (hasPointEstimate) {
      switch(pointEstimateName, mean = gettext("Mean"), median = gettext("Median"))
    } else gettext("Capability metric")
    intervalLabel <- NULL
    if (hasCi) {
      intervalLabel <- switch(options[[paste0(base, "CiType")]],
        custom = gettextf("Custom interval: [%1$.3f, %2$.3f]", options[[paste0(base, "CiLower")]], options[[paste0(base, "CiUpper")]]),
        HPD = bquote(.(gettextf("%s%%", 100 * options[[paste0(base, "CiMass")]]))~CI[HPD]),
        central = gettextf("%s%% CI", 100 * options[[paste0(base, "CiMass")]])
      )
    }

    criteria <- if (showProcessCriteria) .bpcsProcessCriteria(options)
    categoryNames <- if (showProcessCriteria) criteria$labels else character()
    gridLines <- if (showProcessCriteria) criteria$values else numeric()
  }

  # Keep the summary state as a compact array; construct plotting data only here.

  nseq <- attr(estimates, "nseq")

  tb <- tibble::tibble(
    metric = factor(rep(rownames(estimates), times = length(nseq))),
    n      = rep(nseq, each = nrow(estimates)),
    mean   = as.vector(estimates[, pointEstimateName, ]),
    lower  = as.vector(estimates[, "lower", ]),
    upper  = as.vector(estimates[, "upper", ]),
  )
  tb <- tb[tb$metric %in% selectedMetrics, , drop = FALSE]
  failed <- attr(estimates, "failed")
  segments <- cumsum(failed)
  tb$segment <- segments[match(tb$n, nseq)]
  tb <- tb[!failed[match(tb$n, nseq)], , drop = FALSE]
  if (length(selectedMetrics) == 1L)
    singlePanel <- TRUE

  plottedValues <- function(data) {
    c(if (hasPointEstimate) data$mean, if (hasCi) c(data$lower, data$upper))
  }
  sharedYScale <- .bpcsSequentialYScale(plottedValues(tb), gridLines, categoryNames, custom, showProcessCriteria)
  if (!singlePanel && axes %in% c("automatic", "free")) {
    scalesPerMetric <- lapply(split(tb, tb$metric, drop = TRUE), function(data) {
      .bpcsSequentialYScale(plottedValues(data), gridLines, categoryNames, custom, showProcessCriteria)
    })
    facetScale <- ggh4x::facetted_pos_scales(y = scalesPerMetric)
  } else {
    facetScale <- sharedYScale
  }

  ribbon <- NULL
  if (hasCi)
    ribbon <- ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.3)

  extraTheme <- gridLinesLayer <- NULL
  sides <- "bl"
  if (showProcessCriteria) {
    # the outermost ticks are hidden (NA) because one of their bounds is infinite. The inner ticks
    # alternate between NA and black, so there is a tick at the grid lines but no tick at the
    # criteria text (which is secretly an axis tick label).
    rightTickColors <- c(NA, rep(c(NA, "black"), length.out = 2L * length(gridLines) + 1L), NA)
    extraTheme <- ggplot2::theme(axis.ticks.y.right = ggplot2::element_line(colour = rightTickColors))
    sides      <- "blr"
    # I tried using minor.breaks for this, but these are not drawn properly with facet_grid and facetted_pos_scales
    gridLinesLayer <- ggplot2::geom_hline(
      data = data.frame(yintercept = gridLines),
      ggplot2::aes(yintercept = .data$yintercept),
      linewidth = .5, color = "lightgray", linetype = "dashed"
    )

  }

  xLimits <- range(nseq)
  if (axes == "custom") {
    xLimits <- c(axesCustom[["xmin"]], axesCustom[["xmax"]])
    leftLimits <- c(axesCustom[["ymin"]], axesCustom[["ymax"]])
    rightAxis <- if (showProcessCriteria) .bpcsSequentialCriteriaAxis(leftLimits, gridLines, categoryNames) else ggplot2::waiver()
    facetScale <- ggplot2::scale_y_continuous(
      breaks = jaspGraphs::getPrettyAxisBreaks(leftLimits), limits = leftLimits,
      minor_breaks = if (custom) ggplot2::waiver() else gridLines, sec.axis = rightAxis
    )
  }
  xScale <- ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(xLimits), limits = xLimits)
  facet <- if (!singlePanel) ggplot2::facet_wrap(~metric, scales = if (axes %in% c("automatic", "free")) "free_y" else "fixed")

  # a single metric has no facet strip to say which one it is, and doesn't need a legend either
  singleMetricTitle <- if (length(selectedMetrics) == 1L) as.character(selectedMetrics) else NULL
  showLegend        <- singlePanel && length(selectedMetrics) > 1L

  ggplot2::ggplot(tb, ggplot2::aes(x = .data$n, y = .data$mean, group = interaction(.data$metric, .data$segment),
                                   color = .data$metric, fill = .data$metric)) +
    gridLinesLayer +
    ribbon +
    (if (hasPointEstimate) ggplot2::geom_line(linewidth = 1)) +
    facet + facetScale + xScale +
    ggplot2::labs(
      title = singleMetricTitle,
      subtitle = if (!custom) intervalLabel,
      caption = attr(estimates, "caption"),
      x     = gettext("Number of observations"),
      y     = yTitle,
      color = gettext("Metric"),
      fill  = gettext("Metric")
    ) +
    jaspGraphs::geom_rangeframe(sides = sides) +
    jaspGraphs::themeJaspRaw(legend.position = if (showLegend) "right" else "none") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, size = jaspGraphs::graphOptions("fontsize"))) +
    extraTheme

}

# Additional plot functions ----
.bpcsPlotPredictive <- function(jaspResults, dataset, options, fit, position, base = c("priorAndPosteriorPredictivePlotsPosteriorPredictiveDistribution", "priorAndPosteriorPredictivePlotsPriorPredictiveDistribution")) {

  base <- match.arg(base)
  isPrior <- base == "priorAndPosteriorPredictivePlotsPriorPredictiveDistribution"

  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = if (isPrior) gettext("Prior Predictive Distribution") else gettext("Posterior Predictive Distribution"),
    width = 600,
    height = 400,
    position = position,
    dependencies = c(
    .bpcsDefaultDeps(options, prior = isPrior), .bpcsMcmcDeps(),
    base,
    paste0(base, "PointEstimate"),
    paste0(base, "PointEstimateType"),
    paste0(base, "Ci"),
    paste0(base, "CiType"),
    paste0(base, "CiMass"),
    paste0(base, "CiLower"),
    paste0(base, "CiUpper")
  ))

  jaspResults[[base]] <- plot

  ready <- if (isPrior) .bpcsSpecificationsReady(options) else .bpcsIsReady(options)
  if (!ready || is.null(fit) || jaspResults$getError()) return()

  if (!is.null(fit[["error"]])) {
    plot$setError(fit[["error"]])
    return()
  }

  tryCatch({
    predictive <- .bpcsPredictiveSamples(jaspResults, dataset, options, fit, base)
    if (!is.null(predictive$error)) stop(predictive$error, call. = FALSE)
    plot$plotObject <- .bpcsMakePredictivePlot(predictive$samples, options, base)
  }, error = function(e) {
    plot$setError(
      if (isPrior) gettextf("Unexpected error in Prior predictive distribution: %s", e$message)
      else gettextf("Unexpected error in Posterior predictive distribution: %s", e$message)
    )
  })
}

.bpcsPredictiveSamples <- function(jaspResults, dataset, options, fit, base) {
  key <- paste0(base, "Samples")
  if (!is.null(jaspResults[[key]])) return(jaspResults[[key]]$object)
  isPrior <- base == "priorAndPosteriorPredictivePlotsPriorPredictiveDistribution"
  state <- createJaspState()
  jaspResults[[key]] <- state
  fallback <- FALSE
  mcmcFit <- NULL
  state$object <- tryCatch({
    if (!is.null(fit$error)) stop(fit$error, call. = FALSE)
    samples <- tryCatch({
      jaspBase::.setSeedJASP(options)
      qc::extract_predictive_samples(fit$rawfit)
    }, error = function(e) {
      if (fit$rawfit$method != "integration") stop(e)
      fallback <<- TRUE
      x <- if (!isPrior && !is.null(dataset) && ncol(dataset) > 0L) dataset[[1L]] else NULL
      mcmcFit <<- .bpcsFit(x, options, prior = fit$rawfit$prior,
                         samplePriors = isPrior, method = "mcmc")
      jaspBase::.setSeedJASP(options)
      qc::extract_predictive_samples(mcmcFit)
    })
    list(samples = samples, fallback = fallback, mcmcFit = mcmcFit, error = NULL)
  }, error = function(e) list(samples = NULL, fallback = fallback, mcmcFit = mcmcFit, error = conditionMessage(e)))
  state$dependOn(.bpcsStateDeps(options, prior = isPrior,
    mcmc = fallback || options[["advancedEstimationMethod"]] == "mcmc"))
  state$object
}

.bpcsMakePredictivePlot <- function(predictiveSamples, options, base) {
  plt <- jaspGraphs::jaspHistogram(
    predictiveSamples,
    xName = gettext("Future data"),
    density = TRUE
  )

  # Calculate density for positioning elements above histogram
  dens <- stats::density(predictiveSamples)
  maxDensity <- max(dens$y)
  pointLabel <- intervalLabel <- NULL

  # Add point estimate if requested
  if (options[[paste0(base, "PointEstimate")]]) {
    pointEstimateType <- options[[paste0(base, "PointEstimateType")]]
    pointEstimate <- switch(pointEstimateType,
      "mean"   = mean(predictiveSamples),
      "median" = stats::median(predictiveSamples),
      "mode"   = dens$x[which.max(dens$y)]
    )
    pointLabel <- gettextf("%1$s = %2$.3f",
      switch(pointEstimateType, mean = gettext("Mean"), median = gettext("Median"), mode = gettext("Mode")),
      pointEstimate)
    plt <- plt + ggplot2::geom_point(
      data = data.frame(x = pointEstimate, y = 0),
      ggplot2::aes(x = .data$x, y = .data$y),
      size = 3,
      inherit.aes = FALSE
    )
  }

  # Add CI if requested
  if (options[[paste0(base, "Ci")]]) {
    ciType <- options[[paste0(base, "CiType")]]

    ciInterval <- if (ciType == "custom") {
      c(options[[paste0(base, "CiLower")]],
        options[[paste0(base, "CiUpper")]])
    } else {
      ciMass <- options[[paste0(base, "CiMass")]]
      if (ciType == "central") {
        stats::quantile(predictiveSamples, probs = c((1 - ciMass) / 2, (1 + ciMass) / 2))
      } else if (ciType == "HPD") {
        HDInterval::hdi(predictiveSamples, credMass = ciMass)
      }
    }

    boundsLabel <- sprintf("[%.3f, %.3f]", ciInterval[1], ciInterval[2])
    intervalLabel <- switch(ciType,
      custom = gettextf("P(%1$.3f \u2264 x \u2264 %2$.3f) = %3$.1f%%",
        ciInterval[1], ciInterval[2],
        100 * mean(predictiveSamples >= ciInterval[1] & predictiveSamples <= ciInterval[2])),
      central = gettextf("%1$s%% CI: %2$s", format(100 * ciMass, trim = TRUE), boundsLabel),
      HPD = bquote(.(gettextf("%s%%", format(100 * ciMass, trim = TRUE)))~CI[HPD]*": "*.(boundsLabel))
    )

    # Position errorbar above both the histogram and its density estimate.
    histogramHeight <- max(ggplot2::ggplot_build(plt)$data[[1]]$y, na.rm = TRUE)
    maxDensity <- max(maxDensity, histogramHeight)
    yPosition <- maxDensity * 1.1
    plt <- plt + ggplot2::geom_errorbarh(
      data = data.frame(x = mean(ciInterval), xmin = ciInterval[1], xmax = ciInterval[2], y = yPosition),
      ggplot2::aes(x = .data$x, xmin = .data$xmin, xmax = .data$xmax, y = .data$y),
      height = maxDensity * 0.05,
      linewidth = 0.75,
      inherit.aes = FALSE
    )
  }

  annotation <- if (is.null(pointLabel)) intervalLabel else if (is.null(intervalLabel)) pointLabel else
    bquote(.(pointLabel)*"; "*.(intervalLabel))
  plt + ggplot2::labs(subtitle = annotation) +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5, size = jaspGraphs::graphOptions("fontsize")))
}
