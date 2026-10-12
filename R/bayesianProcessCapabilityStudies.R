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

  fit <- .bpcsCapabilityTable(jaspResults, dataset, options, position = 1)
  # drawing prior samples is pointless until the spec limits are set, and every
  # consumer of priorFit already handles NULL
  priorFit <- if (.bpcsIsReady(options)) .bpcsSamplePosteriorOrPrior(jaspResults, dataset, options, prior = TRUE) else NULL

  .bpcsProcessOverviewPlot(jaspResults, dataset, options, fit, priorFit, position = 2)
  .bpcsTimeSeriesPlot(jaspResults, dataset, options, position = 3)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 4)
  .bpcsCapabilityPlot(jaspResults, options, fit, priorFit, position = 5, base = "priorDistributionPlot")

  .bpcsIntervalTable(jaspResults, options, fit, position = 6)

  .bpcsSequentialPointEstimatePlot(   jaspResults, dataset, options, fit, position = 7)
  .bpcsSequentialIntervalEstimatePlot(jaspResults, dataset, options, fit, position = 8)

  .bpcsPlotPredictive(jaspResults, dataset, options, fit,      position = 9, base = "posteriorPredictiveDistributionPlot")
  .bpcsPlotPredictive(jaspResults, dataset, options, priorFit, position = 10, base = "priorPredictiveDistributionPlot")

}

.bpcsIsReady <- function(options) {
  # hasData <- if (options[["dataFormat"]] == "longFormat") {
  #   length(options[["measurementLongFormat"]]) > 0L && options[["measurementLongFormat"]] != ""
  # } else {
  #   length(options[["measurementsWideFormat"]]) > 0L
  # }
  hasData <- length(options[["measurementLongFormat"]]) > 0L && options[["measurementLongFormat"]] != ""
  hasData &&
    options[["lowerSpecificationLimit"]] &&
    options[["upperSpecificationLimit"]] &&
    options[["target"]]
}

.bpcsStateDeps <- function() {
  c(
      # data
      # "dataFormat", "measurementLongFormat", "measurementsWideFormat",
      # "subgroupSizeType", "manualSubgroupSizeValue", "subgroup", "groupingVariableMethod",
      # "stagesLongFormat", "stagesWideFormat",
      "measurementLongFormat",
      # specification
      "target",      "lowerSpecificationLimit",      "upperSpecificationLimit",
      "targetValue", "lowerSpecificationLimitValue", "upperSpecificationLimitValue",
      # likelihood
      "capabilityStudyType",
      # prior
      "priorSettings", "normalModelComponentsList", "tModelComponentsList",
      # MCMC settings
      "noIterations", "noWarmup", "noChains"
  )
}

.bpcsDefaultDeps <- function() {
  c(
      .bpcsStateDeps(),
      "axisLabels",
      # metrics
      "Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm"
  )
}

.bpcsAllMetrics <- function() {
  # casing must match the metric names qc uses, it errors on CpU / CpL
  c("Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm")
}

.bpcsDistributionFromOptions <- function(options) {
  switch(
    options[["capabilityStudyType"]],
    "normalCapabilityAnalysis" = "normal",
    "tCapabilityAnalysis" = "t",
    stop("Unknown capability study type: ", options[["capabilityStudyType"]])
  )
}

.bpcsMethodFromOptions <- function(options) {
  if (identical(.bpcsDistributionFromOptions(options), "normal"))
    "integration"
  else
    "mcmc"
}

# Every model fit goes through here so the likelihood, estimation method, and MCMC settings
# are applied consistently. The predictive plot's MCMC fallback is the only other qc::bpc call.
.bpcsFit <- function(x, options, prior = .bpcsPriorHelper(options), samplePriors = FALSE) {
  qc::bpc(
    x,
    distribution  = .bpcsDistributionFromOptions(options),
    method        = .bpcsMethodFromOptions(options),
    target        = options[["targetValue"]],
    LSL           = options[["lowerSpecificationLimitValue"]],
    USL           = options[["upperSpecificationLimitValue"]],
    prior         = prior,
    chains        = options[["noChains"]],
    warmup        = options[["noWarmup"]],
    iter          = options[["noIterations"]],
    silent        = TRUE,
    seed          = 1,
    sample_priors = samplePriors
  )
}

.bpcsPlotLayoutDeps <- function(base, hasPrior = TRUE, hasEstimate = TRUE, hasCi = TRUE, hasType = FALSE, hasAxes = TRUE) {
  c(
    base,
    if (hasEstimate) .bpcsPlotLayoutEstimateDeps(base),
    if (hasCi)       .bpcsPlotLayoutCiDeps(base),
    if (hasType)     .bpcsPlotLayoutTypeDeps(base),
    if (hasAxes)     .bpcsPlotLayoutAxesDeps(base),
    if (hasPrior)    .bpcsPlotLayoutPriorDeps(base)
  )
}

.bpcsPlotLayoutEstimateDeps <- function(base) { paste0(base, c("IndividualPointEstimate", "IndividualPointEstimateType")) }
.bpcsPlotLayoutCiDeps       <- function(base) { paste0(base, c("IndividualCi", "IndividualCiType", "IndividualCiMass", "IndividualCiLower", "IndividualCiUpper", "IndividualCiBf")) }
.bpcsPlotLayoutTypeDeps     <- function(base) { paste0(base, c("TypeLower", "TypeUpper")) }
.bpcsPlotLayoutAxesDeps     <- function(base) { paste0(base, c("PanelLayout", "Axes", "custom_x_min", "custom_x_max", "custom_y_min", "custom_y_max")) }
.bpcsPlotLayoutPriorDeps    <- function(base) { paste0(base, "PriorDistribution") }

.bpcsProcessCriteriaDeps <- function() {
  "processCriteria"
}

.bpcsOverviewOptionDeps <- function() {
  c("processOverview", "processOverviewMetric", "processOverviewThreshold", "processOverviewReferencePrior",
    .bpcsOverviewBinningDeps())
}

.bpcsOverviewBinningDeps <- function() {
  c("processOverviewBinning", "processOverviewNumberOfBins", "processOverviewObservationsPerBin")
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
    stop(gettext("Specify at least two process criteria regions."), call. = FALSE)

  n      <- length(criteria)
  upper  <- vapply(criteria, function(region) .bpcsScalarOption(region[["upper"]]), numeric(1))
  labels <- vapply(criteria, function(region) .bpcsScalarOption(region[["label"]], "character"), character(1))

  # The right bounds define the regions: the qml only displays each left bound as the right bound of the
  # row above, so the serialized left bounds are ignored. The outer bounds are always open-ended.
  upper[n] <- Inf
  lower    <- c(-Inf, upper[-n])

  if (anyNA(upper) || !all(is.finite(upper[-n])))
    stop(gettext("Process criteria bounds must be numeric."), call. = FALSE)
  if (any(lower >= upper))
    stop(gettext("Each process criteria boundary must be larger than the previous one."), call. = FALSE)
  if (anyNA(labels) || any(!nzchar(trimws(labels))))
    stop(gettext("Each process criterion needs a classification label."), call. = FALSE)

  list(
    lower  = lower,
    upper  = upper,
    labels = make.unique(labels),
    values = upper[-n]
  )
}

.bpcsPriorComponentByName <- function(options, name) {
  listName <- switch(
    .bpcsDistributionFromOptions(options),
    "normal" = "normalModelComponentsList",
    "t"      = "tModelComponentsList"
  )
  for (comp in options[[listName]]) {
    if (identical(comp[["name"]], name))
      return(comp)
  }
  stop(gettextf("No prior distribution is specified for %s.", name), call. = FALSE)
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
  if (is.null(optionsPrior))
    return(NULL)

  if (optionsPrior$type == "jeffreys")
    return(paste0("Jeffreys_", paramName))

  arguments <- list()

  arguments[["distribution"]] <- switch(
    optionsPrior[["type"]],
    "gammaAB" = "gamma",
    "gammaK0" = "gamma",
    optionsPrior[["type"]]
  )

  arguments[["parameters"]] <- switch(
    optionsPrior[["type"]],
    "normal"      = list("mean" = optionsPrior[["mu"]], "sd" = optionsPrior[["sigma"]]),
    "t"           = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["sigma"]], "df" = optionsPrior[["nu"]]),
    "cauchy"      = list("location" = optionsPrior[["mu"]], "scale" = optionsPrior[["theta"]]),
    "gammaAB"     = list("shape" = optionsPrior[["alpha"]], "rate" = optionsPrior[["beta"]]),
    "gammaK0"     = list("shape" = optionsPrior[["k"]], "rate" = 1/optionsPrior[["theta"]]),
    "invgamma"    = list("shape" = optionsPrior[["alpha"]], "scale" = optionsPrior[["beta"]]),
    "lognormal"   = list("meanlog" = optionsPrior[["mu"]], "sdlog" = optionsPrior[["sigma"]]),
    "beta"        = list("alpha" = optionsPrior[["alpha"]], "beta" = optionsPrior[["beta"]]),
    "uniform"     = list("a" = optionsPrior[["a"]], "b" = optionsPrior[["b"]]),
    "exponential" = list("rate" = optionsPrior[["lambda"]]),
    "spike"       = list("location" = optionsPrior[["x0"]])
  )

  if(!arguments[["distribution"]] %in% c("spike", "uniform")) {
    arguments[["truncation"]] <- list(
      lower   = optionsPrior[["truncationLower"]],
      upper   = optionsPrior[["truncationUpper"]]
    )
  }

  return(do.call(BayesTools::prior, arguments))
}

.bpcsMuPriorFromOptions <- function(options) {
  if (options$priorSettings == "default") {
    return("Jeffreys_mu")
  } else {
    comp <- .bpcsPriorComponentByName(options, "mean")
    return(.bpcsPriorFromComponent(comp, "mu"))
  }
}

.bpcsSigmaPriorFromOptions <- function(options) {
  if (options$priorSettings == "default") {
    return("Jeffreys_sigma")
  } else {
    comp <- .bpcsPriorComponentByName(options, "sigma")
    return(.bpcsPriorFromComponent(comp, "sigma"))
  }
}

.bpcsTPriorFromOptions <- function(options) {

  switch(options[["capabilityStudyType"]],
    "normalCapabilityAnalysis" = NULL,
    "tCapabilityAnalysis"      = .bpcsPriorFromComponent(.bpcsPriorComponentByName(options, "df"), "df"),

    stop("Unknown capability study type: ", options[["capabilityStudyType"]])
  )
}

.bpcsPriorHelper <- function(options) {
  if (options$priorSettings == "default") {
    if (options[["capabilityStudyType"]] == "normalCapabilityAnalysis") {
      return("DCSI")
    }
    return("Jeffreys")
  }

  mu_prior    <- .bpcsMuPriorFromOptions(options)
  sigma_prior <- .bpcsSigmaPriorFromOptions(options)
  nu_prior    <- .bpcsTPriorFromOptions(options)

  args <- list(mu = mu_prior, sigma = sigma_prior)
  if (!is.null(nu_prior)) {
    args$nu <- nu_prior
  }

  do.call(qc::prior_independent, args)
}

# Tables ----
.bpcsCapabilityTable <- function(jaspResults, dataset, options, position) {

  # Check if we already have the results cached
  if (!is.null(jaspResults[["bpcsCapabilityTable"]])) {
    if (!.bpcsIsReady(options))
      return(NULL)
    return(.bpcsSamplePosteriorOrPrior(jaspResults, dataset, options)) # will return object from state (if it exists)
  }

  table <- .bpcsCapabilityTableMeta(jaspResults, options, position = position)
  if (!.bpcsIsReady(options)) {

    if (options[["measurementLongFormat"]] != "" || length(options[["measurementsWideFormat"]]) > 0)
      table$addFootnote(gettext(
        "Please specify the Lower Specification Limit, Upper Specification Limit, and Target Value to compute the capability measures."
      ))

    return(NULL)
  }

  resultsObject <- .bpcsSamplePosteriorOrPrior(jaspResults, dataset, options)

  .bpcsCapabilityTableFill(table, resultsObject, options)
  return(resultsObject)

}

# Returns list(rawfit, summaryObject), assembled from the dependency-controlled fit and summary
# states on every call. For prior = TRUE an unavailable prior gives list(error = <message>) instead,
# so the posterior outputs keep working and the prior-only outputs can explain why they are empty.
.bpcsSamplePosteriorOrPrior <- function(jaspResults, dataset, options, prior = FALSE) {

  base <- if (prior) "bpcsPriors" else "bpcs"
  if (prior && !.bpcsPriorIsProper(options))
    return(list(error = gettext("The prior distribution is improper, so it cannot be sampled.")))

  x <- if (ncol(dataset) > 0L) dataset[[1L]] else NULL

  sample <- function() {
    rawfit <- jaspResults[[paste0(base, "State")]] %setOrRetrieve% (
      .bpcsFit(x, options, samplePriors = prior) |>
        createJaspState(jaspDeps(.bpcsStateDeps()))
    )

    summaryObject <- jaspResults[[paste0(base, "SummaryState")]] %setOrRetrieve% (
      summary(
        rawfit, ci.level = options[["credibleIntervalWidth"]]
      ) |>
        createJaspState(jaspDeps(
            options = c(.bpcsStateDeps(), "credibleIntervalWidth")
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
    list(error = gettextf("The prior distribution could not be computed: %s", conditionMessage(e)))
  })
}

.bpcsPriorIsUsable <- function(priorFit) {
  !is.null(priorFit) && is.null(priorFit[["error"]])
}

# Whether the displayed prior is proper. Jeffreys components (and the default Jeffreys prior of the
# Student-t model) are improper, as is a uniform component without finite bounds.
.bpcsPriorIsProper <- function(options) {
  if (options$priorSettings == "default")
    return(identical(.bpcsDistributionFromOptions(options), "normal"))

  components <- tryCatch(.bpcsActivePriorComponents(options), error = function(e) NULL)
  if (is.null(components))
    return(FALSE)

  for (comp in components) {
    if (identical(comp[["type"]], "jeffreys"))
      return(FALSE)
    if (identical(comp[["type"]], "uniform") &&
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

  overtitle <- gettextf("%s%% Credible Interval", 100 * options[["credibleIntervalWidth"]])
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

  table$dependOn(c(.bpcsDefaultDeps(), "credibleIntervalWidth"))

  jaspResults[["bpcsCapabilityTable"]] <- table
  return(table)

}

.bpcsGetSelectedMetrics <- function(options) {
  allMetrics <- .bpcsAllMetrics()
  selectedMetrics <- allMetrics[c(options[["Cp"]],   options[["Cpu"]],  options[["Cpl"]],
                                  options[["Cpk"]],  options[["Cpc"]],  options[["Cpm"]])]
  return(selectedMetrics)
}

.bpcsGetCustomAxisLimits <- function(options, base) {
  keys <- c(paste0(base, "custom_x_", c("min", "max")), paste0(base, "custom_y_", c("min", "max")))
  values <- lapply(keys, function(k) options[[k]])
  names(values) <- c("xmin", "xmax", "ymin", "ymax")
  values
}
# end utils

.bpcsCapabilityTableFill <- function(table, resultsObject, options) {

  df <- as.data.frame(resultsObject[["summaryObject"]][["summary"]])

  # Filter metrics based on user selection
  selectedMetrics <- .bpcsGetSelectedMetrics(options)

  if (length(selectedMetrics) > 0) {
    df <- df[df$metric %in% selectedMetrics, , drop = FALSE]
  }

  table$setData(df)

}

.bpcsIntervalTable <- function(jaspResults, options, fit, position) {

  if (!options[["intervalTable"]])
    return()

  criteria <- tryCatch(.bpcsProcessCriteria(options), error = function(e) e)
  table <- .bpcsIntervalTableMeta(jaspResults, options, position, criteria)
  if (inherits(criteria, "error")) {
    table$setError(gettextf("Invalid process criteria: %s", conditionMessage(criteria)))
    return()
  }

  if (!.bpcsIsReady(options) || is.null(fit))
    return()

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  tryCatch({

    interval_summary <- summary(fit[["rawfit"]], interval_probability = criteria$values)[["interval_summary"]]
    colnames(interval_summary) <- c("metric", paste0("interval", seq_along(criteria$labels)))
    interval_summary <- interval_summary[interval_summary$metric %in% selectedMetrics, , drop = FALSE]
    table$setData(interval_summary)

  }, error = function(e) {

    table$setError(gettextf("Unexpected error in interval table: %s", e$message))

  })

  return()
}

.bpcsIntervalTableMeta <- function(jaspResults, options, position, criteria) {

  table <- createJaspTable(title = gettext("Interval Table"), position = position)
  table$dependOn(c("intervalTable", .bpcsDefaultDeps(), .bpcsProcessCriteriaDeps()))
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
.bpcsProcessOverviewPlot <- function(jaspResults, dataset, options, fit, priorFit, position) {

  base <- "processOverview"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = gettext("Process Overview"), width = 1200, height = 800,
    position = position,
    dependencies = jaspDeps(c(
      .bpcsOverviewOptionDeps(), .bpcsStateDeps(), .bpcsProcessCriteriaDeps()
    ))
  )
  jaspResults[[base]] <- plot

  if (!.bpcsIsReady(options) || is.null(fit) || jaspResults$getError())
    return()

  tryCatch({
    criteria <- .bpcsProcessCriteria(options)
    data <- .bpcsOverviewData(jaspResults, dataset, options, fit, criteria)
    plot$plotObject <- .bpcsMakeProcessOverviewPlot(dataset, options, fit, priorFit, data, criteria)
  }, error = function(e) {
    plot$setError(gettextf("Unexpected error in process overview: %s", e$message))
  })
}

# The sequential fits and the reference-prior fit are cached separately, and both store results for
# every metric, so changing the overview metric or threshold reuses all fits.
.bpcsOverviewData <- function(jaspResults, dataset, options, fit, criteria) {
  sequential <- jaspResults[["processOverviewSequentialData"]] %setOrRetrieve% (
    .bpcsComputeOverviewSequentialData(dataset, options, criteria) |>
      createJaspState(jaspDeps(c(.bpcsOverviewBinningDeps(), .bpcsStateDeps(), .bpcsProcessCriteriaDeps())))
  )
  reference <- jaspResults[["processOverviewReferenceData"]] %setOrRetrieve% (
    .bpcsComputeOverviewReferenceData(dataset, options, fit, criteria) |>
      createJaspState(jaspDeps(c("processOverviewReferencePrior", .bpcsStateDeps(), .bpcsProcessCriteriaDeps())))
  )
  list(sequential = sequential, sensitivity = reference)
}

.bpcsOverviewThresholdIndex <- function(options, criteria) {
  threshold <- .bpcsScalarOption(options[["processOverviewThreshold"]])
  index <- if (is.finite(threshold)) which(abs(criteria$values - threshold) <= 1e-9 * max(1, abs(threshold))) else integer()
  if (length(index) != 1L)
    stop(gettext("The selected threshold is not one of the process criteria boundaries."), call. = FALSE)
  index
}

.bpcsOverviewRegionColors <- function(criteria) {
  colors <- qc::default_region_colors()
  if (length(criteria$labels) > length(colors))
    colors <- grDevices::hcl.colors(length(criteria$labels), palette = "Set 2")
  else
    colors <- colors[seq_len(length(criteria$labels))]

  names(colors) <- criteria$labels
  colors
}

# Cumulative-fit checkpoints: each one refits the model on the first k observations.
.bpcsOverviewSampleSizes <- function(n, options) {
  if (!is.finite(n) || n < 3L)
    stop(gettext("The process overview requires at least 3 observations."), call. = FALSE)

  n <- as.integer(n)
  mode <- options[["processOverviewBinning"]]
  if (identical(mode, "perObservation"))
    return(seq.int(3L, n))

  if (identical(mode, "observationsPerBin")) {
    size <- as.integer(.bpcsScalarOption(options[["processOverviewObservationsPerBin"]]))
    if (is.na(size) || size < 1L)
      stop(gettext("The number of observations per bin must be at least 1."), call. = FALSE)
    return(unique(c(seq.int(3L, n, by = size), n)))
  }

  if (identical(mode, "noBins")) {
    bins <- as.integer(.bpcsScalarOption(options[["processOverviewNumberOfBins"]]))
    if (is.na(bins) || bins < 1L)
      stop(gettext("The number of bins must be at least 1."), call. = FALSE)
    if (bins == 1L)
      return(n)
    return(unique(as.integer(round(seq(3, n, length.out = bins)))))
  }

  stop(gettext("Unknown process overview binning option."), call. = FALSE)
}

.bpcsOverviewReferencePrior <- function(options) {
  value <- .bpcsScalarOption(options[["processOverviewReferencePrior"]], "character")
  if (is.na(value) || !value %in% c("DCSI", "Jeffreys", "unit_information"))
    stop(gettext("Unknown reference prior."), call. = FALSE)
  value
}

.bpcsOverviewMetric <- function(options) {
  metric <- .bpcsScalarOption(options[["processOverviewMetric"]], "character")
  if (is.na(metric) || !metric %in% .bpcsAllMetrics())
    stop(gettext("Unknown process overview capability metric."), call. = FALSE)
  metric
}

.bpcsPriorLabel <- function(prior) {
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
  x <- dataset[[1L]]
  prior <- .bpcsPriorHelper(options)

  jaspBase::startProgressbar(length(sampleSizes), label = gettext("Running process overview"))
  for (i in seq_along(sampleSizes)) {
    # fitting and extracting the probabilities form one checkpoint, it is only stored if both succeed
    checkpoint <- tryCatch(
      .bpcsAllExceedanceProbabilities(.bpcsFit(x[seq_len(sampleSizes[i])], options, prior = prior), criteria$values),
      error = function(e) NULL
    )

    if (is.null(checkpoint)) {
      failed[i] <- TRUE
    } else {
      for (metricName in metrics)
        probabilities[[metricName]][i, ] <- checkpoint[[metricName]]
    }
    jaspBase::progressbarTick()
  }

  list(
    sampleSizes   = sampleSizes,
    probabilities = probabilities,
    failed        = failed
  )
}

.bpcsComputeOverviewReferenceData <- function(dataset, options, fit, criteria) {

  activePrior    <- .bpcsPriorHelper(options)
  referencePrior <- .bpcsOverviewReferencePrior(options)
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
      unavailable <- gettextf("The %1$s could not be fitted: %2$s", referenceLabel, conditionMessage(referenceFit))
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
      unavailable <- c(unavailable, gettextf("The %1$s results could not be computed: %2$s", label, conditionMessage(result)))
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

  metric         <- .bpcsOverviewMetric(options)
  thresholdIndex <- .bpcsOverviewThresholdIndex(options, criteria)
  threshold      <- criteria$values[thresholdIndex]
  regionColors   <- .bpcsOverviewRegionColors(criteria)
  rawData        <- dataset[[1L]]

  timeSeriesPlot <- qc::plot_time_series(
    rawData,
    LSL = options[["lowerSpecificationLimitValue"]],
    target = options[["targetValue"]],
    USL = options[["upperSpecificationLimitValue"]]
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
    ggplot2::labs(title = gettext("B. Prior and posterior distributions"), x = metric, y = gettext("Density")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

  overTimeTitle <- gettextf("C. Monitoring P(%1$s > %2$g | data) sequentially", metric, threshold)
  overTimePlot <- .bpcsMakeOverviewSequentialPanel(data$sequential, metric, thresholdIndex, priorFit, criteria, overTimeTitle)

  sensitivityPlot <- .bpcsMakeOverviewSensitivityPanel(data$sensitivity, metric, thresholdIndex, threshold, regionColors)

  (patchwork::wrap_plots(timeSeriesPlot, densityPlot, overTimePlot, sensitivityPlot, ncol = 2) +
    patchwork::plot_layout(guides = 'collect')) &
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
}

.bpcsMakeOverviewSequentialPanel <- function(sequential, metric, thresholdIndex, priorFit, criteria, title) {

  failed <- sequential$failed
  nFailed <- sum(failed)
  nTotal <- length(failed)
  if (nFailed / nTotal > 0.1)
    return(.bpcsUnavailablePanel(title, gettextf(
      "%1$d of %2$d sequential updates failed (more than 10%%),\nso the sequential probabilities cannot be shown.",
      nFailed, nTotal
    )))

  overTimeData <- data.frame(
    observation = sequential$sampleSizes,
    probability = sequential$probabilities[[metric]][, thresholdIndex]
  )[!failed, , drop = FALSE]
  # consecutive successful checkpoints share a segment, so lines never bridge a failed update
  overTimeData$segment <- cumsum(c(TRUE, diff(which(!failed)) != 1L))

  captions <- character()
  if (nFailed > 0L)
    captions <- c(captions, gettextf("%1$d of %2$d sequential updates failed and are omitted.", nFailed, nTotal))
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
      base, "measurementLongFormat",
      "lowerSpecificationLimit", "lowerSpecificationLimitValue",
      "target", "targetValue",
      "upperSpecificationLimit", "upperSpecificationLimitValue"
    ))
  )
  jaspResults[[base]] <- plot

  if (ncol(dataset) == 0L || jaspResults$getError())
    return()

  tryCatch({
    plot$plotObject <- qc::plot_time_series(
      dataset[[1L]],
      LSL = if (options[["lowerSpecificationLimit"]]) options[["lowerSpecificationLimitValue"]] else NULL,
      target = if (options[["target"]]) options[["targetValue"]] else NULL,
      USL = if (options[["upperSpecificationLimit"]]) options[["upperSpecificationLimitValue"]] else NULL
    ) +
      .bpcsTimeSeriesLabels() +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }, error = function(e) {
    plot$setError(gettextf("Unexpected error in time series plot: %s", e$message))
  })
}

.bpcsCapabilityPlot <- function(jaspResults, options, fit, priorFit, position, base = "posteriorDistributionPlot") {

  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  singlePanel <- options[[paste0(base, "PanelLayout")]] != "multiplePanels"

  isPost <- base == "posteriorDistributionPlot"
  summaryObject <- if (isPost) fit$summaryObject else priorFit$summaryObject
  # only if the user asked for it and the prior is available
  priorSummaryObject <- if (isPost && options[[paste0(base, "PriorDistribution")]] && .bpcsPriorIsUsable(priorFit)) priorFit$summaryObject else NULL

  plotWidth <- 400 * (if (singlePanel) 1 else 3)

  jaspPlt <- createJaspPlot(
    title = if (isPost) gettext("Posterior Distribution") else gettext("Prior Distribution"),
    width  = plotWidth,
    height = 400 * (if (singlePanel) 1 else 2),
    position = position,
    dependencies = jaspDeps(
      options = c(
        .bpcsDefaultDeps(),
        # .bpcsPosteriorPlotDeps(options),
        .bpcsPlotLayoutDeps(base, hasType = FALSE)
      )
    )
  )
  jaspResults[[base]] <- jaspPlt

  if (!.bpcsIsReady(options) || (isPost && is.null(fit)) || (!isPost && is.null(priorFit)))
    return()

  if (!isPost && !is.null(priorFit[["error"]])) {
    jaspPlt$width <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(priorFit[["error"]])
    return()
  }

  tryCatch({

    # Get selected metrics
    selectedMetrics <- .bpcsGetSelectedMetrics(options)

    if (length(selectedMetrics) == 0) {
      NULL
    } else {

      # qc draws the point estimate / ci annotation with ggtext::geom_richtext(size = ...),
      # whose size is in mm and defaults to 18 (~51pt), swamping the panel. Scale it to the
      # width one panel actually gets instead: facet_wrap spreads the metrics over
      # ceiling(sqrt(n)) columns of plotWidth, and 3mm is the largest that keeps the longest
      # label ("Mean = x.xxx; xx.x% CI [x.xxx, x.xxx]") inside a 400px panel. Never grow past
      # the standard jasp font size.
      nColumns       <- if (singlePanel) 1L else ceiling(sqrt(length(selectedMetrics)))
      panelWidth     <- plotWidth / nColumns
      annotationSize <- min(3 * panelWidth / 400, jaspGraphs::graphOptions("fontsize") / ggplot2::.pt)

      jaspPlt$plotObject <- qc::plot_density(
        summaryObject,
        what = selectedMetrics,
        point_estimate     = if (options[[paste0(base, "IndividualPointEstimate")]]) options[[paste0(base, "IndividualPointEstimateType")]] else "none",
        ci                 = if (options[[paste0(base, "IndividualCi")]])            options[[paste0(base, "IndividualCiType")]]            else "none",
        # IndividualCiMass is a 1-100 percentage (see Common/PlotLayout.qml's CIField overrides),
        # but qc::plot_density's ci_level wants a 0-1 proportion and errors above 1.
        ci_level           = options[[paste0(base, "IndividualCiMass")]] / 100,
        ci_custom_left     = options[[paste0(base, "IndividualCiLower")]],
        ci_custom_right    = options[[paste0(base, "IndividualCiUpper")]],
        bf_support         = options[[paste0(base, "IndividualCiBf")]],
        single_panel       = singlePanel,
        axes               = options[[paste0(base, "Axes")]],
        axes_custom        = .bpcsGetCustomAxisLimits(options, base),
        priorSummaryObject = priorSummaryObject,
        textsize           = annotationSize
      ) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw()


    }
  }, error = function(e) {
    jaspPlt$width  <- 400
    jaspPlt$height <- 400
    jaspPlt$setError(
      if (isPost) gettextf("Unexpected error in posterior distribution plot: %s", e$message)
      else gettextf("Unexpected error in prior distribution plot: %s", e$message)
    )
  })

}

# .bpcsPosteriorPlotDeps <- function(options) {
#   c(
#     "posteriorDistributionPlot",
#     "posteriorDistributionPlotIndividualPointEstimate",
#     "posteriorDistributionPlotIndividualPointEstimateType",
#     "posteriorDistributionPlotPriorDistribution",
#     "posteriorDistributionPlotIndividualCi",
#     "posteriorDistributionPlotIndividualCiType",
#     # these match which options are conditionally enabled in the qml file.
#     switch(options[["posteriorDistributionPlotIndividualCiType"]],
#       "central" = "posteriorDistributionPlotIndividualCiMass",
#       "HPD"     = "posteriorDistributionPlotIndividualCiMass",
#       "custom"  = c("posteriorDistributionPlotIndividualCiLower", "posteriorDistributionPlotIndividualCiUpper"),
#       "support" = "posteriorDistributionPlotIndividualCiBf"
#     )
#   )
# }

.bpcsSequentialPointEstimatePlot <- function(jaspResults, dataset, options, fit, position) {

  base <- "sequentialAnalysisPointEstimatePlot"
  # "sequentialAnalysisPointIntervalPlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis Point Estimate"), width = 3*w, height = 2*w,
                        position = position,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(),
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE),
                          "sequentialAnalysisPlotAdditionalInfo",
                          .bpcsProcessCriteriaDeps()
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit)

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base)
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in sequential analysis point estimate plot: %s", e$message))
    }
    )
  }
}

.bpcsSequentialIntervalEstimatePlot <- function(jaspResults, dataset, options, fit, position) {

  # base <- "sequentialAnalysisPointEstimatePlot"
  base <- "sequentialAnalysisPointIntervalPlot"
  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  w <- 400
  plt <- createJaspPlot(title = gettext("Sequential Analysis Interval Estimate"), width = 3*w, height = 2*w,
                        position = position,
                        dependencies = jaspDeps(c(
                          .bpcsDefaultDeps(),
                          # mirrors the flags set on this plot's Common.PlotLayout in the qml
                          .bpcsPlotLayoutDeps(base, hasPrior = FALSE, hasEstimate = FALSE, hasCi = FALSE, hasType = TRUE)
                        )))
  jaspResults[[base]] <- plt

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  sequentialPlotData <- .bpcsGetSequentialAnalysis(jaspResults, dataset, options, fit)

  if (!is.null(sequentialPlotData$error)) {
    plt$setError(sequentialPlotData$error)
  } else {
    tryCatch({
      plt$plotObject <- .bpcsMakeSequentialPlot(sequentialPlotData$data, options, base, custom = TRUE)
    }, error = function(e) {
      plt$setError(gettextf("Unexpected error in sequential analysis interval estimate plot: %s", e$message))
    }
    )
  }
}

.bpcsGetSequentialAnalysis <- function(jaspResults, dataset, options, fit) {

  if (!.bpcsIsReady(options) || jaspResults$getError()) return()

  base1 <- "sequentialAnalysisPointEstimatePlot"
  base2 <- "sequentialAnalysisPointIntervalPlot"

  baseData <- "SequentialAnalysisData"
  tryCatch({
    sequentialPlotData <- jaspResults[[baseData]] %setOrRetrieve% (
      .bpcsComputeSequentialAnalysis(dataset, options, fit) |>
        createJaspState(dependencies = jaspDeps(
          options = c(.bpcsStateDeps(),
                      paste0(base2, c("TypeLower", "TypeUpper")))
          ))
    )

    return(list(data = sequentialPlotData, error = NULL))

  }, error = function(e) {

    return(list(data = NULL, error = e$message))

  })

}

.bpcsComputeSequentialAnalysis <- function(dataset, options, fit) {

  n <- nrow(dataset)
  if (!is.finite(n) || n < 3L) {
    stop("Sequential analysis requires at least 3 observations.", call. = FALSE)
  }
  nfrom <- 3L
  nto   <- n
  nby   <- 1L
  nseq <- seq(nfrom, nto, by = nby)
  estimates <- array(NA, c(6, 5, length(nseq)))

  hasCustom <- options$sequentialAnalysisPointIntervalPlot
  customBounds <- c(options$sequentialAnalysisPointIntervalPlotTypeLower,
                    options$sequentialAnalysisPointIntervalPlotTypeUpper)

  keys <- c("mean", "median", "lower", "upper", "custom")
  dimnames(estimates) <- list(list(), keys, list())

  x <- dataset[[1L]]

  jaspBase::startProgressbar(length(nseq), label = gettext("Running sequential analysis"))

  prior <- .bpcsPriorHelper(options)
  n_failed <- 0L
  for (i in seq_along(nseq)) {

    x_i <- x[1:nseq[i]]
    fit_i <- tryCatch(
      .bpcsFit(x_i, options, prior = prior),
      error = function(e) NULL
    )

    if (is.null(fit_i)) {
      n_failed <- n_failed + 1L
      jaspBase::progressbarTick()
      next
    }

    sum_fit_i <- summary(fit_i, interval_probability = customBounds)
    sum_i <- sum_fit_i$summary
    custom_i <- sum_fit_i$interval_summary[, 3, drop = FALSE]
    colnames(custom_i) <- "custom"
    sum_i <- cbind(sum_i, custom_i)

    if (is.null(rownames(estimates)))
      rownames(estimates) <- sum_i$metric

    estimates[, , i] <- as.matrix(sum_i[keys])
    jaspBase::progressbarTick()
  }

  if (n_failed > 0L && n_failed / length(nseq) > 0.1) {
    stop(
      sprintf(
        "%d of %d sequential fits failed (%.0f%%). Cannot render plot.",
        n_failed, length(nseq), 100 * n_failed / length(nseq)
      ),
      call. = FALSE
    )
  }

  attr(estimates, "nseq") <- nseq

  # we could use this one, but only if the CI width is exactly equal to the one requested here.
  # that would be nice to add at some point so the values in the table are identical to those in the plot
  # sum_n <- summary(fit)$summary
  # estimates[, , n] <- as.matrix(sum_n[keys])

  return(estimates)
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

.bpcsSequentialYScale <- function(lower, upper, gridLines, categoryNames, custom, addInfo) {
  if (custom) {
    # probabilities, so the axis is fixed and unrelated to the process criteria
    leftBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  } else {
    observedRange <- range(lower, upper, na.rm = TRUE)
    if (!all(is.finite(observedRange))) {
      observedRange <- c(0, 1)
    }
    dist <- observedRange[2L] - observedRange[1L]

    observedRange[1L] <- min(observedRange[1L], gridLines[1L] - 0.1 * dist)
    observedRange[2L] <- max(observedRange[2L], gridLines[length(gridLines)] + 0.1 * dist)

    leftBreaks <- jaspGraphs::getPrettyAxisBreaks(observedRange)
  }
  leftLimits <- range(leftBreaks)

  rightAxis <- if (addInfo) .bpcsSequentialCriteriaAxis(leftLimits, gridLines, categoryNames) else ggplot2::waiver()

  ggplot2::scale_y_continuous(breaks = leftBreaks, limits = leftLimits,
                              minor_breaks = if (custom) ggplot2::waiver() else gridLines,
                              sec.axis = rightAxis)
}

.bpcsMakeSequentialPlot <- function(estimates, options, base, custom = FALSE) {

  # this function should move to qc, and these are the arguments that should be passed to the arguments of that function
  single_panel <- options[[paste0(base, "PanelLayout")]] != "multiplePanels"
  axes         <- options[[paste0(base, "Axes")]]
  axes_custom  <- .bpcsGetCustomAxisLimits(options, base)

  pointEstimateOption <- paste0(base, "IndividualPointEstimateType")
  pointEstimateName <- if (options[[pointEstimateOption]] == "mean") "mean" else "median"
  add_additional_info <- options[["sequentialAnalysisPlotAdditionalInfo"]]

  selectedMetrics <- .bpcsGetSelectedMetrics(options)
  if (length(selectedMetrics) == 0L)
    return(NULL)

  ciOption <- paste0(base, "IndividualCi")
  has_ci <- options[[ciOption]]

  if (custom) {
    has_ci <- FALSE
    pointEstimateName <- "custom"
    add_additional_info <- FALSE
    y_title <- gettextf("P(%1$.3f ≤ x ≤ %2$.3f)",
                       options$sequentialAnalysisPointIntervalPlotTypeLower,
                       options$sequentialAnalysisPointIntervalPlotTypeUpper)
    gridLines <- numeric()
    categoryNames <- character()
  } else {

    y_title <- if (has_ci) {
      gettextf("Estimate with 95%% credible interval")
    } else {
      gettext("Estimate")
    }

    criteria <- .bpcsProcessCriteria(options)
    categoryNames <- criteria$labels
    gridLines <- criteria$values
  }

  # this is somewhat ugly, but we convert the 3d array to a tibble for plotting
  # we don't create the tibble immediately in the previous function, because
  # it takes up more space in the state (which means larger jasp files)

  nseq <- attr(estimates, "nseq")

  tb <- tibble::tibble(
    metric = factor(rep(rownames(estimates), times = length(nseq))),
    n      = rep(nseq, each = nrow(estimates)),
    mean   = as.vector(estimates[, pointEstimateName, ]),
    lower  = as.vector(estimates[, "lower", ]),
    upper  = as.vector(estimates[, "upper", ]),
  )
  tb <- tb[tb$metric %in% selectedMetrics, , drop = FALSE]
  if (length(selectedMetrics) == 1L)
    single_panel <- TRUE

  # get y scales per facet
  if (single_panel) {
    y_breaks_per_scale <- .bpcsSequentialYScale(tb$lower, tb$upper, gridLines, categoryNames, custom, add_additional_info)
  } else {
    y_breaks_per_scale <- tapply(tb, tb$metric, \(x) {
      .bpcsSequentialYScale(x$lower, x$upper, gridLines, categoryNames, custom, add_additional_info)
    }, simplify = FALSE)
  }

  ribbon <- NULL
  if (has_ci)
    ribbon <- ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.3)

  extraTheme <- gridLinesLayer <- NULL
  sides <- "bl"
  if (add_additional_info) {
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
      # show.legend = FALSE,
      linewidth = .5, color = "lightgray", linetype = "dashed"
    )

  }

  scale_x <- scale_facet <- facet <- NULL
  noMetrics <- nrow(estimates)
  if (noMetrics == 1L || single_panel) {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(tb$n)
    xLimits <- range(tb$n)
    scale_x <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits)
    scale_facet <- y_breaks_per_scale
  } else {
    scales <- switch(axes,
                     "automatic" = "free_y",
                     "fixed"     = "fixed",
                     "free"      = "free_y",
                     "custom"    = "fixed",
                     stop("Unknown axes option.")
    )
    if (axes == "custom") {
      if (!is.null(axes_custom[["xmin"]]) && !is.null(axes_custom[["xmax"]])) {
        xbreaks <- jaspGraphs::getPrettyAxisBreaks(c(axes_custom[["xmin"]], axes_custom[["xmax"]]))
        scale_x <- ggplot2::scale_x_continuous(limits = sort(c(axes_custom[["xmin"]], axes_custom[["xmax"]])))
      }
      if (!is.null(axes_custom[["ymin"]]) && !is.null(axes_custom[["ymax"]])) {
        ybreaks <- jaspGraphs::getPrettyAxisBreaks(c(axes_custom[["ymin"]], axes_custom[["ymax"]]))
        leftLimits <- sort(c(axes_custom[["ymin"]], axes_custom[["ymax"]]))
        rightAxis <- if (add_additional_info) .bpcsSequentialCriteriaAxis(leftLimits, gridLines, categoryNames) else ggplot2::waiver()
        scale_facet <- ggplot2::scale_y_continuous(breaks = ybreaks, limits = leftLimits,
                                                   minor_breaks = if (custom) ggplot2::waiver() else gridLines,
                                                   sec.axis = rightAxis)
      }
    } else if (axes == "automatic" || axes == "free") {
      scale_facet <- ggh4x::facetted_pos_scales(y = y_breaks_per_scale)
    }
    facet <- ggplot2::facet_wrap(~metric, scales = scales)
  }

  ggplot2::ggplot(tb, ggplot2::aes(x = .data$n, y = .data$mean, group = .data$metric,
                                   color = .data$metric, fill = .data$metric)) +
    gridLinesLayer +
    ribbon +
    ggplot2::geom_line(linewidth = 1) +
    facet + scale_facet + scale_x +
    ggplot2::labs(
      x     = gettext("Number of observations"),
      y     = y_title,
      color = gettext("Metric"),
      fill  = gettext("Metric")
    ) +
    jaspGraphs::geom_rangeframe(sides = sides) +
    jaspGraphs::themeJaspRaw(legend.position = if (single_panel) "right" else "none") +
    extraTheme

}

# Additional plot functions ----
.bpcsPlotPredictive <- function(jaspResults, dataset, options, fit, position, base = c("posteriorPredictiveDistributionPlot", "priorPredictiveDistributionPlot")) {

  base <- match.arg(base)
  isPrior <- base == "priorPredictiveDistributionPlot"

  if (!options[[base]] || !is.null(jaspResults[[base]]))
    return()

  plot <- createJaspPlot(
    title = if (isPrior) gettext("Prior predictive distribution") else gettext("Posterior Predictive Distribution"),
    width = 400,
    height = 400,
    position = position,
    dependencies = c(
    .bpcsDefaultDeps(),
    base,
    paste0(base, "IndividualPointEstimate"),
    paste0(base, "IndividualPointEstimateType"),
    paste0(base, "IndividualCi"),
    paste0(base, "IndividualCiType"),
    paste0(base, "IndividualCiMass"),
    paste0(base, "IndividualCiLower"),
    paste0(base, "IndividualCiUpper")
  ))

  jaspResults[[base]] <- plot

  if (!.bpcsIsReady(options) || is.null(fit) || jaspResults$getError()) return()

  if (!is.null(fit[["error"]])) {
    plot$setError(fit[["error"]])
    return()
  }

  tryCatch({
    rawfit <- fit$rawfit
    predictiveSamples <- tryCatch(
      qc::extract_predictive_samples(rawfit),
      # qc can only draw predictives from an integration fit when the prior is conjugate;
      # for any other prior refit with mcmc so the plot can still be shown
      error = function(e) {
        mcmcfit <- qc::bpc(
          x            = if (ncol(dataset) > 0L) dataset[[1L]] else NULL,
          method       = "mcmc",
          distribution = rawfit$distribution %||% "normal",
          prior        = rawfit$prior,
          LSL          = options[["lowerSpecificationLimitValue"]],
          USL          = options[["upperSpecificationLimitValue"]],
          target       = options[["targetValue"]],
          chains       = options[["noChains"]],
          warmup       = options[["noWarmup"]],
          iter         = options[["noIterations"]],
          silent       = TRUE, seed = 1,
          sample_priors = isPrior
        )
        qc::extract_predictive_samples(mcmcfit)
      }
    )

    plt <- jaspGraphs::jaspHistogram(
      predictiveSamples,
      xName = if (isPrior) gettext("Prior predictive") else gettext("Posterior predictive"),
      density = TRUE
    )

    # Calculate density for positioning elements above histogram
    dens <- stats::density(predictiveSamples)
    maxDensity <- max(dens$y)

    # Add point estimate if requested
    if (options[[paste0(base, "IndividualPointEstimate")]]) {
      pointEstimateType <- options[[paste0(base, "IndividualPointEstimateType")]]
      pointEstimate <- switch(pointEstimateType,
        "mean"   = mean(predictiveSamples),
        "median" = stats::median(predictiveSamples),
        "mode"   = dens$x[which.max(dens$y)]
      )
      plt <- plt + ggplot2::geom_point(
        data = data.frame(x = pointEstimate, y = 0),
        ggplot2::aes(x = .data$x, y = .data$y),
        size = 3,
        inherit.aes = FALSE
      )
    }

    # Add CI if requested
    if (options[[paste0(base, "IndividualCi")]]) {
      ciType <- options[[paste0(base, "IndividualCiType")]]

      ciInterval <- if (ciType == "custom") {
        c(options[[paste0(base, "IndividualCiLower")]],
          options[[paste0(base, "IndividualCiUpper")]])
      } else {
        ciMass <- options[[paste0(base, "IndividualCiMass")]] / 100
        if (ciType == "central") {
          stats::quantile(predictiveSamples, probs = c((1 - ciMass) / 2, (1 + ciMass) / 2))
        } else if (ciType == "HPD") {
          # For HPD, we need HDInterval package or implement it
          if (requireNamespace("HDInterval", quietly = TRUE)) {
            HDInterval::hdi(predictiveSamples, credMass = ciMass)
          } else {
            # Fallback to central interval
            stats::quantile(predictiveSamples, probs = c((1 - ciMass) / 2, (1 + ciMass) / 2))
          }
        }
      }

      # Position errorbar above the histogram
      yPosition <- maxDensity * 1.1
      plt <- plt + ggplot2::geom_errorbarh(
        data = data.frame(x = mean(ciInterval), xmin = ciInterval[1], xmax = ciInterval[2], y = yPosition),
        ggplot2::aes(x = .data$x, xmin = .data$xmin, xmax = .data$xmax, y = .data$y),
        height = maxDensity * 0.05,
        linewidth = 0.75,
        inherit.aes = FALSE
      )
    }

    plot$plotObject <- plt
  }, error = function(e) {
    plot$setError(
      if (isPrior) gettextf("Unexpected error in prior predictive distribution plot: %s", e$message)
      else gettextf("Unexpected error in posterior predictive distribution plot: %s", e$message)
    )
  })
}
