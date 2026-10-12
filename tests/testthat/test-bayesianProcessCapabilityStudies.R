context("[Bayesian Quality Control] Bayesian Process Capability Study")

# jaspTools cannot expand the Common.PlotLayout / Common.Priors components used by
# inst/qml/BayesianProcessCapabilityStudies.qml, and its qml parser stops with an error on
# dropdowns whose values are computed (the process overview threshold and reference prior).
# analysisOptions() is therefore not used; these helpers supply every option with its qml default.
.plotLayoutOptions <- function(base, checked = FALSE) {
  o <- list()
  o[[base]]                                       <- checked
  o[[paste0(base, "IndividualPointEstimate")]]     <- FALSE
  o[[paste0(base, "IndividualPointEstimateType")]] <- "mean"
  o[[paste0(base, "IndividualCi")]]                <- FALSE
  o[[paste0(base, "IndividualCiType")]]            <- "central"
  o[[paste0(base, "IndividualCiMass")]]            <- 95
  o[[paste0(base, "IndividualCiLower")]]           <- 0
  o[[paste0(base, "IndividualCiUpper")]]           <- 1
  o[[paste0(base, "IndividualCiBf")]]              <- "1"
  o[[paste0(base, "TypeLower")]]                   <- 0
  o[[paste0(base, "TypeUpper")]]                   <- 1
  o[[paste0(base, "PanelLayout")]]                 <- "multiplePanels"
  o[[paste0(base, "Axes")]]                        <- "free"
  o[[paste0(base, "custom_x_min")]]                <- 0
  o[[paste0(base, "custom_x_max")]]                <- 1
  o[[paste0(base, "custom_y_min")]]                <- 0
  o[[paste0(base, "custom_y_max")]]                <- 1
  o[[paste0(base, "PriorDistribution")]]           <- FALSE
  o
}

.plotBases <- c("posteriorDistributionPlot", "priorDistributionPlot",
                "sequentialAnalysisPointEstimatePlot", "sequentialAnalysisPointIntervalPlot",
                "posteriorPredictiveDistributionPlot", "priorPredictiveDistributionPlot")

# Default options for the analysis, with spec limits matched to datasets/processCapability.csv
# (40 observations, roughly normal around 10 with sd 0.5).
.bpcsProcessCriteria <- function(upper = c(1, 1.33, 1.5, 2),
                                 labels = c("Incapable", "Capable", "Satisfactory", "Excellent", "Super")) {
  lower <- c(-Inf, upper)
  upper <- c(upper, Inf)
  lapply(seq_along(labels), function(i) list(lower = lower[i], label = labels[i], upper = upper[i]))
}

.bpcsQmlDefaults <- function() {
  list(
    measurementLongFormat             = "",
    capabilityStudyType               = "normalCapabilityAnalysis",
    Cp = TRUE, Cpu = TRUE, Cpl = TRUE, Cpk = TRUE, Cpc = TRUE, Cpm = TRUE,
    lowerSpecificationLimit           = FALSE,
    lowerSpecificationLimitValue      = -1,
    target                            = FALSE,
    targetValue                       = 0,
    upperSpecificationLimit           = FALSE,
    upperSpecificationLimitValue      = 1,
    timeSeriesPlot                    = FALSE,
    processCriteria                   = .bpcsProcessCriteria(),
    processOverview                   = FALSE,
    processOverviewMetric             = "Cpk",
    # the threshold dropdown defaults to its second entry, the second boundary
    processOverviewThreshold          = "1.33",
    processOverviewReferencePrior     = "DCSI",
    processOverviewBinning            = "noBins",
    processOverviewNumberOfBins       = 5,
    processOverviewObservationsPerBin = 10,
    intervalTable                     = FALSE,
    credibleIntervalWidth             = 0.95,
    sequentialAnalysisPlotAdditionalInfo = TRUE,
    sequentialAnalysisUpdatingTable   = FALSE,
    priorSettings                     = "default",
    noIterations                      = 5000,
    noWarmup                          = 1000,
    noChains                          = 1,
    plotWidth                         = 480,
    plotHeight                        = 320
  )
}

.bpcsOptions <- function() {
  options <- .bpcsQmlDefaults()
  extra   <- c(do.call(c, lapply(.plotBases, .plotLayoutOptions)),
               list(axisLabels = FALSE, normalModelComponentsList = list(), tModelComponentsList = list()))
  options[names(extra)] <- extra

  options$capabilityStudyType          <- "normalCapabilityAnalysis"
  options$measurementLongFormat        <- "measurement"
  options$priorSettings                <- "default"
  options$lowerSpecificationLimit      <- TRUE
  options$lowerSpecificationLimitValue <- 8.5
  options$target                       <- TRUE
  options$targetValue                  <- 10
  options$upperSpecificationLimit      <- TRUE
  options$upperSpecificationLimitValue <- 11.5
  # keep the sampler cheap, this analysis refits per observation in the sequential plots
  options$noChains                     <- 1
  options$noWarmup                     <- 200
  options$noIterations                 <- 1000
  options
}

.capabilityRows <- function(results) {
  rows <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}

## Capability table ####

options <- .bpcsOptions()
set.seed(1)
results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

test_that("Analysis runs to completion", {
  expect_equal(results[["status"]], "complete")
  expect_null(results[["results"]][["errorMessage"]])
})

test_that("Capability table reports every metric the user selected", {
  # regression: qc names these Cpu/Cpl and errors on CpU/CpL, mismatched casing
  # silently dropped both metrics from the table
  expect_equal(.capabilityRows(results)$metric, c("Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm"))
})

test_that("Capability table estimates are plausible for a well centred process", {
  df <- .capabilityRows(results)
  # LSL 8.5, USL 11.5, sd about 0.5 => Cp near 1
  expect_equal(df$mean[df$metric == "Cp"], 1.0, tolerance = 0.25)
  # the sampler makes these stochastic, so only assert the ordering that must hold
  expect_true(all(df$lower < df$mean))
  expect_true(all(df$mean  < df$upper))
  expect_true(df$mean[df$metric == "Cpk"] <= df$mean[df$metric == "Cp"])
})

test_that("Deselecting metrics removes them from the table", {
  options <- .bpcsOptions()
  options$Cpu <- FALSE
  options$Cpl <- FALSE
  options$Cpc <- FALSE
  options$Cpm <- FALSE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)
  expect_equal(.capabilityRows(results)$metric, c("Cp", "Cpk"))
})

## Estimation ####

test_that("Estimates are deterministic across runs", {
  # qc::bpc defaults to method = "integration", so the fit is numerical rather
  # than sampled and repeated runs must agree exactly.
  #
  # NOTE: this is also why the MCMC Settings group in the qml currently has no
  # effect on the capability table. noChains/noWarmup/noIterations are passed to
  # qc::bpc (they used to be ignored entirely) but only take effect on the mcmc
  # path, which the analysis never selects because there is no qml control for
  # the estimation method. Either add that control or drop the settings group.
  options <- .bpcsOptions()
  set.seed(1)
  first  <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)
  set.seed(2)
  second <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(.capabilityRows(first)$mean, .capabilityRows(second)$mean)
  expect_equal(.capabilityRows(first)$sd,   .capabilityRows(second)$sd)
})

## Plots ####

test_that("Distribution and predictive plots are produced without error", {
  options <- .bpcsOptions()
  options$posteriorDistributionPlot           <- TRUE
  options$priorDistributionPlot               <- TRUE
  options$posteriorPredictiveDistributionPlot <- TRUE
  options$priorPredictiveDistributionPlot     <- TRUE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  for (base in c("posteriorDistributionPlot", "priorDistributionPlot",
                 "posteriorPredictiveDistributionPlot", "priorPredictiveDistributionPlot")) {
    plotName <- results[["results"]][[base]][["data"]]
    expect_true(!is.null(plotName), info = base)
  }
  expect_length(results[["state"]][["figures"]], 4)
})

## Sequential analysis ####

test_that("Sequential analysis plots are produced without error", {
  # slow, the sequential analysis refits once per observation
  options <- .bpcsOptions()
  options$sequentialAnalysisPointEstimatePlot <- TRUE
  options$sequentialAnalysisPointIntervalPlot <- TRUE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_true(!is.null(results[["results"]][["sequentialAnalysisPointEstimatePlot"]][["data"]]))
  expect_true(!is.null(results[["results"]][["sequentialAnalysisPointIntervalPlot"]][["data"]]))
  expect_length(results[["state"]][["figures"]], 2)
})

test_that("Posterior updating table option is not implemented yet", {
  # the qml ships a "Posterior updating table" checkbox (marked TODO) with no R
  # implementation, so ticking it adds nothing. Guards against the option being
  # quietly forgotten: delete this test when the table is implemented.
  options <- .bpcsOptions()
  options$sequentialAnalysisPointEstimatePlot <- TRUE
  options$sequentialAnalysisUpdatingTable     <- TRUE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_false("sequentialAnalysisUpdatingTable" %in% names(results[["results"]]))
})

## Interval table ####

test_that("Interval table is produced without error", {
  options <- .bpcsOptions()
  options$intervalTable <- TRUE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_true(length(results[["results"]][["bpcsIntervalTable"]][["data"]]) > 0)
})

## Readiness ####

test_that("Analysis stays empty until the specification limits are set", {
  options <- .bpcsOptions()
  options$lowerSpecificationLimit <- FALSE
  options$upperSpecificationLimit <- FALSE
  options$target                  <- FALSE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_length(.capabilityRows(results), 0)
})

## Process criteria and overview helpers ####

.bpcsDataset <- function() {
  read.csv(testthat::test_path("datasets", "processCapability.csv"))
}

.bpcsResultError <- function(results, name) {
  error <- results[["results"]][[name]][["error"]]
  if (is.list(error)) error[["errorMessage"]] else paste(error, collapse = " ")
}

test_that("process overview sample sizes are bounded and include the final observation", {
  noBins <- function(bins) list(processOverviewBinning = "noBins", processOverviewNumberOfBins = bins)

  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(3, noBins(5)), 3)
  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(10, noBins(5)), c(3, 5, 6, 8, 10))

  sampleSizes <- jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(100, noBins(5))
  expect_equal(length(sampleSizes), 5)
  expect_equal(tail(sampleSizes, 1), 100)

  expect_equal(
    jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(10,
      list(processOverviewBinning = "perObservation")), 3:10
  )
})

test_that("process overview checkpoints follow the cumulative binning semantics", {
  perBin <- function(size) list(processOverviewBinning = "observationsPerBin", processOverviewObservationsPerBin = size)

  # observations are added between checkpoints after the initial 3, and the final observation is appended once
  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(30, perBin(10)), c(3, 13, 23, 30))
  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(23, perBin(10)), c(3, 13, 23))
  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(3, perBin(10)), 3)

  # one bin is the single full-data checkpoint, rounding duplicates are removed
  oneBin <- list(processOverviewBinning = "noBins", processOverviewNumberOfBins = 1)
  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(30, oneBin), 30)
  manyBins <- list(processOverviewBinning = "noBins", processOverviewNumberOfBins = 20)
  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(6, manyBins), 3:6)

  expect_error(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(2, oneBin), "at least 3 observations")
  expect_error(jaspBayesianQualityControl:::.bpcsOverviewSampleSizes(10, list()), "Unknown process overview binning")
})

test_that("process overview uses the selected process criterion", {
  options <- list(
    processCriteria = .bpcsProcessCriteria(),
    processOverviewThreshold = "1.33"
  )
  criteria <- jaspBayesianQualityControl:::.bpcsProcessCriteria(options)

  expect_equal(criteria$values, c(1, 1.33, 1.5, 2))
  expect_equal(jaspBayesianQualityControl:::.bpcsOverviewThresholdIndex(options, criteria), 2)
  # the threshold is the numeric boundary as a string; the old "intervalN" aliases are not supported
  expect_error(
    jaspBayesianQualityControl:::.bpcsOverviewThresholdIndex(
      modifyList(options, list(processOverviewThreshold = "interval2")), criteria
    ),
    "not one of the process criteria boundaries"
  )
})

test_that("process criteria regions are connected through the right bounds", {
  # the qml only displays the left bounds, each region starts at the right bound of the region above
  criteria <- jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = list(
    list(lower = -Inf, label = "Incapable", upper = 1),
    list(lower = 1.1,  label = "Capable",   upper = Inf)
  )))
  expect_equal(criteria$lower, c(-Inf, 1))
  expect_equal(criteria$upper, c(1, Inf))

  # rows added in the qml have no left bound until the list synchronizes them
  criteria <- jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = list(
    list(label = "Low",    upper = 1),
    list(label = "Medium", upper = 2),
    list(label = "High",   upper = NULL)
  )))
  expect_equal(criteria$lower, c(-Inf, 1, 2))
  expect_equal(criteria$values, c(1, 2))

  expect_error(
    jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = .bpcsProcessCriteria(c(1.5, 1), c("A", "B", "C")))),
    "larger than the previous one"
  )
  expect_error(
    jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = list(
      list(label = "A", upper = "#"),
      list(label = "B", upper = Inf)
    ))),
    "must be numeric"
  )
})

test_that("process criteria treat the outer bounds as open-ended", {
  # after deleting the first or last row the hidden outer bound can hold a stale finite value
  criteria <- jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = list(
    list(lower = 1,   label = "Low",  upper = 1.5),
    list(lower = 1.5, label = "High", upper = 2)
  )))
  expect_equal(criteria$lower, c(-Inf, 1.5))
  expect_equal(criteria$upper, c(1.5, Inf))
  expect_equal(criteria$values, 1.5)

  expect_error(
    jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = .bpcsProcessCriteria(1, c("Low", " ")))),
    "classification label"
  )
  expect_error(
    jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = .bpcsProcessCriteria(numeric(), "Only"))),
    "at least two"
  )
  expect_error(
    jaspBayesianQualityControl:::.bpcsProcessCriteria(list(processCriteria = .bpcsProcessCriteria(c(1, 1), c("A", "B", "C")))),
    "larger than the previous one"
  )
})

test_that("process overview uses the selected likelihood", {
  expect_equal(
    jaspBayesianQualityControl:::.bpcsDistributionFromOptions(list(capabilityStudyType = "normalCapabilityAnalysis")),
    "normal"
  )
  expect_equal(
    jaspBayesianQualityControl:::.bpcsDistributionFromOptions(list(capabilityStudyType = "tCapabilityAnalysis")),
    "t"
  )
})

test_that("exceedance probabilities are accumulated above each criterion", {
  intervalSummary <- data.frame(
    metric = c("Cp", "Cpk"),
    below = c(0, 0.1),
    interval1 = c(0, 0.2),
    interval2 = c(0, 0.3),
    interval3 = c(0, 0.15),
    above = c(0, 0.25)
  )

  expect_equal(
    jaspBayesianQualityControl:::.bpcsExceedanceProbabilities(intervalSummary, "Cpk", c(1, 4 / 3, 1.5, 2)),
    c(0.9, 0.7, 0.4, 0.25)
  )
})

test_that("process overview probability lengths match the configured thresholds", {
  intervalSummary <- data.frame(
    metric = "Cpk",
    below = 0.1,
    above = 0.9
  )

  expect_error(
    jaspBayesianQualityControl:::.bpcsExceedanceProbabilities(intervalSummary, "Cpk", c(1, 1.33)),
    "Expected 3 interval probabilities"
  )
})

test_that("the sequential criteria axis supports any number of regions", {
  axisFor <- function(gridLines, labels) {
    jaspBayesianQualityControl:::.bpcsSequentialCriteriaAxis(c(0, 3), gridLines, labels)
  }

  two <- axisFor(1.33, c("Bad", "Good"))
  expect_equal(two$breaks, c(0, (0 + 1.33) / 2, 1.33, (3 + 1.33) / 2, 3))
  expect_equal(two$labels, c("", "Bad", "", "Good", ""))

  seven <- axisFor(c(0.5, 1, 1.33, 1.5, 2, 2.5), paste0("R", 1:7))
  expect_length(seven$breaks, 2 * 6 + 3)
  expect_equal(seven$labels[seq(2, 14, 2)], paste0("R", 1:7))
  expect_equal(seven$breaks[seq(3, 13, 2)], c(0.5, 1, 1.33, 1.5, 2, 2.5))
})

## Priors ####

.bpcsComponent <- function(name, type, ...) {
  c(list(name = name, type = type, truncationLower = -Inf, truncationUpper = Inf), list(...))
}

test_that("Student-t custom priors come from the Student-t prior list", {
  options <- list(
    capabilityStudyType = "tCapabilityAnalysis",
    priorSettings       = "customInformative",
    normalModelComponentsList = list(
      .bpcsComponent("mean",  "normal", mu = -50, sigma = 1),
      .bpcsComponent("sigma", "invgamma", alpha = 1, beta = 1, truncationLower = 0)
    ),
    tModelComponentsList = list(
      .bpcsComponent("mean",  "normal", mu = 10, sigma = 2),
      .bpcsComponent("sigma", "invgamma", alpha = 2, beta = 0.5, truncationLower = 0),
      .bpcsComponent("df",    "gammaAB", alpha = 2, beta = 0.1, truncationLower = 0)
    )
  )

  prior <- jaspBayesianQualityControl:::.bpcsPriorHelper(options)
  expect_setequal(names(prior$parameters), c("mu", "sigma", "nu"))
  expect_equal(prior$parameters$mu$parameters$mean, 10)
  expect_equal(prior$parameters$nu$distribution, "gamma")

  # a missing component is an error instead of a silent fallback to another list or a qc default
  options$tModelComponentsList <- options$tModelComponentsList[1:2]
  expect_error(jaspBayesianQualityControl:::.bpcsPriorHelper(options), "df")
})

test_that("prior sampling is only attempted for proper priors", {
  isProper <- jaspBayesianQualityControl:::.bpcsPriorIsProper
  normal <- list(capabilityStudyType = "normalCapabilityAnalysis", priorSettings = "default")

  expect_true(isProper(normal))
  expect_false(isProper(modifyList(normal, list(capabilityStudyType = "tCapabilityAnalysis"))))

  custom <- modifyList(normal, list(priorSettings = "customInformative"))
  custom$normalModelComponentsList <- list(
    .bpcsComponent("mean",  "normal", mu = 10, sigma = 2),
    .bpcsComponent("sigma", "invgamma", alpha = 2, beta = 0.5, truncationLower = 0)
  )
  expect_true(isProper(custom))

  custom$normalModelComponentsList[[1]] <- .bpcsComponent("mean", "jeffreys")
  expect_false(isProper(custom))
})

test_that("Student-t fits use MCMC with the user's settings", {
  options <- .bpcsOptions()
  options$capabilityStudyType <- "tCapabilityAnalysis"
  options$noIterations <- 400
  options$noWarmup     <- 100

  fit <- jaspBayesianQualityControl:::.bpcsFit(.bpcsDataset()[[1L]], options)
  expect_equal(fit$method, "mcmc")
  expect_equal(fit$distribution, "t")
  expect_equal(fit$stanfit@sim$iter, 400)
})

## Process overview data ####

test_that("sensitivity results keep a threshold-by-prior matrix with two regions", {
  options <- .bpcsOptions()
  options$processCriteria <- .bpcsProcessCriteria(1.33, c("Bad", "Good"))
  options$processOverviewReferencePrior <- "Jeffreys"
  criteria <- jaspBayesianQualityControl:::.bpcsProcessCriteria(options)
  x <- .bpcsDataset()
  fit <- list(rawfit = jaspBayesianQualityControl:::.bpcsFit(x[[1L]], options))

  sensitivity <- jaspBayesianQualityControl:::.bpcsComputeOverviewReferenceData(x, options, fit, criteria)
  probabilities <- sensitivity$probabilities[["Cpk"]]
  expect_equal(dim(probabilities), c(1L, 2L))
  expect_equal(colnames(probabilities), c("DCSI prior (active)", "Jeffreys prior"))
  expect_true(all(probabilities >= 0 & probabilities <= 1))
  expect_setequal(unique(sensitivity$densities$prior), colnames(probabilities))
  expect_equal(sensitivity$unavailable, "")

  # identical active and reference priors reuse the active fit under both labels
  options$processOverviewReferencePrior <- "DCSI"
  identical <- jaspBayesianQualityControl:::.bpcsComputeOverviewReferenceData(x, options, fit, criteria)
  expect_equal(colnames(identical$probabilities[["Cpk"]]), c("DCSI prior (active)", "DCSI prior"))
  expect_equal(unname(identical$probabilities[["Cpk"]][, 1]), unname(identical$probabilities[["Cpk"]][, 2]))
})

test_that("a failed reference fit is reported without removing the active prior", {
  options <- .bpcsOptions()
  options$processOverviewReferencePrior <- "Jeffreys"
  criteria <- jaspBayesianQualityControl:::.bpcsProcessCriteria(options)
  x <- .bpcsDataset()
  fit <- list(rawfit = jaspBayesianQualityControl:::.bpcsFit(x[[1L]], options))

  local_mocked_bindings(.bpcsFit = function(...) stop("injected failure"), .package = "jaspBayesianQualityControl")
  sensitivity <- jaspBayesianQualityControl:::.bpcsComputeOverviewReferenceData(x, options, fit, criteria)
  expect_equal(dim(sensitivity$probabilities[["Cpk"]]), c(4L, 1L))
  expect_equal(colnames(sensitivity$probabilities[["Cpk"]]), "DCSI prior (active)")
  expect_match(sensitivity$unavailable, "Jeffreys prior could not be fitted: injected failure")
})

test_that("failed sequential checkpoints are stored as missing", {
  options <- .bpcsOptions()
  options$processOverviewBinning <- "perObservation"
  criteria <- jaspBayesianQualityControl:::.bpcsProcessCriteria(options)
  x <- .bpcsDataset()[1:12, , drop = FALSE]
  realFit <- jaspBayesianQualityControl:::.bpcsFit

  local_mocked_bindings(.bpcsFit = function(x, ...) {
    if (length(x) %in% c(5, 12)) stop("injected failure")
    realFit(x, ...)
  }, .package = "jaspBayesianQualityControl")
  sequential <- jaspBayesianQualityControl:::.bpcsComputeOverviewSequentialData(x, options, criteria)

  expect_equal(sequential$sampleSizes, 3:12)
  expect_equal(which(sequential$failed), c(3L, 10L))
  expect_true(all(is.na(sequential$probabilities[["Cpk"]][c(3, 10), ])))
  expect_false(anyNA(sequential$probabilities[["Cpk"]][-c(3, 10), ]))
})

.bpcsSequentialPanel <- function(failed) {
  n <- length(failed)
  probabilities <- matrix(seq(0.1, 0.9, length.out = n), nrow = n, ncol = 1L)
  probabilities[failed, ] <- NA
  sequential <- list(
    sampleSizes   = seq(3, length.out = n),
    probabilities = list(Cpk = probabilities),
    failed        = failed
  )
  jaspBayesianQualityControl:::.bpcsMakeOverviewSequentialPanel(
    sequential, "Cpk", 1L, priorFit = NULL, criteria = NULL, title = "C"
  )
}

test_that("the sequential panel tolerates up to 10% failed checkpoints without bridging gaps", {
  failed <- rep(FALSE, 10)
  failed[4] <- TRUE
  panel <- .bpcsSequentialPanel(failed)

  expect_equal(panel$data$observation, c(3:5, 7:12))
  expect_equal(panel$data$segment, c(1, 1, 1, 2, 2, 2, 2, 2, 2))
  expect_match(panel$labels$caption, "1 of 10 sequential updates failed")
  expect_no_match(panel$labels$caption, "final probability")
})

test_that("the sequential panel is unavailable above 10% failed checkpoints", {
  failed <- rep(FALSE, 10)
  failed[c(2, 5)] <- TRUE
  panel <- .bpcsSequentialPanel(failed)

  expect_length(panel$layers, 1L)
  expect_match(panel$layers[[1L]]$aes_params$label, "2 of 10 sequential updates failed")
})

test_that("a failed final checkpoint is reported instead of labelling an earlier value as final", {
  failed <- rep(FALSE, 10)
  failed[10] <- TRUE
  panel <- .bpcsSequentialPanel(failed)

  expect_equal(max(panel$data$observation), 11)
  expect_match(panel$labels$caption, "final probability is unavailable")
})

## Process overview and time series ####

test_that("Time series plot is shown independently of the specification limits", {
  options <- .bpcsOptions()
  options$timeSeriesPlot          <- TRUE
  options$lowerSpecificationLimit <- FALSE
  options$upperSpecificationLimit <- FALSE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  plotName <- results[["results"]][["timeSeriesPlot"]][["data"]]
  expect_true(!is.null(plotName))
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  # only the target is drawn as a reference line
  expect_equal(ggplot2::ggplot_build(plot)$data[[1L]]$yintercept, 10)
  expect_equal(plot$labels$y, "Measurements")
  expect_length(.capabilityRows(results), 0)
})

test_that("Process overview is produced for the default criteria", {
  options <- .bpcsOptions()
  options$processOverview <- TRUE
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  plotName <- results[["results"]][["processOverview"]][["data"]]
  expect_true(!is.null(plotName))
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  expect_s3_class(plot, "patchwork")
  expect_length(plot, 4L)
})

test_that("Process overview and interval table follow two and more than five regions", {
  options <- .bpcsOptions()
  options$processOverview <- TRUE
  options$intervalTable   <- TRUE
  options$processOverviewReferencePrior <- "Jeffreys"

  options$processCriteria <- .bpcsProcessCriteria(1.33, c("Bad", "Good"))
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)
  expect_equal(results[["status"]], "complete")
  expect_true(!is.null(results[["results"]][["processOverview"]][["data"]]))
  titles <- vapply(results[["results"]][["bpcsIntervalTable"]][["schema"]][["fields"]], `[[`, character(1), "title")
  expect_length(titles, 3)
  expect_match(titles[2], "^Bad")
  expect_match(titles[3], "^Good")

  options$processCriteria <- .bpcsProcessCriteria(c(0.5, 1, 1.33, 1.5, 2, 2.5), paste0("R", 1:7))
  options$processOverviewThreshold <- "2.5"
  options$processOverviewBinning   <- "observationsPerBin"
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)
  expect_equal(results[["status"]], "complete")
  expect_true(!is.null(results[["results"]][["processOverview"]][["data"]]))
  expect_length(results[["results"]][["bpcsIntervalTable"]][["schema"]][["fields"]], 8)
})

test_that("Invalid process criteria only affect the outputs that use them", {
  options <- .bpcsOptions()
  options$processOverview <- TRUE
  options$intervalTable   <- TRUE
  options$timeSeriesPlot  <- TRUE
  # equal boundaries, which the qml marks as an error before they reach R
  options$processCriteria[[2]]$upper <- options$processCriteria[[1]]$upper
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_match(.bpcsResultError(results, "bpcsIntervalTable"), "larger than the previous one")
  expect_match(.bpcsResultError(results, "processOverview"), "larger than the previous one")
  expect_length(.capabilityRows(results)$metric, 6)
  expect_true(!is.null(results[["results"]][["timeSeriesPlot"]][["data"]]))
})

test_that("An improper prior only disables the prior-based outputs", {
  options <- .bpcsOptions()
  options$priorSettings <- "customInformative"
  options$normalModelComponentsList <- list(
    .bpcsComponent("mean",  "jeffreys"),
    .bpcsComponent("sigma", "invgamma", alpha = 1, beta = 0.15, truncationLower = 0)
  )
  options$posteriorDistributionPlot                  <- TRUE
  options$posteriorDistributionPlotPriorDistribution <- TRUE
  options$priorDistributionPlot                      <- TRUE
  options$priorPredictiveDistributionPlot            <- TRUE
  options$processOverview                            <- TRUE
  options$processOverviewReferencePrior              <- "Jeffreys"
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_length(.capabilityRows(results)$metric, 6)
  expect_true(!is.null(results[["results"]][["posteriorDistributionPlot"]][["data"]]))
  expect_true(!is.null(results[["results"]][["processOverview"]][["data"]]))
  expect_match(.bpcsResultError(results, "priorDistributionPlot"), "improper")
  expect_match(.bpcsResultError(results, "priorPredictiveDistributionPlot"), "improper")
})

test_that("Student-t analysis produces the capability table and process overview", {
  options <- .bpcsOptions()
  options$capabilityStudyType <- "tCapabilityAnalysis"
  options$processOverview <- TRUE
  # the Student-t model only offers the Jeffreys reference prior
  options$processOverviewReferencePrior <- "Jeffreys"
  # a single full-data checkpoint keeps the number of MCMC fits small
  options$processOverviewNumberOfBins   <- 1
  options$noWarmup                      <- 100
  options$noIterations                  <- 400
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  expect_equal(.capabilityRows(results)$metric, c("Cp", "Cpu", "Cpl", "Cpk", "Cpc", "Cpm"))
  expect_true(!is.null(results[["results"]][["processOverview"]][["data"]]))
})

test_that("Sequential point estimate plot uses the configured criteria", {
  # slow, the sequential analysis refits once per observation
  options <- .bpcsOptions()
  options$sequentialAnalysisPointEstimatePlot            <- TRUE
  options$sequentialAnalysisPointEstimatePlotPanelLayout <- "singlePanel"
  options$processCriteria <- .bpcsProcessCriteria(c(0.5, 1, 1.33, 1.5, 2, 2.5), paste0("R", 1:7))
  set.seed(1)
  results <- runAnalysis("bayesianProcessCapabilityStudies", "datasets/processCapability.csv", options)

  expect_equal(results[["status"]], "complete")
  plotName <- results[["results"]][["sequentialAnalysisPointEstimatePlot"]][["data"]]
  expect_true(!is.null(plotName))
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  secondary <- plot$scales$get_scales("y")$secondary.axis
  expect_equal(secondary$labels[seq(2, 14, 2)], paste0("R", 1:7))
})
