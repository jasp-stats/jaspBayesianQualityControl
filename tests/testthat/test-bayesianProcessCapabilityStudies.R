context("Bayesian Process Capability Studies")

datasetPath <- testthat::test_path("datasets", "processCapability.csv")

getOptions <- function(...) {
  options <- jaspTools::analysisOptions("bayesianProcessCapabilityStudies")
  options$measurement                        <- "measurement"
  options$capabilitySpecificationLowerLimit  <- "8"
  options$capabilitySpecificationTargetValue <- "10"
  options$capabilitySpecificationUpperLimit  <- "12"
  modifyList(options, list(...))
}

runBpcs <- function(options, dataset = datasetPath) {
  set.seed(1)
  jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, options, view = FALSE, quiet = TRUE)
}

getPlot <- function(results, name) {
  results[["state"]][["figures"]][[results[["results"]][[name]][["data"]]]][["obj"]]
}

footnotes <- function(table) {
  vapply(table[["footnotes"]], function(footnote) footnote[["text"]], character(1))
}

# Numerical integration with the default (DCSI) prior and all outputs that do not need MCMC ----
options <- getOptions(
  statisticsMetricsCp                                                 = TRUE,
  statisticsMetricsCpu                                                = TRUE,
  statisticsMetricsCpl                                                = TRUE,
  statisticsMetricsCpc                                                = TRUE,
  statisticsMetricsCpm                                                = TRUE,
  statisticsIntervalTable                                             = TRUE,
  timeSeriesPlot                                                      = TRUE,
  priorAndPosteriorInferencePosteriorDistribution                     = TRUE,
  priorAndPosteriorInferencePosteriorDistributionShowPriorDistribution = TRUE,
  statisticsProcessOverview                                           = TRUE,
  sequentialAnalysisPointEstimatePlot                                 = TRUE
)
results <- runBpcs(options)

test_that("Capability table results match", {
  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    0.892408, 1.11478, 1.11273, "Cp",  0.116536, 1.34885,
    0.878717, 1.12527, 1.12353, "Cpu", 0.128386, 1.38174,
    0.861537, 1.10430, 1.10260, "Cpl", 0.126380, 1.35671,
    0.841092, 1.07289, 1.07131, "Cpk", 0.120584, 1.31363,
    0.879366, 1.10150, 1.09942, "Cpc", 0.116463, 1.33551,
    0.879600, 1.10165, 1.09955, "Cpm", 0.116425, 1.33559
  ))
})

test_that("Interval table results match", {
  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(
    0.162688, 0.801978, 0.0345456, 0.000787773, 2.08779e-12, "Cp",
    0.164917, 0.777397, 0.0553538, 0.00233233,  1.74985e-08, "Cpu",
    0.206016, 0.754619, 0.0381345, 0.00123057,  1.61571e-08, "Cpl",
    0.275641, 0.706009, 0.0180258, 0.000324874, 5.26416e-13, "Cpk",
    0.193233, 0.779048, 0.0271703, 0.000548383, 1.06996e-12, "Cpc",
    0.192811, 0.779428, 0.0272111, 0.000549263, 1.06983e-12, "Cpm"
  ))
})

test_that("Time series plot matches", {
  jaspTools::expect_equal_plots(getPlot(results, "timeSeriesPlot"), "time-series")
})

test_that("Posterior distribution plot matches", {
  jaspTools::expect_equal_plots(getPlot(results, "priorAndPosteriorInferencePosteriorDistribution"), "posterior-distribution")
})

test_that("Process overview plot matches", {
  jaspTools::expect_equal_plots(getPlot(results, "statisticsProcessOverview"), "process-overview")
})

test_that("Sequential point estimate plot matches", {
  jaspTools::expect_equal_plots(getPlot(results, "sequentialAnalysisPointEstimatePlot"), "sequential-point-estimate")
})

# Other priors ----
test_that("Unit information prior gives the expected capability table", {
  results <- runBpcs(getOptions(priorDistribution = "unitInformation"))
  table   <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(0.814427, 1.05527, 1.05346, "Cpk", 0.125548, 1.30638))
})

test_that("Dependent custom prior gives the expected capability table", {
  results <- runBpcs(getOptions(
    priorDistribution                      = "custom",
    priorDistributionsCustomSpecification  = "dependent",
    priorDistributionsCustomDependentMeanMu = 10
  ))
  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(0.830006, 1.07148, 1.06969, "Cpk", 0.125848, 1.32312))
})

# MCMC draws differ across platforms, so only check that the outputs are filled ----
test_that("MCMC fit fills the capability and parameter tables", {
  results <- runBpcs(getOptions(
    advancedEstimationMethod       = "mcmc",
    advancedMcmcSettingsIterations = 1000L,
    advancedMcmcSettingsWarmup     = 500L,
    statisticsParameterEstimates   = TRUE
  ))
  expect_equal(results[["status"]], "complete")
  expect_length(results[["results"]][["bpcsCapabilityTable"]][["data"]], 1L)
  expect_length(results[["results"]][["parameterEstimates"]][["data"]], 2L)
})

test_that("Default prior is DCSI for both estimation methods", {
  mcmc <- runBpcs(getOptions(
    advancedEstimationMethod       = "mcmc",
    advancedMcmcSettingsIterations = 4000L,
    advancedMcmcSettingsWarmup     = 1000L
  ))
  integration <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  cpk <- function(table) Filter(function(row) row[["metric"]] == "Cpk", table)[[1L]]
  # Monte Carlo error of the posterior mean is about 0.002
  expect_equal(cpk(mcmc[["results"]][["bpcsCapabilityTable"]][["data"]])[["mean"]], cpk(integration)[["mean"]], tolerance = 0.02)

  studentT <- runBpcs(getOptions(
    dataDistribution               = "studentT",
    advancedEstimationMethod       = "mcmc",
    advancedMcmcSettingsIterations = 1000L,
    advancedMcmcSettingsWarmup     = 500L
  ))
  expect_equal(studentT[["status"]], "complete")
  expect_length(studentT[["results"]][["bpcsCapabilityTable"]][["data"]], 1L)
})

# Invalid input ----
test_that("Missing measurements are excluded with a footnote", {
  dataset <- read.csv(datasetPath)
  dataset$measurement[c(3, 7)] <- NA
  results <- runBpcs(getOptions(), dataset)
  table   <- results[["results"]][["bpcsCapabilityTable"]]
  expect_length(table[["data"]], 1L)
  expect_true("2 observations with missing measurements were excluded." %in% footnotes(table))
})

test_that("Infinite measurements give an error", {
  dataset <- read.csv(datasetPath)
  dataset$measurement[5] <- Inf
  results <- runBpcs(getOptions(), dataset)
  expect_match(results[["results"]][["bpcsCapabilityTable"]][["error"]][["errorMessage"]], "Measurements must be finite")
})

test_that("A lower limit above the upper limit gives an error", {
  results <- runBpcs(getOptions(capabilitySpecificationLowerLimit = "13"))
  expect_match(results[["results"]][["bpcsCapabilityTable"]][["error"]][["errorMessage"]], "`LSL` must be lower than `USL`")
})

test_that("Arithmetic specification limits are evaluated, other text is rejected", {
  results <- runBpcs(getOptions(capabilitySpecificationLowerLimit = "10 - 2*1"))
  jaspTools::expect_equal_tables(results[["results"]][["bpcsCapabilityTable"]][["data"]],
                                 list(0.841092, 1.07289, 1.07131, "Cpk", 0.120584, 1.31363))

  results <- runBpcs(getOptions(capabilitySpecificationLowerLimit = "abc"))
  expect_equal(results[["status"]], "validationError")
  expect_match(results[["results"]][["errorMessage"]], "Lower limit must be a finite numeric value")
})

test_that("An improper prior cannot be shown as a prior distribution", {
  results <- runBpcs(getOptions(priorDistribution = "jeffreys", priorAndPosteriorInferencePriorDistribution = TRUE))
  expect_length(results[["results"]][["bpcsCapabilityTable"]][["data"]], 1L)
  expect_match(results[["results"]][["priorAndPosteriorInferencePriorDistribution"]][["error"]][["errorMessage"]], "improper")
})

test_that("Process criteria that do not increase give an error", {
  options <- getOptions(statisticsIntervalTable = TRUE)
  options$processCriteria[[2]]$rightBound <- 0.5
  results <- runBpcs(options)
  expect_match(results[["results"]][["bpcsIntervalTable"]][["error"]][["errorMessage"]], "must be larger than the previous one")
})

test_that("Without metrics the capability table asks for one and sequential plots stay empty", {
  results <- runBpcs(getOptions(statisticsMetricsCpk = FALSE, sequentialAnalysisPointEstimatePlot = TRUE))
  table   <- results[["results"]][["bpcsCapabilityTable"]]
  expect_true("Select at least one capability metric under Statistics > Metrics." %in% footnotes(table))
  expect_null(getPlot(results, "sequentialAnalysisPointEstimatePlot"))
})
