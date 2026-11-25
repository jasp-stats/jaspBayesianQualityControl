context("Other: runningExampleNormal")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("bayesianProcessCapabilityStudies (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormal.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.936552780628996, 1.16166111505186, 1.16021685045898, "Cpk",
     0.116981783832734, 1.39498212759317))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0817257229754818, 0.841367013008953, 0.074432360656316, 0.00247490334817244,
     1.10769215995781e-11, "Cpk"))

  plotName <- results[["results"]][["priorAndPosteriorInferencePosteriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_posterior-distribution")

  plotName <- results[["results"]][["priorAndPosteriorInferencePriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_prior-distribution")

})

test_that("bayesianProcessCapabilityStudies (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormal.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.936552780628996, 1.16166111505186, 1.16021685045898, "Cpk",
     0.116981783832734, 1.39498212759317))

  plotName <- results[["results"]][["priorAndPosteriorInferencePosteriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_posterior-distribution")

  plotName <- results[["results"]][["sequentialAnalysisPointEstimatePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_sequential-analysis-point-estimate")

})

test_that("bayesianProcessCapabilityStudies (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormal.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.936552780628996, 1.16166111505186, 1.16021685045898, "Cpk",
     0.116981783832734, 1.39498212759317))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0817257229754818, 0.841367013008953, 0.074432360656316, 0.00247490334817244,
     1.10769215995781e-11, "Cpk"))

  plotName <- results[["results"]][["priorAndPosteriorInferencePosteriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_posterior-distribution")

})

