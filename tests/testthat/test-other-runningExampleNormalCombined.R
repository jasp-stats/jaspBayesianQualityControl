context("Other: runningExampleNormalCombined")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("bayesianProcessCapabilityStudies (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormalCombined.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.981767353303742, 1.1103166604969, 1.10990896660887, "Cpk", 0.0661736901128862,
     1.24117487715284))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.046643756819501, 0.952798366065819, 0.000557870362643391, 6.75203620941898e-09,
     2.9202701080421e-36, "Cpk"))

  plotName <- results[["results"]][["priorAndPosteriorInferencePosteriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_posterior-distribution")

  plotName <- results[["results"]][["sequentialAnalysisPointEstimatePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_sequential-analysis-point-estimate")

})

test_that("bayesianProcessCapabilityStudies (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormalCombined.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.981767353303742, 1.1103166604969, 1.10990896660887, "Cpk", 0.0661736901128862,
     1.24117487715284))

  plotName <- results[["results"]][["sequentialAnalysisIntervalEstimatePlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_sequential-analysis-interval-estimate")

})

test_that("bayesianProcessCapabilityStudies (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormalCombined.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.981767353303742, 1.1103166604969, 1.10990896660887, "Cpk", 0.0661736901128862,
     1.24117487715284))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.046643756819501, 0.952798366065819, 0.000557870362643391, 6.75203620941898e-09,
     2.9202701080421e-36, "Cpk"))

  plotName <- results[["results"]][["priorAndPosteriorInferencePosteriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_posterior-distribution")

})

test_that("bayesianProcessCapabilityStudies (analysis 4) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormalCombined.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[4]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.976091886467251, 1.10616579970215, 1.10573814353003, "Cpk",
     0.0669817634605996, 1.23866778617231))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0553693963385773, 0.944109747278389, 0.000520849499620789, 6.88341246310378e-09,
     1.09766118384934e-35, "Cpk"))

  plotName <- results[["results"]][["priorAndPosteriorInferencePosteriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-4_figure-1_posterior-distribution")

})

test_that("bayesianProcessCapabilityStudies (analysis 5) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormalCombined.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[5]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.97183151297743, 1.10232829435213, 1.10189519356163, "Cpk", 0.067205317022892,
     1.23528171456138))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0628315306331212, 0.936722270317286, 0.000446193588322713, 5.46127009175901e-09,
     1.02076660081112e-35, "Cpk"))

  plotName <- results[["results"]][["priorAndPosteriorInferencePosteriorDistribution"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-5_figure-1_posterior-distribution")

})

test_that("bayesianProcessCapabilityStudies (analysis 6) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "runningExampleNormalCombined.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[6]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.00796923719389, 1.13191467761715, 1.13130684187651, "Cp", 0.0641332118498416,
     1.25931612536687, 0.995796895688376, 1.1305971898196, 1.13008183385778,
     "Cpu", 0.0695305242657707, 1.26832286207799, 0.998166188684327,
     1.13323216541471, 1.13271515977239, "Cpl", 0.06966809433525,
     1.2712358128727, 0.981767353303742, 1.1103166604969, 1.10990896660887,
     "Cpk", 0.0661736901128862, 1.24117487715284, 1.00428843523598,
     1.12822461108113, 1.12761456702639, "Cpc", 0.0641317734221949,
     1.25562933653065))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0183368426401196, 0.980379969522416, 0.0012831676519259, 2.01855387073824e-08,
     0, "Cp", 0.0288690570867892, 0.968715312067802, 0.00241547796662359,
     1.44951875205999e-07, 7.9269103503421e-09, "Cpu", 0.0266284003484813,
     0.970620840465126, 0.00275056823564467, 1.82188200741829e-07,
     8.76254779846875e-09, "Cpl", 0.046643756819501, 0.952798366065819,
     0.000557870362643391, 6.75203620941898e-09, 2.9202701080421e-36,
     "Cpk", 0.0212001556510291, 0.977728520014702, 0.00107130929082756,
     1.50434412555818e-08, 0, "Cpc"))

})

