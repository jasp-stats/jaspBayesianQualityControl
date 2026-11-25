context("Other: secom100")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("bayesianProcessCapabilityStudies results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "secom100.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("bayesianProcessCapabilityStudies", dataset, opts)

  table <- results[["results"]][["bpcsCapabilityTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1.2100253408453, 1.39480423719772, 1.39369923537574, "Cp", 0.0959079129247085,
     1.58586466749376, 1.30788232891701, 1.51947101944537, 1.51837636159009,
     "Cpu", 0.1095692343658, 1.7372791542878, 1.08968292292755, 1.27013745495007,
     1.26925682249706, "Cpl", 0.0933640863571912, 1.45559319337066,
     1.0896819950212, 1.27013535603235, 1.26925474925988, "Cpk",
     0.0933634443046671, 1.45558982403172, 1.12719675247719, 1.29950544571505,
     1.2983852010345, "Cpc", 0.0895731010507466, 1.47818220014559
    ))

  table <- results[["results"]][["bpcsIntervalTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.06624571068362e-06, 0.251748488563188, 0.611305091581569, 0.136939352349513,
     1.26001950467864e-09, "Cp", 2.80579149379255e-07, 0.0400618351780435,
     0.393266367407069, 0.566659797536288, 1.17192994492088e-05,
     "Cpu", 0.00148636224235124, 0.739509216732102, 0.25119348856929,
     0.00781092856530376, 3.89095211605906e-09, "Cpl", 0.00148639174268124,
     0.73951780090898, 0.251185691630124, 0.00781011182748578, 3.89072907225341e-09,
     "Cpk", 0.000236866289409822, 0.637011699840508, 0.348573913024023,
     0.0141775208458856, 1.73806541540969e-13, "Cpc"))

  plotName <- results[["results"]][["statisticsProcessOverview"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_process-overview")

})

