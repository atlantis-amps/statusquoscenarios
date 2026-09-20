test_that("bundled data files exist", {
  expect_true(file.exists(
    system.file("extdata", "PugetSound_89b_NAD83.bgm",
                package = "statusquoscenarios")
  ))
  expect_true(file.exists(
    system.file("extdata", "biom_comp_run_status_quo.csv",
                package = "statusquoscenarios")
  ))
  expect_true(file.exists(
    system.file("extdata", "NAA-WAA_sim.csv",
                package = "statusquoscenarios")
  ))
  expect_true(file.exists(
    system.file("extdata",
                "PugetSoundAtlantisFunctionalGroups_2024_V1_SQ.csv",
                package = "statusquoscenarios")
  ))
})

test_that("ensure_output_dir creates directory", {
  tmp <- file.path(tempdir(), "test_output_dir", "nested")
  on.exit(unlink(file.path(tempdir(), "test_output_dir"), recursive = TRUE))
  statusquoscenarios:::ensure_output_dir(tmp)
  expect_true(dir.exists(tmp))
})

test_that("build_nc_path returns correct string", {
  result <- statusquoscenarios:::build_nc_path("/data", 2011, "NH4")
  expect_equal(result,
               file.path("/data", "2011",
                         "pugetsound_SSM_Atlantis_NH4_status_quo_2011.nc"))
})

test_that("plot_biomass_heatmap writes a PDF", {
  skip_on_cran()
  tmp <- tempdir()
  out <- plot_biomass_heatmap(output_dir = tmp, output_file = "test_biom.pdf")
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("plot_naa_heatmap writes a PDF", {
  skip_on_cran()
  tmp <- tempdir()
  out <- plot_naa_heatmap(output_dir = tmp, output_file = "test_naa.pdf")
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("plot_salmon writes a PDF", {
  skip_on_cran()
  tmp <- tempdir()
  out <- plot_salmon(output_dir = tmp, output_file = "test_salmon.pdf")
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("plot_biomass_histogram writes a PDF", {
  skip_on_cran()
  tmp <- tempdir()
  out <- plot_biomass_histogram(output_dir = tmp,
                                output_file = "test_hist.pdf")
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})
