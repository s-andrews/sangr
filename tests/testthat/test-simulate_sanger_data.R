library(sangr)

test_that("Simulation Parameters", {

  good_sequence <- "GATC"

  expect_output(str(simulate_sanger_data(good_sequence)),"tibble")
  expect_equal(ncol(simulate_sanger_data(good_sequence)),5)
  expect_equal(nrow(simulate_sanger_data(good_sequence)),100)

  expect_equal(nrow(simulate_sanger_data(good_sequence,sd = 10)),100)
  expect_equal(nrow(simulate_sanger_data(good_sequence,degrade = 0.1)),100)
  expect_equal(nrow(simulate_sanger_data(good_sequence,noise = 0.5)),100)

})

test_that("Invalid Parameters", {

  expect_error(simulate_sanger_data("gatc"))
  expect_error(simulate_sanger_data("jeyc"))
  expect_error(simulate_sanger_data(1234))
  expect_error(simulate_sanger_data("GATC",sd=-1))
  expect_error(simulate_sanger_data("GATC",degrade=2))
  expect_error(simulate_sanger_data("GATC",noise=-1))

})
