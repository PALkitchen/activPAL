context("Physical Behaviour Summary")
library(activPAL)

test_that("generate.physical.behaviour.summary", {

  input_directory_success_1 <- paste(system.file("extdata/EventsEx1", "", package = "activPAL"),"/",sep="")
  input_directory_errors <- paste(system.file("extdata/EventsEx3", "", package = "activPAL"),"/",sep="")
  output_directory <- paste(tempdir(),"/",sep="")

  generate.physical.behaviour.summary(input_directory_success_1, output_folder = output_directory,
                                      chart_title = "Test1", prefix_length = 5, minimum_wear_time = 1, sort_order = "PEAK_6_MIN_STEPPING")

})
