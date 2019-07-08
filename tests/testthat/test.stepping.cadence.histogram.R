context("Cadence Histogram")
library(activPAL)

test_that("generate_cadence_histograme", {

  input_directory <- paste(getwd(),"/",sep="")
  output_directory <- paste(tempdir(),"\\",sep="")

  file_data <- stepping.cadence.bands.folder.two.stepping.groups(input_directory,output_directory)
  expect_equal(nrow(file_data), 1)
  expect_equal(ncol(file_data), 3)

  file_data <- stepping.cadence.bands.folder.four.stepping.groups(input_directory,output_directory)
  expect_equal(nrow(file_data), 1)
  expect_equal(ncol(file_data), 3)

  file_data <- list.files(output_directory,"Test_-cadence-histogram.png")
  expect_equal(length(file_data), 1)
  expect_equal(file_data, "Test_-cadence-histogram.png")
  file.remove(paste(output_directory,file_data,sep=""))

  file_data <- list.files(output_directory,"median_cadence_summary*.csv")
  expect_equal(length(file_data), 1)
  expect_equal(file_data, "median_cadence_summary.csv")
  file.remove(paste(output_directory,file_data,sep=""))

})
