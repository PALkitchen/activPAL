context("Activity Summary")
library(activPAL)

test_that("generate_activity_summary", {

  input_directory <- paste(getwd(),"/",sep="")
  output_directory <- paste(tempdir(),"\\",sep="")
  file_data <- activity.summary.by.window.duration(input_directory,output_directory,2)
  expect_equal(nrow(file_data), 12)
  expect_equal(ncol(file_data), 12)
  expect_equal(sum(file_data$steps), 2006)

  file_data <- list.files(output_directory,"summary*.csv")
  expect_equal(length(file_data), 1)
  expect_equal(file_data, "Test__2_hours_summary.csv")

  file_data <- list.files(output_directory,"activity_summary_all_files*.csv")
  expect_equal(length(file_data), 1)
  expect_equal(file_data, "activity_summary_all_files.csv")

})
