context("Load File")
library(activPAL)

test_that("file_loading", {

  file_data <- activPAL:::pre.process.events.file("Test_Events.csv")
  expect_equal(nrow(file_data), 179)
  expect_equal(length(which(is.na(file_data$time))), 0)
  expect_equal(sum(file_data$steps), 2006)
  expect_equal(sum(file_data$interval), 86400)
})
