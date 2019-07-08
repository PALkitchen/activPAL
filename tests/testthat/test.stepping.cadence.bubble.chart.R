context("Cadence Histogram")
library(activPAL)

test_that("generate_cadence_histograme", {

  input_directory <- paste(getwd(),"/",sep="")
  output_directory <- paste(tempdir(),"\\",sep="")

  daily.stepping.activity.bubble.chart(input_directory,output_directory)

  file_data <- list.files(output_directory,"Test__bubble_chart.png")
  expect_equal(length(file_data), 1)
  expect_equal(file_data, "Test__bubble_chart.png")
  file.remove(paste(output_directory,file_data,sep=""))

})
