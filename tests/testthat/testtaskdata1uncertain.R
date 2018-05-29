library(projmanr)
library(igraph)
library(tidyr)
library(R6)
library(ggplot2)

context("Critican Path on taskdata1uncertain")

test_that("Check the return size of simulation", {
  res <- simulation(taskdatauncertain1, 100)
  expect_equal(length(res), 3)
  expect_equal(length(res$critical_indexes), 100)
  expect_equal(nrow(res$critical_indexes), 14)
  
  # Run the same tests, change the itr parameter to 
  # ensure that it's working
  res <- simulation(taskdatauncertain1, 1000)
  expect_equal(length(res), 3)
  expect_equal(length(res$critical_indexes), 1000)
  expect_equal(nrow(res$critical_indexes), 14)
})



# The following are the same tests from 'testtaskdata2.R'
# ensuring that the introduction of the uncertain columns did
# not cause any issues

test_that("Correct critical path", {
  res <- critical_path(taskdatauncertain1)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("12", "2", "4", "14", "15"))
  expect_equal(length(res), 5)
  expect_equal(res$total_duration, 23)
  expect_equal(nrow(res$results), nrow(taskdatauncertain1))
})

test_that("Correct critical path is computed with gantt", {
  res <- critical_path(taskdatauncertain1, gantt = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("12", "2", "4", "14", "15"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 23)
  expect_equal(nrow(res$results), nrow(taskdatauncertain1))
})

test_that("Correct critical path is computed with network diagram", {
  res <- critical_path(taskdatauncertain1, network = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("12", "2", "4", "14", "15"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 23)
  expect_equal(nrow(res$results), nrow(taskdatauncertain1))
})

test_that("Correct critical path is computed with both graph", {
  res <- critical_path(taskdatauncertain1, gantt = T, network = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("12", "2", "4", "14", "15"))
  expect_equal(length(res), 7)
  expect_equal(res$total_duration, 23)
  expect_equal(nrow(res$results), nrow(taskdatauncertain1))
})

test_that("Check that both errors work", {
  # Check for I.D Validation
  data <- taskdata1
  data[, 4] <- as.character(data[, 4])
  data[2,4] <- "1,31"
  expect_error(critical_path(data), "Invalid predeccessor id. Using a predeccessor id for a task that does not exist.")
  
  # Check for non-negative duration
  data <- taskdata1
  data[1,3] <- -1
  expect_error(critical_path(data), "Durations must be non-negative")
  
})

test_that("Date output is working correctly", {
  res <- critical_path(taskdatauncertain1, gantt = T, network = T, start_date = "2017-10-7")
  expect_equal(res$end_date, as.Date("2017-10-30"))
  
  res <- critical_path(taskdatauncertain1, gantt = T, network = T)
  expect_equal(res$end_date, Sys.Date() + 23)
})
