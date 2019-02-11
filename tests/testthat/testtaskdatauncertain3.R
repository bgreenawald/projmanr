context("Critican Path on taskdatauncertain3")

library(projmanr)
library(igraph)
library(tidyr)
library(R6)
library(ggplot2)

test_that("Check approximate value of duration mean", {
  res <- simulation(projmanr::taskdatauncertain3, 1000)
  expect_equal(mean(res$durations) > 38 && mean(res$durations) < 42, TRUE)
})

test_that("Check the return size of simulation", {
  res <- simulation(projmanr::taskdatauncertain3, 100)
  expect_equal(length(res), 3)
  expect_equal(length(res$durations), 100)
  expect_equal(nrow(res$critical_indexes), 13)
  
  # Run the same tests, change the itr parameter to
  # ensure that it's working
  res <- simulation(projmanr::taskdatauncertain3, 1000)
  expect_equal(length(res), 3)
  expect_equal(length(res$durations), 1000)
  expect_equal(nrow(res$critical_indexes), 13)
})

test_that("Make sure the the error check on distribution works", {
  temp <- projmanr::taskdatauncertain3
  temp[12, 5] <- "t"
  
  expect_error(simulation(temp, 100), paste("Distribution t not supported,",
                                            "please use triangle, pert,",
                                            "uniform, normal or log_normal"))
  
})



# The following are the same tests from 'testtaskdata3.R'
# ensuring that the introduction of the uncertain columns did
# not cause any issues

test_that("Correct critical path", {
  res <- critical_path(projmanr::taskdatauncertain3)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                    "9", "10", "11", "13"))
  expect_equal(length(res), 5)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdatauncertain3))
})

test_that("Correct critical path is computed with gantt", {
  res <- critical_path(projmanr::taskdatauncertain3, gantt = T)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                    "9", "10", "11", "13"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdatauncertain3))
})

test_that("Correct critical path is computed with network diagram", {
  res <- critical_path(projmanr::taskdatauncertain3, network = T)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                    "9", "10", "11", "13"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdatauncertain3))
})

test_that("Correct critical path is computed with both graph", {
  res <- critical_path(projmanr::taskdatauncertain3, gantt = T, network = T)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                    "9", "10", "11", "13"))
  expect_equal(length(res), 7)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdatauncertain3))
})

test_that("Date output is working correctly", {
  res <- critical_path(projmanr::taskdatauncertain3, gantt = T, network = T,
                       start_date = "2017-10-10")
  expect_equal(res$end_date, as.Date("2017-11-19"))
  
  res <- critical_path(projmanr::taskdatauncertain3, gantt = T, network = T)
  expect_equal(res$end_date, Sys.Date() + 40)
})
