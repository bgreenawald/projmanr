context("Critican Path on taskdata3")

library(projmanr)
library(igraph)
library(tidyr)
library(R6)
library(ggplot2)

test_that("Correct critical path", {
  res <- critical_path(projmanr::taskdata3)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                    "9", "10", "11", "13"))
  expect_equal(length(res), 5)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata3))
})

test_that("Correct critical path is computed with gantt", {
  res <- critical_path(projmanr::taskdata3, gantt = T)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                   "9", "10", "11", "13"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata3))
})

test_that("Correct critical path is computed with network diagram", {
  res <- critical_path(projmanr::taskdata3, network = T)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                    "9", "10", "11", "13"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata3))
})

test_that("Correct critical path is computed with both graph", {
  res <- critical_path(projmanr::taskdata3, gantt = T, network = T)
  expect_equal(length(res$critical_path), 8)
  expect_equal(res$critical_path, c("2", "3", "6", "7", 
                                    "9", "10", "11", "13"))
  expect_equal(length(res), 7)
  expect_equal(res$total_duration, 40)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata3))
})

test_that("Date output is working correctly", {
  res <- critical_path(projmanr::taskdata3, gantt = T, network = T,
                       start_date = "2017-10-10")
  expect_equal(res$end_date, as.Date("2017-11-19"))
  
  res <- critical_path(projmanr::taskdata3, gantt = T, network = T)
  expect_equal(res$end_date, Sys.Date() + 40)
})
