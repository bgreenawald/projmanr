library(projmanr)
library(igraph)
library(tidyr)
library(R6)
library(ggplot2)

context("Critican Path on taskdata1")

test_that("Correct critical path", {
  res <- critical_path(projmanr::taskdata1)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 5)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata1))
})

test_that("Correct critical path is computed with gantt", {
  res <- critical_path(projmanr::taskdata1, gantt = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata1))
})

test_that("Correct critical path is computed with network diagram", {
  res <- critical_path(projmanr::taskdata1, network = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata1))
})

test_that("Correct critical path is computed with both graph", {
  res <- critical_path(projmanr::taskdata1, gantt = T, network = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 7)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(projmanr::taskdata1))
})

test_that("Date output is working correctly", {
  res <- critical_path(projmanr::taskdata1, gantt = T, network = T,
                       start_date = "2017-10-10")
  expect_equal(res$end_date, as.Date("2017-10-29"))

  res <- critical_path(projmanr::taskdata1, gantt = T, network = T)
  expect_equal(res$end_date, Sys.Date() + 19)
})
