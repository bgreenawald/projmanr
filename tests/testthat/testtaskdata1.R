library(projmanr)
library(igraph)
library(tidyr)
library(R6)
library(ggplot2)

context("Critican Path on taskdata1")

test_that("Correct critical path", {
  res <- critical_path(taskdata1)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 5)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(taskdata1))
})

test_that("Correct critical path is computed with gantt", {
  res <- critical_path(taskdata1, gantt = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(taskdata1))
})

test_that("Correct critical path is computed with network diagram", {
  res <- critical_path(taskdata1, network = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 6)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(taskdata1))
})

test_that("Correct critical path is computed with both graph", {
  res <- critical_path(taskdata1, gantt = T, network = T)
  expect_equal(length(res$critical_path), 5)
  expect_equal(res$critical_path, c("1", "2", "4", "7", "8"))
  expect_equal(length(res), 7)
  expect_equal(res$total_duration, 19)
  expect_equal(nrow(res$results), nrow(taskdata1))
})
