library(micomp)
context("micomp utilities")

test_that("does pvalcol produce expected results", {
  tc <- c("darkgreen", "yellow", "red")
  expect_equal(micomp:::pvalcol(c(0.06, 0.9, 0.0001, 0.3, 0.2, 0.02)),
               c(tc[1], tc[1], tc[3], tc[1], tc[1], tc[2]))
  expect_equal(micomp:::pvalcol(1), tc[1])
})
