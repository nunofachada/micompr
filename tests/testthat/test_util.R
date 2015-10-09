library(micomp)
context("micomp utilities")

# Test pvalcol
test_that("does pvalcol produce expected results", {
  tc <- c("darkgreen", "yellow", "red")
  expect_equal(micomp:::pvalcol(c(0.06, 0.9, 0.0001, 0.3, 0.2, 0.02)),
               c(tc[1], tc[1], tc[3], tc[1], tc[1], tc[2]))
  expect_equal(micomp:::pvalcol(c(1, 0, 0.01, 0.05)),
               c(tc[1], tc[3], tc[2], tc[1]))
  expect_equal(micomp:::pvalcol(1), tc[1])
  expect_equal(micomp:::pvalcol(c(1, 0.5, 0, 0.9, 0.4, 0.001),
                                c("green","black","red","cyan"),
                                c(1, 0.5, 0.1)),
               c("green", "black", "cyan", "black", "red","cyan"))
})

# Test plotcols
test_that("does plotcols produce expected results", {
  expect_equal(micomp:::plotcols(),
               c("blue", "red", "green", "gold", "violet", "cyan"))
})
