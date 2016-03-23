library(micompr)
context("micompr utilities")

# Test pvalcol
test_that("pvalcol produces expected results", {
  tc <- c("darkgreen", "yellow", "red")
  expect_equal(micompr:::pvalcol(c(0.06, 0.9, 0.0001, 0.3, 0.2, 0.02), tc),
               c(tc[1], tc[1], tc[3], tc[1], tc[1], tc[2]))
  expect_equal(micompr:::pvalcol(c(1, 0, 0.01, 0.05), tc),
               c(tc[1], tc[3], tc[2], tc[1]), tc)
  expect_equal(micompr:::pvalcol(1, tc), tc[1])
  expect_equal(micompr:::pvalcol(c(1, 0.5, 0, 0.9, 0.4, 0.001),
                                c("green","black","red","cyan"),
                                c(1, 0.5, 0.1)),
               c("green", "black", "cyan", "black", "red","cyan"))
})

# Test plotcols
test_that("plotcols produces expected results", {
  expect_equal(micompr:::plotcols(),
               c("blue", "red", "green", "gold", "violet", "cyan"))
})

# Test centerscale
test_that("centerscale produces expected results", {
  v <- c(-10, -3, 400, 10, 10, 15, -800, -33909.34, 3.21, 0, -2, 100, -100.5,
         19, 0.0005, 14.1, 3e10, -2e-10, -1.4e12, 20, 400, 4.553e-19, 20)
  expect_equal(centerscale(v, "center"),
               v - mean(v))
  expect_equal(centerscale(v, "auto"),
               (v - mean(v)) / sd(v))
  expect_equal(centerscale(v, "range"),
               (v - mean(v)) / (max(v) - min(v)))
  expect_equal(centerscale(v, "iqrange"),
               (v - mean(v)) / IQR(v, type = 5))
  expect_equal(centerscale(v, "vast"),
               (v - mean(v)) * mean(v) / var(v))
  expect_equal(centerscale(v, "pareto"),
               (v - mean(v)) / sqrt(sd(v)))
  expect_equal(centerscale(v, "level"),
               (v - mean(v)) / mean(v))
  expect_equal(centerscale(v, "none"),
               v)

})
