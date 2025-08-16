# Copyright (c) 2016-2025 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

library(micompr)
context("micompr utilities")

# Test pvalnum
test_that("pvalnum correctly converts p-values to numeric values", {
  expect_equal(micompr:::pvalnum(c("0.06", 0.9, "<0.0001", "0.3", "100", "x")),
               c(0.06, 0.9, 0, 0.3, 100, 0))
  expect_equal(micompr:::pvalnum(0.001), 0.001)
  expect_equal(micompr:::pvalnum(""), 0)
})

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

test_that("centerscale handles NA values with na.rm", {
  v_na <- c(1, 2, NA, 3)

  # autoscale with na.rm = TRUE
  m  <- mean(v_na, na.rm = TRUE)
  s  <- sd(v_na, na.rm = TRUE)
  expect_equal(
    centerscale(v_na, "auto", na.rm = TRUE),
    (v_na - m) / s
  )

  # range with na.rm = TRUE
  r  <- diff(range(v_na, na.rm = TRUE))
  expect_equal(
    centerscale(v_na, "range", na.rm = TRUE),
    (v_na - m) / r
  )

  # iqrange with na.rm = TRUE (keeps your original mean-centering contract)
  iq <- IQR(v_na, type = 5, na.rm = TRUE)
  expect_equal(
    centerscale(v_na, "iqrange", na.rm = TRUE),
    (v_na - m) / iq
  )
})

test_that("centerscale applies zero_action = 'zeros' by default (and warns)", {
  v_const <- rep(5, 4)    # sd=0, var=0, range=0, IQR=0
  expect_warning(expect_equal(centerscale(v_const, "auto"),   rep(0, 4)))
  expect_warning(expect_equal(centerscale(v_const, "range"),  rep(0, 4)))
  expect_warning(expect_equal(centerscale(v_const, "iqrange"),rep(0, 4)))
  expect_warning(expect_equal(centerscale(v_const, "vast"),   rep(0, 4)))
  expect_warning(expect_equal(centerscale(v_const, "pareto"), rep(0, 4)))

  # level with constant nonzero mean has denom != 0; should not warn, still zeros
  expect_silent(expect_equal(centerscale(v_const, "level"),   (v_const - mean(v_const)) / mean(v_const)))
})

test_that("centerscale zero_action = 'unscaled' returns original vector (and warns)", {
  v_const <- rep(5, 3)
  expect_warning(expect_equal(
    centerscale(v_const, "range", zero_action = "unscaled"),
    v_const
  ))
  expect_warning(expect_equal(
    centerscale(v_const, "auto", zero_action = "unscaled"),
    v_const
  ))
})

test_that("centerscale zero_action = 'fill' uses zero_fill (and warns)", {
  v_const <- rep(5, 5)
  expect_warning(expect_equal(
    centerscale(v_const, "iqrange", zero_action = "fill", zero_fill = 0.5),
    rep(0.5, length(v_const))
  ))
  expect_warning(expect_equal(
    centerscale(v_const, "vast", zero_action = "fill", zero_fill = -7),
    rep(-7, length(v_const))
  ))
})

test_that("centerscale 'center' and 'none' do not warn for constant vectors", {
  v_const <- rep(5, 3)
  expect_silent(centerscale(v_const, "center"))
  expect_silent(centerscale(v_const, "none"))
})

test_that("concat_outputs produces correct results and expected errors", {

  # Set RNG seed for reproducible results
  set.seed(123)

  # Output 1, length 100
  out1 <- matrix(rnorm(2000, mean = 0, sd = 1), nrow = 20)
  # Output 2, length 200
  out2 <- matrix(rnorm(4000, mean = 100, sd = 200), nrow = 20)
  # Output 1, length 50
  out3 <- matrix(rnorm(1000, mean = -1000, sd = 10), nrow = 20)

  # Different methods for centering and scaling
  expect_is(concat_outputs(list(out1, out2, out3), "center"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out3), "auto"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out3), "range"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out3), "iqrange"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out3), "vast"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out3), "pareto"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out3), "level"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out3), "none"), "matrix")

  # More or less outputs
  expect_is(concat_outputs(list(out1), "range"), "matrix")
  expect_is(concat_outputs(list(out1, out2), "range"), "matrix")
  expect_is(concat_outputs(list(out1, out2, out1, out2, out3, out1, out3),
                           "range"), "matrix")

  # Errors
  expect_error(concat_outputs("this is not a list", "none"),
               "'outputlist' argument is not a list")
  expect_error(concat_outputs(list(), "none"),
               "'outputlist' is an empty list")
  m1 = matrix(c(2, 2, 2, 1), nrow=2)
  m2 = matrix(c(1, 3, 4, 1, 6, 7, 0, 0, 8), nrow=3)
  expect_error(concat_outputs(list(m1, m2)),
               paste0("Number of observations (rows) is not the same for ",
                      "each output matrix"),
               fixed = TRUE)
})