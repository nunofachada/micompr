library(micompr)
context("assumptions")

test_that("assumptions_manova constructs the expected objects", {


  # Test depends on whether the MVN and biotools packages are present
  if (!requireNamespace("MVN", quietly = TRUE) ||
      !requireNamespace("biotools", quietly = TRUE) ) {

    # MVN and biotools packages are NOT present

    expect_message(assumptions_manova(iris[, 1:4], iris[, 5]),
                   "MANOVA assumptions require 'MVN' and 'biotools' packages.")

    expect_null(assumptions_manova(iris[, 1:4], iris[, 5]))

  } else {

    # MVN and biotools packages are present

    amnv <- assumptions_manova(iris[, 1:4], iris[, 5])

    expect_is(amnv, "assumptions_manova")
    for (rt in amnv$mvntest) {
      expect_is(rt, "royston")
    }
    expect_is(amnv$vartest, "boxM")

  }

})

test_that("assumptions_paruv constructs the expected objects", {

  auv <- assumptions_paruv(iris[, 1:4], iris[, 5])

  expect_is(auv, "assumptions_paruv")

  # Test that the Shapiro-Wilk test is present for all tested dependent
  # variables and groups
  for (dv in auv$uvntest) {

    for (swt in dv) {

      expect_is(swt, "htest")
      expect_output(print(swt),
                    "Shapiro-Wilk normality test",
                    fixed = TRUE)
    }
  }

  # Test that the Bartlett test is present for all tested dependent variables
  for (bt in auv$vartest) {

    expect_is(bt, "htest")
    expect_output(print(bt),
                  "Bartlett test of homogeneity of variances",
                  fixed = TRUE)
  }

})