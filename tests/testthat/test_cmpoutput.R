library(micompr)
context("cmpoutput")

test_that("cmpoutput constructs the expected objects", {

  # Minimum percentage of variance to be explained
  minvar <- 0.9

  # Instantiate several cmpoutput objects for testing using the datasets
  # provided with the package
  cmp_ok1 <- cmpoutput("SheepPop",
                       minvar,
                       pphpc_ok$data[["Pop.Sheep"]],
                       pphpc_ok$factors)

  cmp_noshuff2 <- cmpoutput("WolfPop",
                            minvar,
                            pphpc_noshuff$data[["Pop.Wolf"]],
                            pphpc_noshuff$factors)

  cmp_diff7 <- cmpoutput("Concat",
                         minvar,
                         pphpc_diff$data[["All"]],
                         pphpc_diff$factors)

  cmp_vlo6 <- cmpoutput("GrassEn",
                        minvar,
                        pphpc_testvlo$data[["Energy.Grass"]],
                        pphpc_testvlo$factors)

  # Instantiate a cmpoutput object with output from four pphpc implementations
  # Instantiate a grpobjects first
  go_quad <-
    grpoutputs(6,
               c(system.file("extdata", "nl_ok", package = "micompr"),
                 system.file("extdata", "j_ex_ok", package = "micompr"),
                 system.file("extdata", "j_ex_noshuff", package = "micompr"),
                 system.file("extdata", "j_ex_diff", package = "micompr")),
               rep("stats400v1*.tsv", 4))

  cmp_quad3 <- cmpoutput("GrassQty",
                         minvar,
                         go_quad$data[["out3"]],
                         go_quad$factors)

  #### Start testing ####

  ## Common tests for the five cmpoutput objects ##
  for (ccmp in list(cmp_ok1, cmp_noshuff2, cmp_diff7, cmp_vlo6, cmp_quad3)) {

    # Check if cmpoutput objects have the correct type
    expect_is(ccmp, "cmpoutput")

    # Test if the minimum percentage of variance to be explained matches what
    # was set at instantiation time
    expect_equal(ccmp$ve, minvar)

    # Check that the number of PCs which explain the specified minimum
    # percentage of variance has the expected value
    expect_equal(ccmp$npcs,
                 match(TRUE, cumsum(ccmp$varexp) > minvar))

    # Check that the tests objects are what is expected and p-values are within
    # the 0-1 range
    if (ccmp$npcs > 1) {
      expect_is(ccmp$tests$manova, "manova")
      expect_true((ccmp$p.values$manova >= 0) && (ccmp$p.values$manova <= 1),
                  "MANOVA p-value not between 0 and 1.")
    } else {
      expect_null(ccmp$tests$manova)
    }
    for (i in ccmp$npcs) {
      if (length(levels(ccmp$factors)) == 2) {
        expect_is(ccmp$tests$parametric[[i]], "htest")
      } else {
        expect_is(ccmp$tests$parametric[[i]], "aov")
      }
      expect_is(ccmp$tests$nonparametric[[i]], "htest")
      expect_true((ccmp$p.values$parametric[i] >= 0)
                  && (ccmp$p.values$parametric[i] <= 1),
                  "Parametric test p-value not between 0 and 1.")
      expect_true((ccmp$p.values$nonparametric[i] >= 0)
                  && (ccmp$p.values$nonparametric[i] <= 1),
                  "Non-parametric test p-value not between 0 and 1.")
    }
  }

  ## Different tests for the cmpoutput objects ##

  # Check the names given to the comparisons
  expect_equal(cmp_ok1$name, "SheepPop")
  expect_equal(cmp_noshuff2$name, "WolfPop")
  expect_equal(cmp_diff7$name, "Concat")
  expect_equal(cmp_vlo6$name, "GrassEn")
  expect_equal(cmp_quad3$name, "GrassQty")

  # Check that the dimensions of the scores (PCA) matrix are limited by the
  # number of factors (i.e. number of observations)
  # In these cases output length (number of variables) is always larger than
  # the number of factors (number of observations)
  expect_equal(dim(cmp_ok1$scores),
               c(length(pphpc_ok$factors), length(pphpc_ok$factors)))
  expect_equal(dim(cmp_noshuff2$scores),
               c(length(pphpc_noshuff$factors), length(pphpc_noshuff$factors)))
  expect_equal(dim(cmp_diff7$scores),
               c(length(pphpc_diff$factors), length(pphpc_diff$factors)))
  expect_equal(dim(cmp_vlo6$scores),
               c(length(pphpc_testvlo$factors), length(pphpc_testvlo$factors)))
  expect_equal(dim(cmp_quad3$scores),
               c(length(go_quad$factors), length(go_quad$factors)))

  # Check if the factors are the same as in the original data
  expect_equal(cmp_ok1$factors, pphpc_ok$factors)
  expect_equal(cmp_noshuff2$factors, pphpc_noshuff$factors)
  expect_equal(cmp_diff7$factors, pphpc_diff$factors)
  expect_equal(cmp_vlo6$factors, pphpc_testvlo$factors)
  expect_equal(cmp_quad3$factors, go_quad$factors)

})


test_that("cmpoutput throws errors when improperly invoked", {

  # Test for incorrect 've' parameter
  expect_error(
    cmpoutput("A", 1.1, pphpc_ok$data[[1]], pphpc_ok$factors),
    "'ve' parameter must be between 0 and 1.",
    fixed = TRUE
  )
  expect_error(
    cmpoutput("B", -0.01, pphpc_ok$data[[2]], pphpc_ok$factors),
    "'ve' parameter must be between 0 and 1.",
    fixed = TRUE
  )

  # Test for invalid number of levels
  expect_error(
    cmpoutput("C", 0.5, pphpc_ok$data[[3]], rep(1, length(pphpc_ok$factors))),
    "At least two factors are required to perform model comparison.",
    fixed = TRUE
  )

  # Test for error due to different number of observations in data and factors
  expect_error(
    cmpoutput("D", 0.3, pphpc_ok$data[[4]], rep(pphpc_ok$factors, 2)),
    "Number of observations in 'data' and 'factors' does not match.",
    fixed = TRUE
  )

})

test_that("assumptions.cmpoutput creates the correct object", {

  #### No warnings #####

  # Create a cmpoutput object from the provided datasets
  cmp <- cmpoutput("All", 0.8, pphpc_ok$data[["All"]], pphpc_ok$factors)

  # Get the assumptions for the parametric tests performed in cmp
  acmp <- assumptions(cmp)

  # Check that the objects are of the correct type
  expect_is(acmp, "assumptions_cmpoutput")
  expect_is(acmp$manova, "assumptions_manova")
  expect_is(acmp$ttest, "assumptions_paruv")

  #### Warnings about more variables than observations ####

  # Create a cmpoutput object from the provided datasets, set ve to 0.9 such
  # that more PCs are required than before
  cmp <- cmpoutput("All", 0.9, pphpc_ok$data[["All"]], pphpc_ok$factors)

  # Check that the creation of the assumptions object produces the expected
  # warnings, i.e. that there are more variables (represented by the PCs) than
  # observations
  expect_warning(assumptions(cmp),
                 paste("Royston test requires more observations than",
                       "(dependent) variables (DVs). Reducing number of",
                       "variables from 10 to 9 in group"),
                 fixed = TRUE)

  # Get the assumptions for the parametric tests performed in cmp, disable
  # warnings this time
  oldw <- getOption("warn")
  options(warn = -1)

  acmp <- assumptions(cmp)

  options(warn = oldw)

  # Check that the objects are of the correct type
  expect_is(acmp, "assumptions_cmpoutput")
  expect_is(acmp$manova, "assumptions_manova")
  expect_is(acmp$ttest, "assumptions_paruv")

  #### Test with insufficient observations for the Royston test ####

  # In this case it should not be possible to perform the Royston test
  cmp <- cmpoutput("GrassEn",
                   0.9,
                   pphpc_testvlo$data[["Energy.Grass"]],
                   pphpc_testvlo$factors)

  # Check that if it is so
  expect_warning(assumptions(cmp),
                 "Royston test requires at least 4 observations",
                 fixed = TRUE)

  # Do create the object and check remaining stuff
  oldw <- getOption("warn")
  options(warn = -1)

  acmp <- assumptions(cmp)

  options(warn = oldw)

  # Check that the objects are of the correct type
  expect_is(acmp, "assumptions_cmpoutput")
  expect_is(acmp$manova, "assumptions_manova")
  expect_is(acmp$ttest, "assumptions_paruv")


})