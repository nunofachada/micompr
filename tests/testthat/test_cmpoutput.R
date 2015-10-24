library(micompr)
context("cmpoutput")

# Test pvalcol
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

  #### Start testing ####

  # Test if the minimum percentage of variance to be explained matches what was
  # set at instantiation time
  expect_equal(cmp_ok1$ve, minvar)
  expect_equal(cmp_noshuff2$ve, minvar)
  expect_equal(cmp_diff7$ve, minvar)
  expect_equal(cmp_vlo6$ve, minvar)

  # Check the names given to the comparisons
  expect_equal(cmp_ok1$name, "SheepPop")
  expect_equal(cmp_noshuff2$name, "WolfPop")
  expect_equal(cmp_diff7$name, "Concat")
  expect_equal(cmp_vlo6$name, "GrassEn")

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

  # Check if the factors are the same as in the original data
  expect_equal(cmp_ok1$factors, pphpc_ok$factors)
  expect_equal(cmp_noshuff2$factors, pphpc_noshuff$factors)
  expect_equal(cmp_diff7$factors, pphpc_diff$factors)
  expect_equal(cmp_vlo6$factors, pphpc_testvlo$factors)

  # Check that the number of PCs which explain the specified minimum percentage
  # of variance has the expected value
  expect_equal(cmp_ok1$npcs,
               match(TRUE, cumsum(cmp_ok1$varexp) > minvar))
  expect_equal(cmp_noshuff2$npcs,
               match(TRUE, cumsum(cmp_noshuff2$varexp) > minvar))
  expect_equal(cmp_diff7$npcs,
               match(TRUE, cumsum(cmp_diff7$varexp) > minvar))
  expect_equal(cmp_vlo6$npcs,
               match(TRUE, cumsum(cmp_vlo6$varexp) > minvar))

})