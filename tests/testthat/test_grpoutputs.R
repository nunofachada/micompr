library(micompr)
context("grpoutputs")

test_that("grpoutputs constructs the expected objects", {

  outputs <- c("PopSheep", "PopWolf", "QtyGrass",
               "EnSheep", "EnWolf", "EnGrass",
               "All")

  # Load data from extdata files
  dir_nl_ok <- system.file("extdata", "nl_ok", package = "micompr")
  dir_jex_ok <- system.file("extdata", "j_ex_ok", package = "micompr")
  dir_jex_noshuff <- system.file("extdata", "j_ex_noshuff", package = "micompr")
  dir_jex_diff <- system.file("extdata", "j_ex_diff", package = "micompr")
  files <- "stats400v1*.tsv"

  # Instantiate several grpoutputs objects
  go_ok <- grpoutputs(outputs, c(dir_nl_ok, dir_jex_ok),
                      c(files, files),
                      lvls = c("NLOK", "JEXOK"), concat = T)
  go_noshuff <- grpoutputs(outputs, c(dir_nl_ok, dir_jex_noshuff),
                           c(files, files),
                           lvls = c("NLOK", "JEXNOSHUF"), concat = T)
  go_diff <- grpoutputs(outputs, c(dir_nl_ok, dir_jex_diff),
                        c(files, files),
                        lvls = c("NLOK", "JEXDIFF"), concat = T)
  go_tri <- grpoutputs(outputs[1:6],
                       c(dir_nl_ok, dir_jex_noshuff, dir_jex_diff),
                       c(files, files, files),
                       lvls = c("NLOK", "JEXNOSHUF", "JEXDIFF"), concat = F)

  #### Start testing ####

  # Test if objects have the correct class
  expect_is(go_ok, "grpoutputs")
  expect_is(go_noshuff, "grpoutputs")
  expect_is(go_diff, "grpoutputs")
  expect_is(go_tri, "grpoutputs")

  # Test if groups are as expected
  expect_equal(go_ok$groups, c(10, 10))
  expect_equal(go_noshuff$groups, c(10, 10))
  expect_equal(go_diff$groups, c(10, 10))
  expect_equal(go_tri$groups, c(10, 10, 10))

  # Test if levels are as expected
  expect_equal(go_ok$lvls, c("NLOK", "JEXOK"))
  expect_equal(go_noshuff$lvls, c("NLOK", "JEXNOSHUF"))
  expect_equal(go_diff$lvls, c("NLOK", "JEXDIFF"))
  expect_equal(go_tri$lvls, c("NLOK", "JEXNOSHUF", "JEXDIFF"))

  # Test if concat is as expected
  expect_true(go_ok$concat)
  expect_true(go_noshuff$concat)
  expect_true(go_diff$concat)
  expect_false(go_tri$concat)

})
