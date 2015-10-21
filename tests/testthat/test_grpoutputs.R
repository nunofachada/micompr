library(micompr)
context("grpoutputs")

# Test grpoutputs constructor
test_that("grpoutputs constructs the expected objects", {

  outputs <- c("PopSheep", "PopWolf", "QtyGrass",
               "EnSheep", "EnWolf", "EnGrass",
               "All")

  dir_nl_ok <- system.file("extdata", "nl_ok", package = "micompr")
  dir_jex_ok <- system.file("extdata", "j_ex_ok", package = "micompr")
  dir_jex_noshuff <- system.file("extdata", "j_ex_noshuff", package = "micompr")
  dir_jex_diff <- system.file("extdata", "j_ex_diff", package = "micompr")
  files <- "stats400v1*.tsv"

  go_ok <- grpoutputs(outputs, c(dir_nl_ok, dir_jex_ok),
                      c(files, files),
                      lvls = c("NLOK", "JEXOK"), concat = T)
  go_noshuff <- grpoutputs(outputs, c(dir_nl_ok, dir_jex_noshuff),
                           c(files, files),
                           lvls = c("NLOK", "JEXNOSHUF"), concat = T)
  go_diff <- grpoutputs(outputs, c(dir_nl_ok, dir_jex_diff),
                        c(files, files),
                        lvls = c("NLOK", "JEXDIFF"), concat = T)
  go_tri <- grpoutputs(outputs, c(dir_nl_ok, dir_jex_noshuff, dir_jex_diff),
                       c(files, files, files),
                       lvls = c("NLOK", "JEXNOSHUF", "JEXDIFF"), concat = T)

  expect_is(go_ok, "grpoutputs")
  expect_is(go_noshuff, "grpoutputs")
  expect_is(go_diff, "grpoutputs")
  expect_is(go_tri, "grpoutputs")

})