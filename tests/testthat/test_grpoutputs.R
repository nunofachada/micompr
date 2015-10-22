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
  go_tri <- grpoutputs(6,
                       c(dir_nl_ok, dir_jex_noshuff, dir_jex_diff),
                       c(files, files, files))

  #### Start testing ####

  # Test if objects have the correct class
  expect_is(go_ok, "grpoutputs")
  expect_is(go_noshuff, "grpoutputs")
  expect_is(go_diff, "grpoutputs")
  expect_is(go_tri, "grpoutputs")

  # Test if outputs are as expected
  expect_equal(names(go_ok$data), outputs)
  expect_equal(names(go_noshuff$data), outputs)
  expect_equal(names(go_diff$data), outputs)
  expect_equal(names(go_tri$data),
               c("out1", "out2", "out3", "out4", "out5", "out6"))

  # Test if groups are as expected
  expect_equal(go_ok$groups, c(10, 10))
  expect_equal(go_noshuff$groups, c(10, 10))
  expect_equal(go_diff$groups, c(10, 10))
  expect_equal(go_tri$groups, c(10, 10, 10))

  # Test if levels are as expected
  expect_equal(go_ok$lvls, c("NLOK", "JEXOK"))
  expect_equal(go_noshuff$lvls, c("NLOK", "JEXNOSHUF"))
  expect_equal(go_diff$lvls, c("NLOK", "JEXDIFF"))
  expect_equal(go_tri$lvls, c(1, 2, 3))

  # Test if concat is as expected
  expect_true(go_ok$concat)
  expect_true(go_noshuff$concat)
  expect_true(go_diff$concat)
  expect_false(go_tri$concat)

})

test_that("grpoutputs throws errors when improperly invoked", {

  # OS-specific file separator
  fs <- .Platform$file.sep

  #### Start testing ####

  # Should throw error because number of levels != number of files
  expect_error(
    grpoutputs(4, c("dir1", "dir2"), c("*.tsv"), lvls = c("A", "B")),
    "Number of file sets is not the same as the given number of factor levels.",
    fixed = TRUE
  )

  # Should throw error because no files were found
  expect_error(
    grpoutputs(4, "some_fake_folder", c("fake_files*.csv", "also_fakes*.csv")),
    paste("No files were found: some_fake_folder", fs, "fake_files*.csv",
          sep = ""),
    fixed = TRUE
  )

  # Should throw error because number of specified outputs is larger than
  # outputs available in file
  expect_error(
    grpoutputs(7, system.file("extdata", "nl_ok", package = "micompr"),
               "stats400v1r1.tsv", lvls = "just_the_one", concat = F),
    paste("Specified number of outputs is larger than the number ",
          "of outputs in file '",
          system.file("extdata", "nl_ok", package = "micompr"), fs,
          "stats400v1r1.tsv'.", sep = ""),
    fixed = TRUE
  )

  # Should throw error because outputs in files have different lengths
  expect_error(
    grpoutputs(4, c(system.file("extdata", "nl_ok", package = "micompr"),
                    system.file("extdata", "testdata", package = "micompr")),
               c("stats400v1r1.tsv", "stats400v1r1n50.tsv")),
    paste("Length of outputs in file '",
          system.file("extdata", "testdata", package = "micompr"),
          fs, "stats400v1r1n50.tsv",
          "' does not match the length of outputs in file '",
          system.file("extdata", "nl_ok", package = "micompr"),
          fs, "stats400v1r1.tsv",
          "'.", sep = ""),
    fixed = TRUE
  )

  # Should expect error because at least 3 outputs are required when requesting
  # output concatenation
  expect_error(
    grpoutputs(2, c(system.file("extdata", "nl_ok", package = "micompr"),
                    system.file("extdata", "j_ex_ok", package = "micompr")),
               c("stats400v1*.tsv", "stats400v1*.tsv"), concat = T),
    paste("A minimum of 3 outputs must be specified in order to use ",
          "output concatenation.", sep = "")
  )

})
