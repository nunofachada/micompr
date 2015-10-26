library(micompr)
context("micomp")

test_that("micomp constructs the expected objects", {

  # Output names
  outputs <- c("PopSheep", "PopWolf", "QtyGrass",
               "EnSheep", "EnWolf", "EnGrass",
               "All")

  # Minimum percentage of variance to be explained
  minvar <- 0.9

  # Determine location of extdata files
  dir_nl_ok <- system.file("extdata", "nl_ok", package = "micompr")
  dir_jex_ok <- system.file("extdata", "j_ex_ok", package = "micompr")
  dir_jex_noshuff <- system.file("extdata", "j_ex_noshuff", package = "micompr")
  dir_jex_diff <- system.file("extdata", "j_ex_diff", package = "micompr")
  dir_na <- system.file("extdata", "testdata", "NA", package = "micompr")
  files <- "stats400v1*.tsv"
  filesA_na <- "stats400v1*n20A.tsv"
  filesB_na <- "stats400v1*n20B.tsv"

  # 1 - Build a micomp object using data from extdata files

  # 1a - Use files containing package datasets, three comparisons
  mic1a <- micomp(outputs, minvar,
                  list(
                    list(name = "NLOKvsJEXOK",
                         folders = c(dir_nl_ok, dir_jex_ok),
                         files = c(files, files),
                         lvls = c("NLOK", "JEXOK")),
                    list(name = "NLOKvsJEXNOSHUFF",
                         folders = c(dir_nl_ok, dir_jex_noshuff),
                         files = c(files, files),
                         lvls = c("NLOK", "JEXNOSHUFF")),
                    list(name = "NLOKvsJEXDIFF",
                         folders = c(dir_nl_ok, dir_jex_diff),
                         files = c(files, files),
                         lvls = c("NLOK", "JEXDIFF"))),
                  concat = T)

  # 1b - Use files containing test dataset, one comparison, just five outputs
  # (unnamed), no concatenation, unnamed levels
  mic1b <- micomp(5, minvar,
                  list(
                    list(name = "testVLOdata",
                         folders = dir_na,
                         files = c(filesA_na, filesB_na))))

  # 2 - Use package datasets (i.e. grpoutputs objects) directly
  mic2 <- micomp(outputs, minvar,
                 list(
                   list(name = "NLOKvsJEXOK", grpout = pphpc_ok),
                   list(name = "NLOKvsJEXNOSHUFF", grpout = pphpc_noshuff),
                   list(name = "NLOKvsJEXDIFF", grpout = pphpc_diff)),
                 concat = T)

  # 3 - Use manually inserted data, unnamed outputs, no concatenation
  mic3 <- micomp(6, minvar,
                 list(
                   list(name = "NLOKvsJEXOK",
                        grpout = list(data = pphpc_ok$data,
                                      factors = pphpc_ok$factors)),
                   list(name = "NLOKvsJEXNOSHUFF",
                        grpout = list(data = pphpc_noshuff$data,
                                      factors = pphpc_noshuff$factors)),

                   list(name = "NLOKvsJEXDIFF",
                        grpout = list(data = pphpc_diff$data,
                                      factors = pphpc_diff$factors))),
                 concat = F)

  ##### Start testing #####

  # Check object dimensions
  expect_equal(dim(mic1a), c(7, 3))
  expect_equal(dim(mic1b), c(5, 1))
  expect_equal(dim(mic2), c(7, 3))
  expect_equal(dim(mic3), c(6, 3))

  # Check object row names
  expect_equal(rownames(mic1a), outputs)
  expect_equal(rownames(mic1b), c("out1", "out2", "out3", "out4", "out5"))
  expect_equal(rownames(mic2), outputs)
  expect_equal(rownames(mic3),
               c("out1", "out2", "out3", "out4", "out5", "out6"))

  # Check object column names
  expect_equal(colnames(mic1a),
               c("NLOKvsJEXOK", "NLOKvsJEXNOSHUFF", "NLOKvsJEXDIFF"))
  expect_equal(colnames(mic1b),
               "testVLOdata")
  expect_equal(colnames(mic2),
               c("NLOKvsJEXOK", "NLOKvsJEXNOSHUFF", "NLOKvsJEXDIFF"))
  expect_equal(colnames(mic3),
               c("NLOKvsJEXOK", "NLOKvsJEXNOSHUFF", "NLOKvsJEXDIFF"))

  # Check properties of sub-objects
  for (i in 1:dim(mic1a)[1]) {
    for (j in 1:dim(mic1a)[2]) {

      # Get current subobject
      sobj <- mic1a[[i, j]]

      # Is subobject a cmpoutput object?
      expect_is(sobj, "cmpoutput")

      # Do objects have the
    }
  }



})