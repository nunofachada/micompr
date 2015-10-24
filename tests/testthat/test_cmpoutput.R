library(micompr)
context("cmpoutput")

# Test pvalcol
test_that("cmpoutput constructs the expected objects", {

  varexp <- 0.9

  cmp_ok1 <- cmpoutput("SheepPop",
                       varexp,
                       pphpc_ok$data[["Pop.Sheep"]],
                       pphpc_ok$factors)

  cmp_noshuff2 <- cmpoutput("WolfPop",
                            varexp,
                            pphpc_noshuff$data[["Pop.Wolf"]],
                            pphpc_noshuff$factors)

  cmp_diff7 <- cmpoutput("All",
                         varexp,
                         pphpc_diff$data[["All"]],
                         pphpc_diff$factors)

  cmp_vlo6 <- cmpoutput("GrassEn",
                        varexp,
                        pphpc_testvlo$data[["Energy.Grass"]],
                        pphpc_testvlo$factors)

})