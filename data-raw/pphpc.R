library(micomp)

outputs <- c("Pop.Sheep", "Pop.Wolf", "Qty.Grass", "Energy.Sheep",
             "Energy.Wolf", "Energy.Grass", "All")

dir_nl_ok <- "inst/extdata/nl_ok"
dir_jex_ok <- "inst/extdata/j_ex_ok"
dir_jex_noshuff <- "inst/extdata/j_ex_noshuff"
dir_jex_diff <- "inst/extdata/j_ex_diff"

files <- "stats400v1*.tsv"

pphpc_ok <- grpoutputs(outputs,
                       c(dir_nl_ok, dir_jex_ok),
                       c(files, files),
                       lvls = c("NLOK", "JEXOK"),
                       concat = T)

pphpc_noshuff <- grpoutputs(outputs,
                            c(dir_nl_ok, dir_jex_noshuff),
                            c(files, files),
                            lvls = c("NLOK", "JEXNOSHUF"),
                            concat = T)

pphpc_diff <- grpoutputs(outputs,
                         c(dir_nl_ok,dir_jex_diff),
                         c(files, files),
                         lvls = c("NLOK", "JEXDIFF"),
                         concat = T)

save(pphpc_ok, file = "data/pphcp_ok.rdata")
save(pphpc_noshuff, file = "data/pphcp_noshuff.rdata")
save(pphpc_diff, file = "data/pphcp_diff.rdata")

