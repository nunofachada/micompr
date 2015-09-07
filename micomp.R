#' micomp...
#'
#' @param nout 
#' @param nvars 
#' @param ve
#' @param comps
#'
#' @return
#' @export
#'
#' @examples
micomp <- function(outputs, nvars, ve, ...) {
  
  # Put comparisons in a list
  comps <- list(...)
  
  # Determine number of comparions
  ncomp <- length(comps)
  
  #
  if (length(outputs) == 1) {
    nout <- outputs
    outputs <- paste("out", 1:nout, sep="")
  } else {
    nout <- length(outputs)
  }

  # List of grouped outputs for each comparison
  grpd_outputs <- list()
  
  # Results of each comparison for each output
  comp_res = vector("list", length = nout * ncomp)
  dim(comp_res) <- c(nout, ncomp)
  
  # Group outputs for each comparison
  for (i in 1:ncomp) {
    
    grpd_outputs[[i]] <- 
      group_outputs(outputs, nvars, unlist(comps[[i]][1]), unlist(comps[[i]][2]))
    
  }
  
  # Cycle through each output 
  for (i in 1:nout) {

    # Cycle through comparisons
    for (j in 1:ncomp) {
      
      comp_res[[i, j]] <- 
        compare_output(outputs[i], ve, grpd_outputs[[j]]$data[i,,], grpd_outputs[[j]]$factors)

    }

  }
  
  class(comp_res) <- "micomp"
  comp_res

}

print.micomp <- function(mcmp) {
  
  dims <- dim(mcmp)
  nout <- dims[1]
  ncomp <- dims[2]
  
  # Get a vector with output names
  outputs <- lapply(mcmp[,1], function(mc) return(mc$name))
  
  # Cycle through comparisons
  for (i in 1:ncomp) {
    
    npcs <- lapply(mcmp[,i], function (mc) return (mc$npcs))
    p_mnv <- lapply(mcmp[,i], function (mc) return (mc$p.values$manova))
    p_par <- lapply(mcmp[,i], function (mc) return (mc$p.values$parametric[1]))
    p_npar <- lapply(mcmp[,i], function (mc) return (mc$p.values$nonparametric[1]))

    
    df <- data.frame(rbind(npcs,p_mnv,p_par,p_npar), stringsAsFactors = F, row.names = c("#PCs", "MNV", "Par.test", "NonParTest"))
    names(df) <- outputs
    
    cat("==== Comparison", i, "====\n")
    print(df, digits=5, print.gap=2)
    
  }
  
}

plot.micomp <- function(mcmp, ...) {
  
  
}