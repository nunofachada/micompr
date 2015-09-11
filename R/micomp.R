#' micomp...
#'
#' @param nout 
#' @param nvars 
#' @param ve
#' @param comps
#'
#' @return Some stuff
#' @export
#'
#' @examples #' micomp()
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
      grpoutputs(outputs, nvars, unlist(comps[[i]][1]), 
                    unlist(comps[[i]][2]), lvls=comps[[i]]$lvls)
    
  }
  
  # Cycle through each output 
  for (i in 1:nout) {

    # Cycle through comparisons
    for (j in 1:ncomp) {
      
      comp_res[[i, j]] <- 
        cmpoutput(outputs[i], ve, 
                       grpd_outputs[[j]]$data[i,,], 
                       grpd_outputs[[j]]$factors)

    }

  }
  
  class(comp_res) <- "micomp"
  comp_res

}

#' Title
#'
#' @param mcmp 
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
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

# TODO The color vector could be some kind of global
#' Title
#'
#' @param mcmp 
#' @param col 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo()
plot.micomp <- function(mcmp, col=micomp:::plotcols(), ...) {
  
  dims <- dim(mcmp)
  nout <- dims[1]
  ncomp <- dims[2]
  nplots = nout * ncomp

  m <- matrix(1:(nplots + ncomp), nrow=ncomp, ncol=nout+1, byrow=T)
  layout(mat=m)
  
  for (i in 1:ncomp) {
    
    # Get factors from the first output of the current comparison
    facts <- mcmp[[1,i]]$factors

    # Set title and legend for current comparison
    plot(0, type = "n", axes=FALSE, xlab="", ylab="")
    text(1,1, pos=1,labels=paste("Comp. ", i))
    legend("center", legend=unique(facts), fill=col, horiz=F)
    
    for (j in 1:nout) {
        
      # Get data
      scores <- mcmp[[j,i]]$scores
      varexp <- mcmp[[j,i]]$varexp
      
      # Score plot (first two PCs)
      #par(mar = rep(2, 4))
      plot.default(scores[,1], scores[,2], col=col[as.numeric(facts)], 
                   xlab=paste("PC1 (", round(varexp[1] * 100, 2), "%)", sep = ""), 
                   ylab=paste("PC2 (", round(varexp[2] * 100, 2), "%)", sep = ""), 
                   main=mcmp[[j,i]]$name, ...)
    }
  }
  
}

#' Title
#'
#' @param obj 
#' @param ... 
#'
#' @return todo
#' @export
#'
#' @examples #' todo
assumptions.micomp <- function(obj, ...) {
  micas <- lapply(obj, function(x) x$assumptions)
  #dim(micas) <- dim(obj)
  class(micas) <- "assumptions_micomp"
}

print.assumptions_micomp <- function(micas, ...) {
  
}