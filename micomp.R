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
micomp <- function(nout, nvars, ve, ...) {
  
  # Put comparisons in a list
  comps <- list(...)
  
  # Determine number of comparions
  ncomp <- length(comps)
  
  # List of grouped outputs for each comparison
  grpd_outputs <- list()
  
  # Results of each comparison for each output
  comp_res = vector("list", length = nout * ncomp)
  dim(comp_res) <- c(nout, ncomp)
  
  # Group outputs for each comparison
  for (i in 1:ncomp) {
    
    grpd_outputs[[i]] <- 
      group_outputs(nout, nvars, unlist(comps[[i]][1]), unlist(comps[[i]][2]))
    
  }
  
  # Cycle through each output 
  for (i in 1:nout) {

    # Cycle through comparisons
    for (j in 1:ncomp) {
      
      comp_res[[i, j]] <- 
        compare_models(ve, grpd_outputs[[j]]$data[i,,], grpd_outputs[[j]]$factors)

    }

  }
  
  comp_res

}