#' Group outputs.
#'
#' @param outputs
#' @param nvars 
#' @param folders 
#' @param files 
#' @param lvls
#'
#' @return
#' @export
#'
#' @examples
grpoutputs <- function(outputs, nvars, folders, files, lvls=NULL) {
  
  # Determine number of file sets (i.e. number of unique factors or levels)
  nfilesets <- length(files)
  
  if (!is.null(lvls)) {
    
    # Check if the number of given levels corresponds to the number
    # of file sets
    if (length(lvls) != nfilesets)
      stop("Number of file sets is not the same as the given number",
           " of factors.")
    
  } else {
  
    # Create default levels
    lvls <- 1:nfilesets
    
  }

  # Instantiate the 'factors' vector
  factors <- vector()
  
  # Adjust the number of folders in folder vector if required
  folders <- rep_len(folders, nfilesets)
  
  # Instantiate the 'groups' vector
  groups <- vector(mode="integer", length=nfilesets)
  
  # Determine number of files (i.e. observations) for each file set (i.e. factor)
  for (i in 1:nfilesets) {
    
    # Current file set (i.e. factor)
    curr_files <- dir(folders[i], pattern=glob2rx(files[i]))
    
    # How many files in set? (i.e. how many observations for current factor)
    groups[i] <- length(curr_files)
    
    # Stop if no files are found
    if (groups[i] == 0)
      stop("No files were found: ", file.path(folders[i], files[i]))
    
    # Increase factor vector
    factors <- c(factors, rep(lvls[i], groups[i]))
    
  }
  
  # Create proper factor vector
  factors <- factor(factors);
  
  # Determine total number of files for all sets (i.e. observations)
  nobs <- sum(groups)
  
  #
  if (length(outputs) == 1) {
    nout <- outputs
    outputs <- paste("out", 1:nout, sep="")
  } else {
    nout <- length(outputs)
  }
  
  # Create grouped outputs array
  data <- array(dim=c(nout, nobs, nvars))
  
  # Cycle through all file sets
  for (i in 1:nfilesets) {
    
    # Current file set
    curr_files <- dir(folders[i], pattern=glob2rx(files[i]))
    
    # Base index for current file set
    if (i == 1) {
      bidx <- 0;
    } else {
      bidx <- sum(groups[1:(i-1)])
    }

    # Cycle through files in current set
    for (j in 1:groups[i]) {
      
      # Current file
      cfile <- file.path(folders[i], curr_files[j])
    
      # Read file data
      tdata <- read.table(cfile)
      
      # Organize data
      for (k in 1:nout) {
        data[k,bidx+j,] <- t(tdata[,k])
      }
    }
    
  }
  
  # Return outputs, groups and factors
  go <- list(data=data, outputs=outputs, groups=groups, factors=factors, lvls=lvls)
  class(go) <- "grpoutputs"
  go

}

print.grpoutputs <- function(go) {
  
  cat("Number of outputs: ", dim(go$data)[1], "\n")
  cat("Outputs: ", paste(go$outputs,collapse=", "), "\n")
  cat("Dimensions: ", dim(go$data)[2:3], "\n")
  cat("Group size by factor:\n")
  for (i in 1:length(go$groups)) {
    cat("\t", go$lvls[i], ": ", go$groups[i], "\n")
  }

}

plot.grpoutputs <- function(go, col=c("blue","red","green","gold","violet","cyan"), ...) {
  
  # TODO: Mean plot, max/min plot
  
  nout = dim(go$data)[1];
  ncols = min(2, nout)
  nrows = nout %/% ncols
  m <- matrix(c(1:nout, rep(nout+1,ncols)), nrow=nrows+1, ncol=ncols, byrow=T)
  layout(mat=m, heights=c(rep(0.85/nrows, nrows), 0.15))

  # Plot each output separately
  for (i in 1:nout) {
    
    # Find the maximum and minimum of the current output
    ymax <- max(go$data[i,,])
    ymin <- min(go$data[i,,])
    xlen <- length(go$data[i,1,])
    
    # Prepare plot
    plot.default(0, xlim=c(0,xlen), ylim=c(ymin,ymax), main=go$outputs[i], type="n")
    
    # Plot lines
    for (j in 1:length(go$factors)) {
      lines(go$data[i,j,], col=col[unclass(go$factors)[j]], ...)
    }

  }
  par(mar = rep(2, 4))
  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend("top", legend=go$lvls, fill=col, horiz=T)
  
}

# TODO Other nice generics to implement: mean, max, min, median, std