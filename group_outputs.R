#' Group outputs.
#'
#' @param nout 
#' @param nvars 
#' @param folders 
#' @param files 
#'
#' @return
#' @export
#'
#' @examples
group_outputs <- function(nout, nvars, folders, files) {
  
  # Determine number of file sets
  nfilesets <- length(files)
  
  # Adjust the number of folders in folder vector if required
  folders <- rep_len(folders, nfilesets)
  
  # Instantiate the 'groups' vector
  groups <- vector(mode="integer", length=nfilesets)

  factors <- vector()
  
  # Determine number of files for in each file set (i.e. observations)
  for (i in 1:nfilesets) {
    
    # Current file set
    curr_files <- dir(folders[i], pattern=glob2rx(files[i]))
    
    # How many files in set?
    groups[i] <- length(curr_files)
    
    # Stop if no files are found
    if (groups[i] == 0)
      stop("No files were found: ", file.path(folders[i], files[i]))
    
    # Increase factor vector
    factors <- c(factors, vector(mode="integer", length=groups[i]) + i)
    
  }
  
  # Create proper factor vector
  factors <- factor(factors);
  
  # Determine total number of files for all sets (i.e. observations)
  nobs <- sum(groups)
  
  # Create grouped outputs array
  outputs <- array(dim=c(nout, nobs, nvars))
  
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
        outputs[k,bidx+j,] <- t(tdata[,k])
      }
    }
    
  }
  
  # Return outputs, groups and factors
  list(data=outputs, groups=groups, factors=factors)
  
}