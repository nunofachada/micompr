#' Title
#'
#' @param data 
#' @param factors 
#'
#' @return to do
#' @export
#'
#' @examples #' todo
assumptions_manova <- function(data, factors) {
  
  # `assumpt` will be a list containing the test for multivariate normality 
  # and the test for the homogeneity of covariance matrices
  assumpt = list()
  assumpt$mvntest <- list()
  assumpt$vartest <- NULL

  # Number of variables
  nvars <- dim(data)[2]
  
  # Cycle through factors for the multivariate normality tests
  for (f in unique(factors)) {
    # Indexes of current factor level
    fidx <- factors==f
    nobs <- sum(fidx)
    assumpt$mvntest[[f]] <- MVN::roystonTest(data[fidx,1:min(nobs-1, nvars)])
  }
  
  # Perform the homogeneity of covariance matrices test (Box's M)
  maxvars <- min(table(factors)-1, nvars)
  assumpt$vartest <- biotools::boxM(data[,1:maxvars], factors)
  
  assumpt
}

#' Title
#'
#' @param data 
#' @param factors 
#'
#' @return to do
#' @export
#'
#' @examples #' todo
assumptions_paruv <- function(data, factors) {
  
  # Number of variables
  nvars <- dim(data)[2]
  if (is.null(nvars)) nvars <- 1
  
  # `assumpt` will be a list containing the tests for univariate normality 
  # assumptions and the test for the homogeneity of variances assumption.
  assumpt = list()
  assumpt$uvntest <- list()
  assumpt$vartest <- list()
  
  # Cycle through each variable
  for (d in 1:nvars) {

    # Get current data
    if (nvars == 1) {
      currdata <- data
    } else {
      currdata <- data[, d]
    }
    
    # Perform univariate normality tests for each group for current variable
    for (f in unique(factors)) {
      assumpt$uvntest[[f]] <- list()
      assumpt$uvntest[[f]][[d]] <- shapiro.test(currdata[factors==f])
    }
  
    # Perform the homogeneity of variances test for current variable
    assumpt$vartest[[d]] <- bartlett.test(currdata ~ factors)
  
  }

  assumpt
  
}