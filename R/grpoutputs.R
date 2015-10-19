#' Group outputs
#'
#' Group outputs from multiple observations of the models to be compared.
#'
#' @param outputs A vector with the labels of each output, or an integer with
#' the number of outputs (in which case output labels will be assigned
#' automatically).
#' @param folders Vector of folder names where to read files from. These are
#' recycled if \code{length(folders) < length(files)}.
#' @param files Vector of filenames (with wildcards) to load in each folder.
#' @param lvls Vector of factor (group) names, must be the same length as
#' \code{files}, i.e. each file set will be associated with a different group.
#' If not given, default group names will be set.
#' @param concat If TRUE add an additional output which corresponds to the
#' concatenation of all outputs, properly range scaled.
#' @param ... Options passed to \code{\link{read.table}}, which is used to read
#' the files specified in \code{files}.
#'
#' @return Object of class \code{grpoutputs} containing the following data:
#' \describe{
#'  \item{data}{List of all outputs, each one grouped into a \emph{n} x \emph{m}
#'        matrix, where \emph{n} is the total number of output observations
#'        and \emph{m} is the number of variables (i.e. output length).}
#'  \item{groups}{Vector containing number of output observations in each
#'        group.}
#'  \item{factors}{Factors (or groups) associated with each observation.}
#'  \item{lvls}{Vector of factor names in the order they occur (as given in
#'        parameter with the same name).}
#'  \item{concat}{Boolean indicating if this object was created with an
#'        additional concatenated output.}
#' }
#'
#' @export
#'
#' @examples
#' NULL
#'
grpoutputs <- function(outputs, folders, files, lvls = NULL, concat = F, ...) {

  # Determine number of file sets (i.e. number of unique factors or levels)
  nfilesets <- length(files)

  # Check if lvls is NULL
  if (!is.null(lvls)) {

    # If lvls is not NULL, check if the number of levels corresponds to the
    # number of file sets
    if (length(lvls) != nfilesets) {
      stop("Number of file sets is not the same as the given number",
           " of factors.")
    }

  } else {

    # lvls is NULL, create default levels
    lvls <- 1:nfilesets

  }

  # Instantiate the 'factors' vector
  factors <- vector()

  # Adjust the number of folders in folder vector if required
  folders <- rep_len(folders, nfilesets)

  # Instantiate the 'groups' vector
  groups <- vector(mode = "integer", length = nfilesets)

  # Determine number of files (i.e. observations) for each file set (i.e.
  # factor)
  for (i in 1:nfilesets) {

    # Current file set (i.e. factor)
    curr_files <- dir(folders[i], pattern = glob2rx(files[i]))

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

  # Set default output names if not given
  if (length(outputs) == 1) {
    nout <- outputs - concat
    outputs <- paste("out", 1:nout, sep = "")
  } else {
    nout <- length(outputs) - concat
  }

  # Create grouped outputs list
  data <- list()

  # Is this the first file to be opened?
  first <- TRUE

  # Cycle through all file sets
  for (i in 1:nfilesets) {

    # Current file set
    curr_files <- dir(folders[i], pattern = glob2rx(files[i]))

    # Base index for current file set
    bidx <- if (i == 1) {
      0
    } else {
      sum(groups[1:(i - 1)])
    }

    # Cycle through files in current set
    for (j in 1:groups[i]) {

      # Current file
      cfile <- file.path(folders[i], curr_files[j])

      # Read file data
      tdata <- read.table(cfile, ...)

      # Check that the number of outputs specified by the user is the same or
      # less than the number of outputs available
      if (nout > dim(tdata)[2]) {
        stop(paste("Specified number of outputs is larger than the number ",
                   "of outputs in file '", cfile, "'.", sep = ""))
      }

      # Is this the first file to be opened?
      if (first) {
        # Yes, it's the first file, create required data structures

        # Keep name of first file
        firstfilename <- cfile

        # Next one won't be the first
        first <- FALSE

        # Determine the length of each output
        outlen <- colSums(!is.na(tdata))

        # Create grouped outputs matrix for each output
        for (k in 1:nout) {
          out <- outputs[k]
          data[[out]] <- matrix(nrow = nobs, ncol = outlen[k])
        }

      } else {
        # Not the first file, check that individual outputs in current file
        # have the same length as the same outputs in the first file

        if (any(colSums(!is.na(tdata)) != outlen)) {
          stop(paste("Length of outputs in file '", cfile, "' does not match ",
                     "the length of outputs in file '", firstfilename, "'.",
                     sep = ""))
        }

      }

      # Organize data
      for (k in 1:nout) {
        # Current output name
        out <- outputs[k]
        # Get current output vector
        o <- tdata[, k]
        # Keep current output vector, transposed and removed of NAs
        data[[out]][bidx + j, ] <- t(o[!is.na(o)])
      }
    }

  }

  # Perform output concatenation?
  if (concat) {
    outconcat <- matrix(nrow = nobs, ncol = sum(outlen))
    for (i in 1:nobs) {
      outconcat[i, ] <- unlist(sapply(data, function(x, row)
        (x[row, ] - mean(x[row, ])) / (max(x[row, ]) - min(x[row, ])), i))
    }
    nout <- nout + 1
    data[[outputs[nout]]] <- outconcat
  }

  # Return outputs, groups and factors
  go <- list(data = data,
             groups = groups,
             factors = factors,
             lvls = lvls,
             concat = concat)
  class(go) <- "grpoutputs"
  go

}

#' Print information about grouped outputs
#'
#' Print information about objects of class \code{grpoutputs}.
#'
#' @param go Object of class \code{grpoutputs}.
#'
#' @return The argument \code{go}, invisibly, as for all \code{\link{print}}
#' methods.
#'
#' @export
#'
#' @examples
#' NULL
#'
print.grpoutputs <- function(go) {

  # Use summary to get the info to be printed
  smgo <- summary(go)

  # Print info
  cat("Number of outputs: ", length(go$data), "\n")
  cat("\nOutput dimensions:\n")
  print(smgo$output.dims)
  cat("\nGroup size by factor:\n")
  print(smgo$group.sizes)

  # Return input parameter, invisibly
  invisible(go)

}

#' Summary method for grouped outputs
#'
#' Summary method for objects of class \code{grpoutputs}.
#'
#' @param go Object of class \code{grpoutputs}.
#'
#' @return A list with the following components:
#' \describe{
#'  \item{output.dims}{Dimensions of each output, i.e. number of observations
#'        and number of variables (i.e. output length).}
#'  \item{group.sizes}{Number of output observations in each group.}
#' }
#'
#' @export
#'
#' @examples
#' NULL
#'
summary.grpoutputs <- function(go) {

  # Get dimensions of each output
  outptab <- sapply(go$data, function(x) dim(x))
  rownames(outptab) <- c("N.Obs", "N.Vars")

  # Get group sizes
  grpszbyfact <- data.frame(group.size = go$groups,
                            row.names = go$lvls,
                            stringsAsFactors = F)

  # Return list with summary information
  list(`output.dims` = outptab, `group.sizes` = grpszbyfact)

}

#' Plot grouped outputs
#'
#' Plot each grouped output.
#'
#' Each output is plotted individually, and observations are plotted on top of
#' each other. Observations from different groups are plotted with different
#' colors (which can be controlled through the \code{col} parameter).
#'
#' This function can be very slow for a large number of observations.
#'
#' @param go Object of class \code{grpoutputs}.
#' @param col Vector of colors to use on observations of different groups.
#' @param ... Extra options passed to \code{\link{plot.default}}.
#'
#' @return None.
#' @export
#'
#' @examples
#' NULL
#'
plot.grpoutputs <- function(go, col = micomp:::plotcols(), ...) {

  # TODO: Mean plot, max/min plot

  # Get required data
  nout <- length(go$data);
  nout_simpl <- nout - go$concat
  ncols <- min(2, nout)
  outputs <- names(go$data)

  # Build layout matrix =======================================

  # One plot space for each normal output
  l1 <- 1:nout_simpl
  totsp <- nout_simpl

  # Make adjustment if number of plots is not pair
  l2 <- if (nout_simpl %% 2 != 0) {
    totsp <- totsp + 1;
    totsp
  } else {
    NULL
  }

  # Get plot space for concatenated output
  l3 <- if (go$concat) {
    totsp <- totsp + ncols;
    rep(totsp - (ncols == 2), ncols)
  } else {
    NULL
  }

  # Get plot space for legend
  l4 <- rep(totsp + 1 - (ncols == 2), ncols)

  # Concatenate layout vector
  lv <- c(l1, l2, l3, l4)

  # Create layout matrix
  m <- matrix(lv, ncol = ncols, byrow = T)

  # Set layout and plot outputs  ===================================

  # Set layout
  nrows <- length(lv) / ncols
  layout(mat = m, heights = c(rep(0.85 / nrows, nrows), 0.15))

  # Plot each output separately
  for (i in 1:nout) {

    out <- outputs[i]

    # Find the maximum and minimum of the current output
    ymax <- max(go$data[[out]])
    ymin <- min(go$data[[out]])
    xlen <- length(go$data[[out]][1,])

    # Take into account non-pair number of simple outputs
    if ((go$concat) && (i == nout) && (nout_simpl %% 2 != 0)) {
      plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
    }

    # Prepare plot
    plot.default(0, xlim = c(0,xlen), ylim = c(ymin,ymax),
                 main = out, type = "n", ...)

    # Plot lines
    for (i in 1:length(go$factors)) {
      lines(go$data[[out]][i,], col = col[unclass(go$factors)[i]])
    }

  }

  # Take into account non-pair number of simple outputs
  if ((!go$concat) && (nout_simpl %% 2 != 0)) {
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
  }

  # Plot legend showing colors assigned to groups
  par(mar = rep(2, 4))
  plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend("top", legend = go$lvls, fill = col, horiz = T)

  invisible(NULL)

}
