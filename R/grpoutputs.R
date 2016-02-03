#' Group outputs
#'
#' Group outputs from multiple observations of the models to be compared.
#'
#' @param outputs A vector with the labels of each output, or an integer with
#' the number of outputs (in which case output labels will be assigned
#' automatically). In either case, the number of outputs should account for
#' an additional concatenated output, as specified in the \code{concat}
#' parameter.
#' @param folders Vector of folder names where to read files from. These are
#' recycled if \code{length(folders) < length(files)}.
#' @param files Vector of filenames (with wildcards) to load in each folder.
#' @param lvls Vector of factor (group) names, must be the same length as
#' \code{files}, i.e. each file set will be associated with a different group.
#' If not given, default group names will be set.
#' @param concat If TRUE add an additional output which corresponds to the
#' concatenation of all outputs, properly centered and scaled.
#' @param centscal Method for centering and scaling outputs if \code{concat} is
#' TRUE. It can be one of "center", "auto", "range" (default), "iqrange",
#' "vast", "pareto" or "level". Centering and scaling is performed by the
#' \code{\link{centerscale}} function.
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
#' # Determine paths for data folders, each containing outputs for 10 runs of
#' # the PPHPC model
#' dir_nl_ok <- system.file("extdata", "nl_ok", package = "micompr")
#' dir_jex_ok <- system.file("extdata", "j_ex_ok", package = "micompr")
#' files <- "stats400v1*.tsv"
#'
#' # Create a grouped outputs object using outputs from NetLogo and Java
#' # implementations of the PPHPC model
#' go <- grpoutputs(7, c(dir_nl_ok, dir_jex_ok), c(files, files),
#'                  lvls = c("NL", "JEX"), concat = TRUE)
#'
#' # Do the same, but specify output names and don't specify levels
#' go <- grpoutputs(c("a", "b", "c", "d", "e", "f"),
#'                  c(dir_nl_ok, dir_jex_ok), c(files, files))
grpoutputs <- function(outputs, folders, files, lvls = NULL, concat = F,
                       centscal = "range", ...) {

  # Determine number of file sets (i.e. number of unique factors or levels)
  nfilesets <- length(files)

  # Check if lvls is NULL
  if (!is.null(lvls)) {

    # If lvls is not NULL, check if the number of levels corresponds to the
    # number of file sets
    if (length(lvls) != nfilesets) {
      stop("Number of file sets is not the same as the given number",
           " of factor levels.")
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

  # Did user specify output names or a number of outputs?
  if ((length(outputs) == 1) && (is.numeric(outputs))) {

    # User specified number of outputs, set default names
    nout <- trunc(outputs) - concat
    outputs <- paste("out", 1:trunc(outputs), sep = "")

  } else {

    # User specified output names, determine number of outputs
    nout <- length(outputs) - concat

  }

  # User must specify at least 3 outputs to use output concatenation, such that
  # there is output 1, output 2 and their concatenation.
  if ((nout < 2) && concat) {
    # We check for nout < 2 because nout is the number of outputs minus the
    # concatenated output.
    stop(paste("A minimum of 3 outputs must be specified in order to use ",
               "output concatenation.", sep = ""))

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

      # Check that the number of outputs specified by the user is not larger
      # than the number of outputs available
      if (nout > dim(tdata)[2]) {
        stop(paste("Specified number of outputs is larger than the number ",
                   "of outputs in file '", cfile, "'.", sep = ""))
      }

      # If the user specified less outputs than those available in the file,
      # discard extra outputs.
      if (nout < dim(tdata)[2]) {
        tdata <- tdata[, 1:nout]
      }

      # Make sure tdata is in matrix form even if it only has one output
      if (nout == 1) {
        tdata <- matrix(tdata, ncol = nout)
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
      outconcat[i, ] <- unlist(sapply(data, function(x)
        centerscale(x[i, ], type = centscal)))
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
#' @param x Object of class \code{grpoutputs}.
#' @param ... Currently ignored.
#'
#' @return The argument \code{x}, invisibly, as for all \code{\link{print}}
#' methods.
#'
#' @export
#'
#' @examples
#' # Determine paths for data folders, each containing outputs for 10 runs of
#' # the PPHPC model
#' dir_nl_ok <- system.file("extdata", "nl_ok", package = "micompr")
#' dir_jex_diff <- system.file("extdata", "j_ex_diff", package = "micompr")
#' files <- "stats400v1*.tsv"
#'
#' # Create a grpoutputs object
#' go <- grpoutputs(6, c(dir_nl_ok, dir_jex_diff), c(files, files))
#'
#' # Print information about object (could just type "go" instead)
#' print(go)
#' ## Number of outputs:  6
#' ##
#' ## Output dimensions:
#' ##        out1 out2 out3 out4 out5 out6
#' ## N.Obs    20   20   20   20   20   20
#' ## N.Vars  201  201  201  201  201  201
#' ##
#' ## Group size by factor:
#' ##   group.size
#' ## 1         10
#' ## 2         10
#'
print.grpoutputs <- function(x, ...) {

  # Use summary to get the info to be printed
  smgo <- summary(x)

  # Print info
  cat("Number of outputs: ", length(x$data), "\n")
  cat("\nOutput dimensions:\n")
  print(smgo$output.dims)
  cat("\nGroup size by factor:\n")
  print(smgo$group.sizes)

  # Return input parameter, invisibly
  invisible(x)

}

#' Summary method for grouped outputs
#'
#' Summary method for objects of class \code{grpoutputs}.
#'
#' @param object Object of class \code{grpoutputs}.
#' @param ... Currently ignored.
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
#' # Determine paths for data folders, each containing outputs for 10 runs of
#' # the PPHPC model
#' dir_nl_ok <- system.file("extdata", "nl_ok", package = "micompr")
#' dir_jex_noshuff <-
#'  system.file("extdata", "j_ex_noshuff", package = "micompr")
#' files <- "stats400v1*.tsv"
#'
#' # Create a grpoutputs object
#' go <-
#'  grpoutputs(c("o1", "o2"), c(dir_nl_ok, dir_jex_noshuff), c(files, files))
#'
#' # Obtain summary
#' summary(go)
#' ## $output.dims
#' ##         o1  o2
#' ## N.Obs   20  20
#' ## N.Vars 201 201
#' ##
#' ## $group.sizes
#' ##   group.size
#' ## 1         10
#' ## 2         10
#' ##
#'
summary.grpoutputs <- function(object, ...) {

  # Get dimensions of each output
  outptab <- sapply(object$data, function(x) dim(x))
  rownames(outptab) <- c("N.Obs", "N.Vars")

  # Get group sizes
  grpszbyfact <- data.frame(group.size = object$groups,
                            row.names = object$lvls,
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
#' colors (which can be controlled through the \code{col} parameter given in
#' ...).
#'
#' This function can be very slow for a large number of observations.
#'
#' @param x Object of class \code{grpoutputs}.
#' @param ... Extra options passed to \code{\link{plot.default}}.
#'
#' @return None.
#' @export
#'
#' @examples
#' # Determine paths for the data folder containing outputs of different
#' # lengths
#' dir_na <- system.file("extdata", "testdata", "NA", package = "micompr")
#' # Sets of files A and B have 3 files each
#' filesA <- "stats400v1*n20A.tsv"
#' filesB <- "stats400v1*n20B.tsv"
#'
#' # Instantiate grpoutputs object
#' go <-
#'  grpoutputs(7, dir_na, c(filesA, filesB), lvls = c("A", "B"), concat = TRUE)
#'
#' # Plot grpoutputs object
#' plot(go)
#'
plot.grpoutputs <- function(x, ...) {

  # Get required data
  nout <- length(x$data);
  nout_simpl <- nout - x$concat
  ncols <- min(2, nout)
  outputs <- names(x$data)

  # Was a color specified?
  params <- list(...)
  if (exists("col", where = params)) {
    col <- params$col
  } else {
    col <- plotcols()
  }

  # One output or more?
  if (nout == 1) {
    # If only one output, just draw a simple plot with legend included

    leginc <- T
    m <- matrix(1, nrow = 1)

  } else {
    # Otherwise, make several subplots

    leginc <- F

    # One plot space for each non-concatenated output
    m <- 1:nout_simpl

    # Is number of non-concatenated outputs odd?
    if (nout_simpl %% 2 != 0) {

      # If so, put a zero so there is an empty subplot
      m <- c(m, 0)

    }

    # Do we have a concatenated output?
    if (x$concat) {
      # If so, find space for it
      m <- c(m, rep(max(m) + 1, 2))
    }

    # Space for legend
    m <- c(m, rep(max(m) + 1, 2))

    # Put m in matrix form
    m <- matrix(m, ncol = ncols, byrow = T)

  }

  # Set layout and plot outputs  ===================================

  # Set layout
  layout(m)

  # Plot each output separately
  for (i in 1:nout) {

    out <- outputs[i]

    # Find the maximum and minimum of the current output
    ymax <- max(x$data[[out]])
    ymin <- min(x$data[[out]])
    xlen <- length(x$data[[out]][1,])

    # Prepare plot
    plot.default(0, xlim = c(0, xlen), ylim = c(ymin, ymax),
                 main = out, type = "n", ...)

    # Plot lines
    for (i in 1:length(x$factors)) {
      lines(x$data[[out]][i,], col = col[unclass(x$factors)[i]])
    }

    # Include legend in plot?
    if (leginc) {
      legend("top", legend = x$lvls, fill = col, horiz = T)
    }

  }

  # Plot legend in own subplot?
  if (!leginc) {
    par(mar = rep(2, 4))
    plot(0, type = "n", axes = FALSE, xlab = "", ylab = "")
    legend("top", legend = x$lvls, fill = col, horiz = T)
  }

  invisible(NULL)

}
