#' Concatenate strings without any separator characters
#'
#' Concatenate strings without any separator characters.
#'
#' This function simply calls \code{\link{paste0}} with the \code{collapse}
#' option set to \code{""}.
#'
#' @param ... one or more \R objects, to be converted to character vectors.
#'
#' @return A character vector of the concatenated values without any separator
#' characters.
#'
#' @keywords internal
#'
#' @examples
#' micompr:::pst("a", "b", "c", c("a", "b", "c"))
#' # [1] "abcaabcbabcc"
#'
pst <- function(...) {
  paste0(..., collapse = "")
}

#' Format p-values
#'
#' Generic function to format \emph{p}-values.
#'
#' @param pval Numeric \emph{p}-value to format (between 0 and 1).
#' @param params A list of method-dependent options.
#'
#' @return A string representing the formatted \emph{p}-value.
#'
#' @export
#'
#' @seealso \code{\link{pvalf.default}}
#'
pvalf <- function(pval, params) UseMethod("pvalf")

#' Default p-value formatting method
#'
#' Format a \emph{p}-value for printing in a \code{LaTeX} table. Requires the
#' \emph{ulem} \code{LaTeX} package for underlining the \emph{p}-values.
#'
#' @param pval Numeric value between 0 and 1.
#' @param params A list of options. This function accepts the following options:
#' \describe{
#'   \item{minval}{If \emph{p}-value is below this value, return this value
#'         preceded by a "<" sign instead instead.}
#'   \item{lim1val}{If \emph{p}-value is below this value, it will be
#'         double-underlined.}
#'   \item{lim2val}{If \emph{p}-value is below this value, it will be
#'         underlined.}
#'   \item{na_str}{String to use for NAs. By default NAs are returned as is.}
#' }
#'
#' @return A string representing the formatted \code{pval}.
#'
#' @export
#'
#' @examples
#' pvalf(0.1)
#' # [1] "0.100"
#'
#' pvalf(0.000001)
#' # [1] "\\uuline{1e-06}"
#'
#' pvalf(c(0.06, 0.04, 0.005, 0.00001), list(minval = 0.0001))
#' # [1] "0.060"                 "\\uline{0.040}"        "\\uuline{0.005}"
#' # [4] "\\uuline{$\\lt$1e-04}"
#'
pvalf.default <- function(pval, params = list()) {

  if (exists("minval", where = params)) {
    minval <- params$minval
  } else {
    minval <- 0
  }

  if (exists("lim1val", where = params)) {
    lim1val <- params$lim1val
  } else {
    lim1val <- 0.01
  }

  if (exists("lim2val", where = params)) {
    lim2val <- params$lim2val
  } else {
    lim2val <- 0.05
  }

  if (lim1val >= lim2val) {
    stop("lim1val must be lower than lim2val.")
  }

  fval <- ifelse(pval > 0.0005,
                 formatC(pval, format = "f", digits = 3),
                 formatC(pval, format = "e", digits = 0))

  fval <- ifelse(pval < minval,
                 paste("<",
                       ifelse(minval > 0.0005,
                              formatC(minval, format = "f", digits = 3),
                              formatC(minval, format = "e", digits = 0)),
                       sep = ""),
                 fval)

  fval <- ifelse(pval < lim1val,
                 paste("\\uuline{", fval, "}", sep = ""),
                 ifelse(pval < lim2val,
                        paste("\\uline{", fval, "}", sep = ""),
                        fval))

  # Replace NAs with a specific string?
  if (exists("na_str", where = params)) {
    fval <- replace(fval, is.na(fval), params$na_str)
  }

  fval

}

#' Simple \code{TikZ} scatter plot
#'
#' Create a simple 2D \code{TikZ} scatter plot, useful for plotting PCA data.
#'
#' This function creates a simple \code{TikZ} 2D scatter plot within a
#' \code{tikzpicture} environment. The points are plotted on a normalized
#' figure with \emph{x} and \emph{y} axes bounded between [-1, 1]. To render
#' adequately, the final \code{LaTeX} document should load the \code{plotmarks}
#' \code{TikZ} library.
#'
#' @param data Data to plot, \emph{m} x 2 numeric matrix, where \emph{m} is the
#' number of observations or points to plot.
#' @param factors Factors determining to which group observations in \code{data}
#' belong to.
#' @param marks Character vector determining how to draw the points in
#' \code{TikZ}, for example: \code{
#' c("mark=square*,mark options={color=red},mark size=0.8pt",
#'   "mark=*,mark size=0.6pt",
#'   "mark=o,mark size=0.7pt")}.
#' @param tscale The \code{scale} property of the \code{TikZ} figure.
#' @param axes_color Axes color (must be a \code{LaTeX}/\code{TikZ} color).
#'
#' @return A string containing the \code{TikZ} figure code for plotting the
#' specified \code{data}.
#'
#' @export
#'
#' @examples
#' tikzscat(rbind(c(1.5, 2), c(0.5, 1)), factor(c(1,2)),
#'          c("mark=square*,mark options={color=red},mark size=0.8pt",
#'            "mark=*,mark size=0.6pt"),
#'          6)
#' # [1] "\\begin{tikzpicture}[scale=6] \\path (-1.2,-1.2) (1.2,1.2);
#' # \\draw[very thin,color=gray] (0,1.1)--(0,-1.1);
#' # \\draw[very thin,color=gray] (1.1,0)--(-1.1,0);
#' # \\path plot[mark=square*,mark options={color=red},mark size=0.8pt]
#' # coordinates { (0.750,1.000)};
#' # \\path plot[mark=*,mark size=0.6pt] coordinates { (0.250,0.500)};
#' # \\end{tikzpicture}"
#'
tikzscat <- function(data, factors, marks, tscale, axes_color = "gray") {

  # Only two first dimensions
  data <- data[, 1:2]

  # Normalize
  data <- data / max(abs(data))

  # Unique groups
  ugrps <- unique(factors)

  # Begin tikzfigure
  figstr <- paste("\\begin{tikzpicture}[scale=", tscale, "] ",
                  "\\path (-1.2,-1.2) (1.2,1.2);",
                  "\\draw[very thin,color=", axes_color, "] ",
                  "(0,1.1)--(0,-1.1); ",
                  "\\draw[very thin,color=", axes_color, "] ",
                  "(1.1,0)--(-1.1,0);",
                  sep = "");

  # Cycle
  for (i in 1:length(ugrps)) {

    # Get points in group (make sure result is a matrix if only one point
    # available)
    pts_in_grp <- matrix(data = data[factors == ugrps[i], ],
                         nrow = sum(factors == ugrps[i]))

    # Begin plotting points in current group
    figstr <- sprintf("%s \\path plot[%s] coordinates {", figstr, marks[i])

    # Cycle points in group
    for (j in 1:dim(pts_in_grp)[1]) {
      figstr <- sprintf("%s (%5.3f,%5.3f)", figstr, pts_in_grp[j, 1],
                        pts_in_grp[j, 2]);
    }

    # End current group
    figstr <- sprintf("%s}; ", figstr);
  }


  # Close tikzpicture environment
  figstr <- sprintf("%s \\end{tikzpicture}", figstr);

  # Return figure
  figstr

}

#' Multiple \code{TikZ} 2D scatter plots for a list of output comparisons.
#'
#' Produce multiple \code{TikZ} 2D scatter plots for a list of
#' \code{\link{cmpoutput}} objects.
#'
#' This function is mainly to be used by the \code{\link{toLatex.micomp}}
#' method.
#'
#' @param cmps List of \code{\link{cmpoutput}} objects.
#' @param marks Character vector determining how to draw the points in
#' \code{TikZ}, for example: \code{
#' c("mark=square*,mark options={color=red},mark size=0.8pt",
#'   "mark=*,mark size=0.6pt",
#'   "mark=o,mark size=0.7pt")}.
#' @param tscale The \code{scale} property of the \code{TikZ} figure.
#' @param before \code{LaTeX} code to paste before each \code{TikZ} figure.
#' @param after \code{LaTeX} code to paste after each \code{TikZ} figure.
#'
#' @return List of TikZ 2D scatter plots corresponding to the comparisons
#' provided in \code{cmps}.
#'
#' @export
#' @keywords internal
#'
tscat_apply <- function(cmps, marks, tscale, before = "", after = "") {

  # Get data and factors to produce figures
  scores <- lapply(cmps, function(x) x$scores)
  factors <- lapply(cmps, function(x) x$factors)

  # Produce figures
  plts <- mapply(tikzscat, scores, factors, MoreArgs = list(marks, tscale))

  # Wrap figures with before and after latex code
  figs <- paste(before, plts, after)

  # Return TikZ figures
  figs

}

#' Convert \code{micomp} object to \code{LaTeX} table.
#'
#' This method converts \code{\link{micomp}} objects to character vectors
#' representing \code{LaTeX} tables.
#'
#' This method is inspired by the functionality provided by the \code{xtable}
#' and \code{print.xtable} (both from the
#' \href{https://cran.r-project.org/web/packages/xtable/index.html}{xtable})
#' package), but follows the standard behavior of the \code{\link{toLatex}}
#' generic.
#'
#' @param object A \code{\link{micomp}} object.
#' @param ... Currently ignored.
#' @param data_show Vector of strings specifying what data to show. Available
#' options are:
#' \describe{
#'   \item{npcs-i}{Number of principal components required to explain i-th
#'         user-specified percentage of variance.}
#'   \item{mnvp-i}{MANOVA \emph{p}-values for the i-th user-specified percentage
#'         of variance to explain.}
#'   \item{parp-j}{Parametric test \emph{p}-values for the j-th principal
#'         component.}
#'   \item{nparp-j}{Non-parametric test \emph{p}-values for the j-th principal
#'         component.}
#'   \item{aparp-j}{Parametric test \emph{p}-values adjusted with weighted
#'         Bonferroni procedure for the j-th principal component.}
#'   \item{anparp-j}{Non-parametric test \emph{p}-values adjusted with weighted
#'         Bonferroni procedure for the j-th principal component.}
#'   \item{varexp-j}{Explained variance for the j-th principal component.}
#'   \item{scoreplot}{Output projection on the first two principal components.}
#'   \item{sep}{Place a separator (e.g. midrule) between data.}
#' }
#' @param data_labels Vector of strings specifying the labels of the data to
#' show. It must be NULL or have the same length as the \code{data_show}
#' parameter.
#' @param data_labels_col Show the column containing the data labels?
#' @param table_placement \code{LaTeX} table placement.
#' @param latex_envs Wrap table in the specified \code{LaTeX}
#' environments.
#' @param booktabs Use \code{booktabs} table beautifier package?
#' @param booktabs_cmalign How to align \code{cmidule} when using the
#' \code{booktabs} package.
#' @param caption Table caption.
#' @param caption_cmd Command used for table caption.
#' @param label Table label for cross-referencing.
#' @param col_width Resize table to page column width?
#' @param pvalf_f \emph{P}-value formatter function, which receives a numeric
#' value between 0 and 1 and returns a string containing the formatted value.
#' Default is \code{\link{pvalf.default}} (requires \code{ulem} \code{LaTeX}
#' package).
#' @param pvalf_params Parameters for \code{pvalf_f} function. Default is empty
#' list.
#' @param scoreplot_marks Vector of strings specifying how \code{TikZ} should
#' draw points belonging to each group in the score plot.
#' @param scoreplot_scale \code{TikZ} scale for each score plot figure.
#' @param scoreplot_before \code{LaTeX} code to paste before each \code{TikZ}
#' score plot figure.
#' @param scoreplot_after \code{LaTeX} code to paste after each \code{TikZ}
#' score plot figure.
#'
#' @return A character vector where each element holds one line of the
#' corresponding \code{LaTeX} table.
#'
#' @export
#'
#' @importFrom utils toLatex
#'
#' @examples
#'
#' # Create a micomp object, use provided dataset, three first outputs, plus
#' # a fourth concatenated output
#' mic <- micomp(4, 0.8,
#'               list(list(name = "NLOKvsJEXOK", grpout = pphpc_ok),
#'                    list(name = "NLOKvsJEXNOSHUFF", grpout = pphpc_noshuff),
#'                    list(name = "NLOKvsJEXDIFF", grpout = pphpc_diff)),
#'               concat = TRUE)
#'
#' # Print latex table source to screen
#' toLatex(mic)
#'
toLatex.micomp <- function(
  object,
  ...,
  data_show = c("npcs-1", "mnvp-1", "parp-1", "nparp-1", "scoreplot"),
  data_labels = NULL,
  data_labels_col = T,
  table_placement = "ht",
  latex_envs = c("center"),
  booktabs = F,
  booktabs_cmalign = "l",
  caption = NULL,
  caption_cmd = "\\caption",
  label = NULL,
  col_width = F,
  pvalf_f = pvalf.default,
  pvalf_params = list(),
  scoreplot_marks = c(
    "mark=square*,mark options={color=red},mark size=0.8pt",
    "mark=*,mark size=0.6pt",
    "mark=o,mark size=0.7pt"),
  scoreplot_scale = 6,
  scoreplot_before = "\\raisebox{-.5\\height}{\\resizebox {1.2cm} {1.2cm} {",
  scoreplot_after = "}}") {

  # ######################## #
  # Obtain basic useful data #
  # ######################## #

  # How many rows each comparison will have, including separators
  ndata <- length(data_show)

  # Output names
  out_names <- rownames(object)

  # Number of outputs
  nout <- length(out_names)

  # Comparison names
  cmp_names <- colnames(object)

  # Number of comparisons
  ncmp <- length(cmp_names)

  # Get vector of variances to explain
  ve <- attr(object, "ve")

  # ########################### #
  # Determine final data labels #
  # ########################### #

  # Allocate empty final data labels vector
  dlabels_final <- vector(mode = "character", length = ndata)

  # Cycle through data to show
  for (i in 1:length(data_show)) {

    # Try to use user-specified label, if a valid one was given

    # Is data_labels null?
    if (!is.null(data_labels)) {

      # Is there enough elements in data_labels for current data value?
      if (length(data_labels) >= i) {

        # Is current label a character and not NA?
        if (!is.na(data_labels[i]) && is.character(data_labels[i])) {

          # Use user-specified label
          dlabels_final[i] <- data_labels[i];
        }
      }
    }

    # If we were unable to use a user-specified label, use default label
    if (dlabels_final[i] == "") {

      # Data to show in current row
      cdata <- data_show[i]

      # Split field name
      cdata_split <- unlist(strsplit(cdata, "-"))

      # Get first part of field name
      cdata_cmd <- cdata_split[1]

      # Does a second part of the field name exist? If so, it represents the
      # index of the variance to explain for the "npcs" and "mnvp" commands and
      # the principal component for the "parp", "nparp", "aparp", "anparp" and
      # "varexp" commands. If not specified, 1 is assumed in both cases.
      cdata_arg <-
        if (length(cdata_split) > 1) as.numeric(cdata_split[2])
      else 1

      # Determine default label for data to show in current row
      dlabels_final[i] <-
        switch(cdata_cmd,

               # Number of principal components
               npcs = pst("$\\#$PCs (", 100 * ve[cdata_arg], "\\% var.)"),

               # MANOVA p-values
               mnvp = pst("MNV (", 100 * ve[cdata_arg], "\\% var.)"),

               # Parametric p-values (raw)
               parp = pst("Par. test (PC", cdata_arg, ") "),

               # Non-parametric p-values (raw)
               nparp = pst("Non-par. test (PC", cdata_arg, ") "),

               # Parametric p-values (adjusted)
               aparp = pst("Par. test* (PC", cdata_arg, ") "),

               # Non-parametric p-values (adjusted)
               anparp = pst("Non-par. test* (PC", cdata_arg, ") "),

               # Percentage of explained variance
               varexp = pst("\\% var. (PC", cdata_arg, ") "),

               # Score plot
               scoreplot = "PCS ",

               # Separator
               sep = "")
    }
  }

  # ################################## #
  # Populate array with data for table #
  # ################################## #

  # Create table data array
  tabdata <- array(dim = c(ndata, nout, dim(object)[2]),
                   dimnames = list(
                     dlabels_final, out_names, cmp_names))

  # Cycle through comparisons
  for (cidx in 1:ncmp) {

    # Name of current comparison
    cmp <- cmp_names[cidx]

    # Get list of compared outputs for current comparison
    micmp <- object[, cmp]

    # Rows with comparison data
    for (didx in 1:length(data_show)) {

      # Data to show in current row
      cdata <- data_show[didx]

      # Split field name
      cdata_split <- unlist(strsplit(cdata, "-"))

      # Get first part of field name
      cdata_cmd <- cdata_split[1]

      # Does a second part of the field name exist? If so, it represents the
      # index of the variance to explain for the "npcs" and "mnvp" commands and
      # the principal component for the "parp", "nparp", "aparp", "anparp" and
      # "varexp" commands. If not specified, 1 is assumed in both cases.
      cdata_arg <-
        if (length(cdata_split) > 1) as.numeric(cdata_split[2])
      else 1

      # Obtain data line
      tabdata[didx, , cidx] <-
        switch(cdata_cmd,

               # Number of principal components
               npcs = sapply(micmp, function(mc) mc$npcs[cdata_arg]),

               # MANOVA p-values
               mnvp = pvalf_f(
                 sapply(micmp, function(mc) mc$p.values$manova[cdata_arg]),
                 pvalf_params),

               # Parametric p-values (raw)
               parp = pvalf_f(
                 sapply(micmp, function(mc) mc$p.values$parametric[cdata_arg]),
                 pvalf_params),

               # Non-parametric p-values (raw)
               nparp = pvalf_f(
                 sapply(micmp,
                        function(mc) mc$p.values$nonparametric[cdata_arg]),
                 pvalf_params),

               # Parametric p-values (adjusted)
               aparp = pvalf_f(
                 sapply(
                   micmp,
                   function(mc) mc$p.values$parametric_adjusted[cdata_arg]),
                 pvalf_params),

               # Non-parametric p-values (adjusted)
               anparp = pvalf_f(
                 sapply(
                   micmp,
                   function(mc) mc$p.values$nonparametric_adjusted[cdata_arg]),
                 pvalf_params),

               # Percentage of explained variance
               varexp = sapply(
                 micmp,
                 function(x)
                   if (x$varexp[cdata_arg] < 0.0001) {
                     "<0.1"
                   } else {
                     sprintf("%6.1f", x$varexp[cdata_arg] * 100)
                   }),

               # Score plot
               scoreplot = tscat_apply(object[, cmp],
                                       scoreplot_marks,
                                       scoreplot_scale,
                                       scoreplot_before,
                                       scoreplot_after),

               # Separator
               sep = "")

    }

  }

  # ########### #
  # Build table #
  # ########### #

  # Determine type of lines/rules to use in table
  hlines <- if (booktabs) {
    list(top = "\\toprule", mid = "\\midrule", bot = "\\bottomrule",
         c = pst("\\cmidrule(", booktabs_cmalign, ")"))
  } else {
    list(top = "\\hline", mid = "\\hline", bot = "\\hline", c = "\\cline")
  }

  # Initialize table variable
  ltxtab <- list()
  idx <- 1

  # Set table float environment
  ltxtab[[idx]] <- pst("\\begin{table}[", table_placement, "]")
  idx <- idx + 1

  # Set specified LaTeX environments
  ltxtab[[idx]] <- pst("\\begin{", latex_envs ,"}")
  idx <- idx + 1

  # Set a resize box?
  if (col_width) {
    ltxtab[[idx]] <- "\\resizebox{\\columnwidth}{!}{%"
    idx <- idx + 1
  }

  # Do we have a column with the data labels?
  if (data_labels_col) {
    dlpos <- "l"
    dlheader <- " & \\multirow{2}{*}{Test}"
    dlsep <- " &"
  } else {
    dlpos <- ""
    dlheader <- ""
    dlsep <- ""
  }

  # Set tabular environment
  ltxtab[[idx]] <- pst("\\begin{tabular}{c", dlpos, pst(rep("r", nout)), "}")
  idx <- idx + 1

  # Add top line/rule
  ltxtab[[idx]] <- hlines$top
  idx <- idx + 1

  # Add header
  ltxtab[[idx]] <- pst("\\multirow{2}{*}{Comp.}", dlheader, " & ",
                       "\\multicolumn{", nout, "}{c}{Outputs} \\\\")
  idx <- idx + 1

  # Add intermediate line/rule
  ltxtab[[idx]] <- pst(hlines$c, "{", 2 + data_labels_col, "-",
                       1 + data_labels_col  + nout, "}")
  idx <- idx + 1

  # Add output names
  ltxtab[[idx]] <- pst(dlsep, " & ", paste(rownames(object), collapse = " & ",
                                           sep = ""),
                       "\\\\")
  idx <- idx + 1

  # Cycle through comparisons
  for (cmp in cmp_names) {

    # Put a midrule
    ltxtab[[idx]] <- hlines$mid
    idx <- idx + 1

    # Get list of compared outputs for current comparison
    micmp <- object[, cmp]

    # Multi-row with comparison name
    ltxtab[[idx]] <- pst("\\multirow{", ndata,"}{*}{", cmp, "}")
    idx <- idx + 1

    # Rows with comparison data
    for (didx in 1:length(data_show)) {

      # Data to show in current row
      cdata <- data_show[didx]

      # Is there a data label?
      if (is.null(data_labels)) {
        lbl <- NULL
      } else {
        lbl <- data_labels[didx]
      }

      # Split field name
      cdata_split <- unlist(strsplit(cdata, "-"))

      # Get first part of field name
      cdata_cmd <- cdata_split[1]

      # Does a second part of the field name exist? If so, it represents the
      # index of the variance to explain for the "npcs" and "mnvp" commands and
      # the principal component for the "parp", "nparp", "aparp", "anparp" and
      # "varexp" commands. If not specified, 1 is assumed in both cases.
      cdata_arg <-
        if (length(cdata_split) > 1) as.numeric(cdata_split[2])
      else 1

      # Add row to table, determine type of row to add
      ltxtab[[idx]] <-
        switch(cdata_cmd,

               # Number of principal components
               npcs = pst(
                 dlabels(pst("$\\#$PCs (", 100 * ve[cdata_arg], "\\% var.)"),
                         lbl),
                 pst(" & ", sapply(micmp, function(mc) mc$npcs[cdata_arg])),
                 "\\\\"),

               # MANOVA p-values
               mnvp = pst(
                 dlabels(pst("MNV (", 100 * ve[cdata_arg], "\\% var.)"), lbl),
                 pst(" & ",
                     pvalf_f(sapply(micmp,
                                    function(mc) mc$p.values$manova[cdata_arg]),
                             pvalf_params)),
                 "\\\\"),

               # Parametric p-values (raw)
               parp = pst(
                 dlabels(pst(uvpartest, " (PC", cdata_arg, ") "), lbl),
                 pst(" & ",
                     pvalf_f(sapply(micmp,
                                    function(mc)
                                      mc$p.values$parametric[cdata_arg]),
                             pvalf_params)),
                 "\\\\"),

               # Non-parametric p-values (raw)
               nparp = pst(
                 dlabels(pst(uvnpartest, " (PC", cdata_arg, ") "), lbl),
                 pst(" & ",
                     pvalf_f(sapply(micmp,
                                    function(mc)
                                      mc$p.values$nonparametric[cdata_arg]),
                             pvalf_params)),
                 "\\\\"),

               # Parametric p-values (adjusted)
               aparp = pst(
                 dlabels(pst(uvpartest, "* (PC", cdata_arg, ") "), lbl),
                 pst(" & ",
                     pvalf_f(
                       sapply(micmp,
                              function(mc)
                                mc$p.values$parametric_adjusted[cdata_arg]),
                       pvalf_params)),
                 "\\\\"),

               # Non-parametric p-values (adjusted)
               anparp = pst(
                 dlabels(pst(uvnpartest, "* (PC", cdata_arg, ") "), lbl),
                 pst(" & ",
                     pvalf_f(
                       sapply(micmp,
                              function(mc)
                                mc$p.values$nonparametric_adjusted[cdata_arg]),
                       pvalf_params)),
                 "\\\\"),

               # Percentage of explained variance
               varexp = pst(
                 dlabels(pst("\\% var. (PC", cdata_arg, ") "), lbl),
                 pst(" & ",
                     sapply(micmp,
                            function(x) if (x$varexp[cdata_arg] < 0.0001) {
                              "<0.1"
                            } else {
                              sprintf("%6.1f", x$varexp[cdata_arg] * 100)
                            }),
                     "\\%"),
                 "\\\\"),

               # Score plot
               scoreplot = pst(
                 dlabels("PCS ", lbl),
                 pst(" & ", tscat_apply(object[, cmp],
                                        scoreplot_marks,
                                        scoreplot_scale,
                                        scoreplot_before,
                                        scoreplot_after)),
                 "\\\\"),

               # Separator
               sep = pst(hlines$c, "{", 1 + data_labels_col, "-",
                         1 + data_labels_col  + nout, "}"))

      # Next row
      idx <- idx + 1

    }

  }

  # Set the bottom line
  ltxtab[[idx]] <- hlines$bot
  idx <- idx + 1

  # Close the tabular environment
  ltxtab[[idx]] <- "\\end{tabular}"
  idx <- idx + 1

  # If open, close the resize box
  if (col_width) {
    ltxtab[[idx]] <- "} % resize box"
    idx <- idx + 1
  }

  # Add a caption, if specified
  if (!is.null(caption)) {
    ltxtab[[idx]] <- pst(caption_cmd, "{", caption,"}")
    idx <- idx + 1
  }

  # Add reference label, if specified
  if (!is.null(label)) {
    ltxtab[[idx]] <- pst("\\label{", label,"}")
    idx <- idx + 1
  }

  # Close specified LaTeX environments
  ltxtab[[idx]] <- pst("\\end{", rev(latex_envs) ,"}")
  idx <- idx + 1

  # Close table float environment
  ltxtab[[idx]] <- "\\end{table}"
  idx <- idx + 1

  # Setup final table object
  ltxtab <- unlist(ltxtab)
  class(ltxtab) <- "Latex"

  # Return table
  ltxtab

}
