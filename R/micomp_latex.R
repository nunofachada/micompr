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
#' @param axes.color Axes color (must be a \code{LaTeX}/\code{TikZ} color).
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
tikzscat <- function(data, factors, marks, tscale, axes.color = "gray") {

  # Only two first dimensions
  data <- data[, 1:2]

  # Normalize
  data <- data / max(abs(data))

  # Unique groups
  ugrps <- unique(factors)

  # Begin tikzfigure
  figstr <- paste("\\begin{tikzpicture}[scale=", tscale, "] ",
                  "\\path (-1.2,-1.2) (1.2,1.2);",
                  "\\draw[very thin,color=", axes.color, "] ",
                  "(0,1.1)--(0,-1.1); ",
                  "\\draw[very thin,color=", axes.color, "] ",
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
#' @param data.show Vector of strings specifying what data to show. Available
#' options are:
#' \describe{
#'   \item{npcs}{Number of principal components required to explain the
#'         percentage of variance specified when \code{mic} was created.}
#'   \item{mnvp}{MANOVA \emph{p}-values.}
#'   \item{parp-i}{Parametric test \emph{p}-values for the i-th principal
#'         component.}
#'   \item{nparp-i}{Non-parametric test \emph{p}-values for the i-th principal
#'         component.}
#'   \item{aparp-i}{Parametric test \emph{p}-values adjusted with weighted
#'         Bonferroni procedure for the i-th principal component.}
#'   \item{anparp-i}{Non-parametric test \emph{p}-values adjusted with weighted
#'         Bonferroni procedure for the i-th principal component.}
#'   \item{varexp-i}{Explained variance for the i-th principal component.}
#'   \item{scoreplot}{Output projection on the first two principal components.}
#' }
#' @param table.placement \code{LaTeX} table placement.
#' @param latex.environments Wrap table in the specified \code{LaTeX}
#' environments.
#' @param booktabs Use \code{booktabs} table beautifier package?
#' @param booktabs.cmalign How to align \code{cmidule} when using the
#' \code{booktabs} package.
#' @param caption Table caption.
#' @param label Table label for cross-referencing.
#' @param col.width Resize table to page column width?
#' @param pvalf.f \emph{P}-value formatter function, which receives a numeric
#' value between 0 and 1 and returns a string containing the formatted value.
#' Default is \code{\link{pvalf.default}} (requires \code{ulem} \code{LaTeX}
#' package).
#' @param pvalf.params Parameters for \code{pvalf.f} function. Default is empty
#' list.
#' @param scoreplot.marks Vector of strings specifying how \code{TikZ} should
#' draw points belonging to each group in the score plot.
#' @param scoreplot.scale \code{TikZ} scale for each score plot figure.
#' @param scoreplot.before \code{LaTeX} code to paste before each \code{TikZ}
#' score plot figure.
#' @param scoreplot.after \code{LaTeX} code to paste after each \code{TikZ}
#' score plot figure.
#'
#' @return A character vector where each element holds one line of the
#' corresponding \code{LaTeX} table.
#'
#' @export
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
  data.show = c("npcs", "mnvp", "parp-1", "nparp-1", "scoreplot"),
  table.placement = "ht",
  latex.environments = c("center"),
  booktabs = F,
  booktabs.cmalign = "l",
  caption = NULL,
  label = NULL,
  col.width = F,
  pvalf.f = pvalf.default,
  pvalf.params = list(),
  scoreplot.marks = c(
    "mark=square*,mark options={color=red},mark size=0.8pt",
    "mark=*,mark size=0.6pt",
    "mark=o,mark size=0.7pt"),
  scoreplot.scale = 6,
  scoreplot.before = "\\raisebox{-.5\\height}{\\resizebox {1.2cm} {1.2cm} {",
  scoreplot.after = "}}") {

  # How many rows will the table have?
  ndata <- length(data.show)

  # Determine type of lines/rules to use in table
  hlines <- if (booktabs) {
    list(top = "\\toprule", mid = "\\midrule", bot = "\\bottomrule",
         c = pst("\\cmidrule(", booktabs.cmalign, ")"))
  } else {
    list(top = "\\hline", mid = "\\hline", bot = "\\hline", c = "\\cline")
  }

  # Number of outputs
  nout <- dim(object)[1]

  # Get micomp object summary
  smic <- summary(object)

  # Initialize table variable
  ltxtab <- list()
  idx <- 1

  # Set table float environment
  ltxtab[[idx]] <- pst("\\begin{table}[", table.placement, "]")
  idx <- idx + 1

  # Set specified LaTeX environments
  ltxtab[[idx]] <- pst("\\begin{", latex.environments ,"}")
  idx <- idx + 1

  # Set a resize box?
  if (col.width) {
    ltxtab[[idx]] <- "\\resizebox{\\columnwidth}{!}{%"
    idx <- idx + 1
  }

  # Set tabular environment
  ltxtab[[idx]] <- pst("\\begin{tabular}{cl", pst(rep("r", nout)), "}")
  idx <- idx + 1

  # Add top line/rule
  ltxtab[[idx]] <- hlines$top
  idx <- idx + 1

  # Add header
  ltxtab[[idx]] <- pst("\\multirow{2}{*}{Comp.} & \\multirow{2}{*}{Test} & ",
                       "\\multicolumn{", nout, "}{c}{Outputs} \\\\")
  idx <- idx + 1

  # Add intermediate line/rule
  ltxtab[[idx]] <- pst(hlines$c, "{3-", 2 + nout, "}")
  idx <- idx + 1

  # Add output names
  ltxtab[[idx]] <- pst(" & & ", paste(rownames(object), collapse = " & ",
                                      sep = ""),
                       "\\\\")
  idx <- idx + 1

  # Cycle through comparisons
  for (cmp in colnames(object)) {

    # Determine univariate test names
    nfacts <- length(levels(object[, cmp][[1]]$factors))
    if (nfacts == 2) {
      uvpartest <- "$t$-test"
      uvnpartest <- "MW"
    } else {
      uvpartest <- "ANOVA"
      uvnpartest <- "KW"
    }

    # Put a midrule
    ltxtab[[idx]] <- hlines$mid
    idx <- idx + 1

    # Get list of compared outputs for current comparison
    micmp <- object[, cmp]

    # Multi-row with comparison name
    ltxtab[[idx]] <- pst("\\multirow{", ndata,"}{*}{", cmp, "}")
    idx <- idx + 1

    # Rows with comparison data
    for (cdata in data.show) {

      # Split field name
      cdata_split <- unlist(strsplit(cdata, "-"))

      # Get first part of field name
      cdata_cmd <- cdata_split[1]

      # Does a second part of the field name exist? If so, it represents the
      # principal component.
      cdata_pc <- if (length(cdata_split) > 1) { as.numeric(cdata_split[2]) }

      # Add row to table, determine type of row to add
      ltxtab[[idx]] <-
        switch(cdata_cmd,

               # Number of principal components
               npcs = pst(" & $\\#$PCs ",
                          pst(" & ", sapply(micmp, function(mc) mc$npcs)),
                          "\\\\"),

               # MANOVA p-values
               mnvp = pst(" & MNV ",
                          pst(" & ", pvalf.f(
                            sapply(micmp, function(mc)
                              mc$p.values$manova),
                            pvalf.params)),
                          "\\\\"),

               # Parametric p-values (raw)
               parp = pst(" & ", uvpartest, " (PC", cdata_pc, ") ",
                          pst(" & ", pvalf.f(
                            sapply(micmp, function(mc)
                              mc$p.values$parametric[cdata_pc]),
                            pvalf.params)),
                          "\\\\"),

               # Non-parametric p-values (raw)
               nparp = pst(" & ", uvnpartest, " (PC", cdata_pc, ") ",
                           pst(" & ", pvalf.f(
                             sapply(micmp, function(mc)
                               mc$p.values$nonparametric[cdata_pc]),
                             pvalf.params)),
                           "\\\\"),

               # Parametric p-values (adjusted)
               aparp = pst(" & ", uvpartest, "* (PC", cdata_pc, ") ",
                           pst(" & ", pvalf.f(
                             sapply(micmp, function(mc)
                               mc$p.values$parametric_adjusted[cdata_pc]),
                             pvalf.params)),
                           "\\\\"),

               # Non-parametric p-values (adjusted)
               anparp = pst(" & ", uvnpartest, "* (PC", cdata_pc, ") ",
                            pst(" & ", pvalf.f(
                              sapply(micmp, function(mc)
                                mc$p.values$nonparametric_adjusted[cdata_pc]),
                              pvalf.params)),
                            "\\\\"),

               # Percentage of explained variance
               varexp = pst(" & \\% var. (PC", cdata_pc, ") ",
                            pst(" & ", sapply(micmp, function(x)
                              if (x$varexp[cdata_pc] < 0.0001) "<0.1"
                              else sprintf("%6.1f", x$varexp[cdata_pc] * 100)),
                              "\\%"),
                            "\\\\"),

               # Score plot
               scoreplot = pst(" & PCS ",
                               pst(" & ",
                                   tscat_apply(object[, cmp],
                                               scoreplot.marks,
                                               scoreplot.scale,
                                               scoreplot.before,
                                               scoreplot.after)),
                               "\\\\"))

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
  if (col.width) {
    ltxtab[[idx]] <- "} % resize box"
    idx <- idx + 1
  }

  # Add a caption, if specified
  if (!is.null(caption)) {
    ltxtab[[idx]] <- pst("\\caption{", caption,"}")
    idx <- idx + 1
  }

  # Add reference label, if specified
  if (!is.null(label)) {
    ltxtab[[idx]] <- pst("\\label{", label,"}")
    idx <- idx + 1
  }

  # Close specified LaTeX environments
  ltxtab[[idx]] <- pst("\\end{", rev(latex.environments) ,"}")
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
