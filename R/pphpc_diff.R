# Copyright (c) 2016-2018 Nuno Fachada
# Distributed under the MIT License (http://opensource.org/licenses/MIT)

#' Data from two implementations of the PPHPC model, one of which setup with a
#' different parameter
#'
#' A dataset containing simulation output data from two implementations of the
#' PPHPC model, one of which setup with a different parameter.
#'
#' @format A \code{\link{grpoutputs}} object containing simulation output data
#' from 20 runs of the PPHPC model, 10 runs from each implementation. The model
#' has six outputs, but the object contains a seventh output corresponding to
#' the concatenation of the six outputs
#'
#' @source Runs are obtained from the NetLogo and Java (EX with 8 threads)
#' implementations of the PPHPC model available at
#' \url{https://github.com/nunofachada/pphpc}.  The \code{config400v1.txt}
#' configuration was used in both cases, with the exception of restart
#' parameter, \eqn{c_r}, in the Java implementation, which was set to 9 instead
#' of 10.
"pphpc_diff"