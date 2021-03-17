#' Fits the Linear Quadratic Model
#'
#' It helps fit the linear quadratic model using the \code{\link[CFAssay:cellsurvLQfit]{cellsurvLQfit}} function of the CFAssay package, without having us go through the data wrangling steps necessary for it. Visit \url{https://bioconductor.org/packages/release/bioc/html/CFAssay.html} for more details about the method of the fit.
#'
#' @param data A data frame containing at least the following five columns: "cline", "Exp", "dose", "ncells", "ncolonies".
#' @param ctype Name of the cell-line/group for which the model is to be fit.
#' @param method Method used for the fit. It's \code{"ml"} (maximum likelihood) by default. Can be \code{"ls"} (least squares) or \code{"franken"} (weighted least squares as described by Franken eta al.(2006)).
#' @param PEmethod Controls the value of the plating efficiencies. \code{"fit"} calculates fitted plating efficiencies as model parameters, \code{"fix"} uses fixed ones calculated from the observed zero dose data or from a column named \code{pe} in \code{data}.
#' @return An object of class \code{cellsurvLQfit}, as returned by \code{\link[CFAssay:cellsurvLQfit]{cellsurvLQfit}}.
#' @examples
#' lqmodelFit(datatab, HN31)
#' lqmodelFit(datatab, HN31, method = "ls", PEmethod = "fix")
#' @export
lqmodelFit <- function(data, ctype, method = "ml", PEmethod = "fit") {
  invisible(utils::capture.output(fit <- CFAssay::cellsurvLQfit(subset(data, cline == ctype), method = method, PEmethod = PEmethod)))
  return(fit)
}
