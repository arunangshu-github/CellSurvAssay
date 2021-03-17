#' Plots Cell Survival Curves
#'
#' This helps to plot the cell survival curves. One function plots the curves, both for one cell type or multiple cell types, without having us go through the data wrangling steps.
#' It plots the type of plots returned by the \code{\link[CFAssay:plot.cellsurvLQfit]{plot.cellsurvLQfit}} function.
#'
#' @param data A data frame containing at least the following five columns: "cline", "Exp", "dose", "ncells", "ncolonies".
#' @param ... The name of the cell type(s)/group(s). If entering multiple cell types, separate each by a comma.
#' @param method Method used for the fit. It's \code{"ml"} (maximum likelihood) by default. Can be \code{"ls"} (least squares) or \code{"franken"} (weighted least squares as described by Franken eta al.(2006)).
#' @param PEmethod Controls the value of the plating efficiencies. \code{"fit"} calculates fitted plating efficiencies as model parameters, \code{"fix"} uses fixed ones calculated from the observed zero dose data or from a column named \code{pe} in \code{data}.
#' @param col A vector of strings denoting the colors of the curves. Size of the vector should be equal to the number of cell types entered.
#' @param xlim A vector denoting the limits of x-axis.
#' @param ylim A vector denoting the limits of y-axis.
#' @param xlab A string denoting the label of x-axis.
#' @param ylab A string denoting the label of y-axis.
#' @param title A string denoting the title of the plot.
#' @param pch An integer denoting the shape of the points in the plot.
#' @examples
#' # Single curve
#' plotCSCurve(datatab, "control-B")
#' plotCSCurve(datatab, "control-NT", col = "blue", pch = 4, ylim = c(0.1, 1), xlab = "X-axis", ylab = "Y-axis", title = "Curve of Control-NT")
#' # Multiple curves
#' plotCSCurve(datatab, "control-B", "control-NT")
#' plotCSCurve(datatab, "shCASP8-N", "shCASP8-B+Z+N", "control-B", col = c("red", "blue", "purple"), pch = 4, ylim = c(0.1, 1), xlab = "X-axis", ylab = "Y-axis", title = "Multiple Curves")
#' @export
plotCSCurve <- function(data, ..., method = "ml", PEmethod = "fit", col = NULL, xlim = NULL, ylim = c(0.008, 1),
                           xlab = "Dose (Gy)", ylab = "Survival (1 = 100%)", title = NULL, pch = 1) {





  myplot.cellsurvLQfit <- function (x, xlim = NULL, ylim = c(0.008, 1), xlab = "Dose (Gy)",
                                    ylab = "Survival (1 = 100%)", col = 1, pch = 1, add = FALSE, title = NULL,
                                    ...)
  {
    fit <- x
    if (!class(fit) == "cellsurvLQfit")
      stop("Fit object not of class 'cellsurvLQfit'!")
    data <- fit$data
    data$dose2 <- data$dose^2
    data$lcells <- log(data$ncells)
    uexp <- unique(data$Exp)
    doses <- unique(data$dose)
    maxd <- max(doses)
    b <- fit$coef[c("dose", "dose2")]
    if (is.null(xlim))
      xlim <- c(0, maxd)
    graphics::par(lwd = 2)
    if (!add)
      graphics::curve(exp(b[1] * x + b[2] * x^2), from = 0, to = maxd,
                      log = "y", col = col, xlim = xlim, ylim = ylim,
                      xlab = xlab, ylab = ylab, xaxt = "n", main = title)
    if (add)
      graphics::curve(exp(b[1] * x + b[2] * x^2), from = 0, to = maxd,
                      col = col, xaxt = "n", main = title, add = TRUE)
    if (0 %in% doses) {
      S0 <- CFAssay::pes(data)$S0
      names(S0) <- rownames(CFAssay::pes(data))
      meanSF <- CFAssay::sfpmean(data, S0)
    }
    if (!(0 %in% doses)) {
      data$pe <- exp(data$logPle)
      meanSF <- CFAssay::sfpmean(data)
    }
    pts <- meanSF[1, ]
    sems <- meanSF[2, ]
    graphics::points(doses, pts, col = col, pch = pch)
    graphics::segments(x0 = doses, y0 = pts - sems, x1 = doses, y1 = pts +
                         sems, col = col)
    axis(1, at = doses)
  }





  cell_types <- c(...)
  invisible(utils::capture.output(fit <- CFAssay::cellsurvLQfit(subset(data, cline == cell_types[1]), method = method, PEmethod = PEmethod)))
  if (is.null(col)) {
    myplot.cellsurvLQfit(fit, col = 1, ylim = ylim, xlab = xlab, ylab = ylab, pch = pch, title = title)
    if (length(cell_types) > 1) {
      for (i in 2:length(cell_types)) {
        invisible(utils::capture.output(fit <- CFAssay::cellsurvLQfit(subset(data, cline == cell_types[i]), method = method, PEmethod = PEmethod)))
        myplot.cellsurvLQfit(fit, col = i, add = TRUE, ylim = ylim, xlab = xlab, ylab = ylab, pch = pch, title = title)
      }
    }
    graphics::legend("bottomleft", cell_types, text.col = 1:length(cell_types))
  }else {
    myplot.cellsurvLQfit(fit, col = col[1], ylim = ylim, xlab = xlab, ylab = ylab, pch = pch, title = title)
    if (length(cell_types) > 1) {
      for (i in 2:length(cell_types)) {
        invisible(utils::capture.output(fit <- CFAssay::cellsurvLQfit(subset(data, cline == cell_types[i]), method = method, PEmethod = PEmethod)))
        myplot.cellsurvLQfit(fit, col = col[i], add = TRUE, ylim = ylim, xlab = xlab, ylab = ylab, pch = pch, title = title)
      }
    }
    graphics::legend("bottomleft", cell_types, text.col = col)
  }
}
