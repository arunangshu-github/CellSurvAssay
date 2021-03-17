#' Plots Cell Survival Curves using ggplot()
#'
#' This helps to plot the cell survival curves using ggplot().
#' One function plots the curves, both for one cell type , or multiple cell types.
#'
#' @param data A data frame containing at least the following five columns: "cline", "Exp", "dose", "ncells", "ncolonies".
#' @param ... The name of the cell type(s)/group(s). If entering multiple cell types, separate each by a comma.
#' @param method Method used for the fit. It's \code{"ml"} (maximum likelihood) by default. Can be \code{"ls"} (least squares) or \code{"franken"} (weighted least squares as described by Franken eta al.(2006)).
#' @param PEmethod Controls the value of the plating efficiencies. \code{"fit"} calculates fitted plating efficiencies as model parameters, \code{"fix"} uses fixed ones calculated from the observed zero dose data or from a column named \code{pe} in \code{data}.
#' @param colors A vector of strings denoting the colors of the curves. Size of the vector should be equal to the number of cell types entered.
#' @param xlab A string denoting the label of x-axis.
#' @param ylab A string denoting the label of y-axis.
#' @param title A string denoting the title of the plot.
#' @param legend_title A string denoting the title of the legend.
#' @param point_shape An integer denoting the shape of the points in the plot.
#' @param point_size An integer denoting the size of the points in the plot.
#' @param segment_type An integer denoting the type of the straight line segments in the plot.
#' @param segment_width An integer denoting the width of straight line segments in the plot.
#' @param curve_width An integer denoting the width of curved line segments in the plot.
#' @param curve_type An integer denoting the type of curved line segments in the plot.
#' @param theme The name of the theme to be used in the plot
#' @param ylim A vector denoting the limits of the y-axis
#' @param ybreaks A vector denoting the y-axis breaks/ticks
#' @examples
#' # Single curve
#' ggplotCSCurve(datatab, "shCASP8-NT")
#' # This plot replicates the plot created by the CFAssay function plot.cellsurvLQfit
#' ggplotCSCurve(datatab, "control-B", colors = "black", xlab = "X-axis", ylab = "Y-axis", title = "Single Plot", point_shape = 1, point_size = 2, segment_width = 1, segment_type = 1, curve_width = 1, curve_type = 1, legend_title = "Legend", ylim = c(0.008, 1.05), ybreaks = c(0.01, 0.05, 0.20, 0.50, 1), theme = theme_classic())
#' # Multiple Curves
#' ggplotCSCurve(datatab, "shCASP8-N", "shCASP8-B+Z+N", "control-B", "control-NT")
#' ggPlotforCFAssay(datatab, "shCASP8-N", "shCASP8-B+Z+N", "control-B", "control-NT", colors = c("darkgreen", "steelblue", "orange", "magenta"), xlab = "X-axis", ylab = "Y-axis", title = "Multiple Curves", point_shape = 15, point_size = 1, segment_width = 0.5, segment_type = 1, curve_width = 1, curve_type = 1, legend_title = "Legend", theme = theme_dark())
#' @export
ggplotCSCurve <- function(data, ..., method = "ml", PEmethod = "fit", colors = NULL, title = NULL, xlab = "Doses (Gy)", ylab = "Survival (1 = 100%)",
                             legend_title = "Cell types", point_shape = 16, point_size = 2, segment_type = 1, segment_width = 1,
                             curve_width = 1, curve_type = 1, theme = theme_bw(), ylim = NULL, ybreaks = waiver()) {






  dfforggPlot <- function(datatable, cell_type, method, PEmethod) {
    alpha <- c()
    beta <- c()
    pts_minus_sems <- c()
    pts_plus_sems <- c()
    points <- c()
    subset_data <- subset(datatable, cline == cell_type)
    invisible(utils::capture.output(fit <- CFAssay::cellsurvLQfit(subset_data), method))
    x <- fit
    data <- fit$data
    data$dose2 <- data$dose^2
    data$lcells <- log(data$ncells)
    uexp <- unique(data$Exp)
    doses <- unique(data$dose)
    maxd <- max(doses)
    b <- fit$coef[c("dose", "dose2")]
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
    points <- c(points, pts)
    pts_plus_sems <- c(pts + sems)
    pts_minus_sems <- c(pts - sems)
    alpha <- rep(b[1], times = length(doses))
    beta <- rep(b[2], times = length(doses))
    ctype <- rep(cell_type, times = length(doses))
    df <- data.frame(ctype, alpha, beta, doses, pts, pts_plus_sems, pts_minus_sems)
    rownames(df) <- NULL
    return(df)
  }






  cell_types <- c(...)
  l <- length(cell_types)
  merged_df <- dfforggPlot(data, cell_types[1], method, PEmethod)


  if (l > 1) {
    for (i in 2:l) {
      df <- dfforggPlot(data, cell_types[i], method, PEmethod)
      merged_df <- rbind(merged_df, df)
    }
  }
  dose_0_df <- merged_df %>% dplyr::filter(doses == 0)
  alpha_vec <- c(dose_0_df$alpha)
  beta_vec <- c(dose_0_df$beta)
  doses <- unique(merged_df$doses)
  n_seq <- 100
  df_sim <- data.frame(d = rep(with(merged_df, seq(min(doses), max(doses), length.out = n_seq)), l),
                       ctype = rep(cell_types, each = n_seq),
                       alpha = rep(alpha_vec, each = n_seq),
                       beta = rep(beta_vec, each = n_seq)) %>%
    dplyr::mutate(y_hat = exp(alpha*d + beta*(d^2)))
  if (is.null(colors)) {
    colors <- randomcoloR::distinctColorPalette(l)
  }


  p <- ggplot2::ggplot(data = merged_df, ggplot2::aes(x = doses, y = pts, color = ctype)) +
    ggplot2::geom_point(ggplot2::aes(x = doses, y = pts), shape = point_shape, size = point_size) +
    ggplot2::geom_segment(ggplot2::aes(x = doses, xend = doses, y = pts_minus_sems, yend = pts_plus_sems), linetype = segment_type, size = segment_width) +
    #geom_pointrange(aes(ymin = pts_minus_sems, ymax = pts_plus_sems), shape = point_shape, linetype = segment_type, size = segment_width) +
    ggplot2::geom_line(data = df_sim, ggplot2::aes(y = y_hat, x = d, color = ctype), linetype = curve_type, size = curve_width) +
    #ylim(ylimits) +
    ggplot2::scale_y_log10(limits = ylim, breaks = ybreaks) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(title = title, x = xlab, y = ylab, color = legend_title) +
    theme
  print(p)
}
