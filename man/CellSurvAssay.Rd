% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CellSurvAssay.R
\docType{package}
\name{CellSurvAssay}
\alias{CellSurvAssay}
\title{CellSurvAssay}
\description{
This R package has been written around
\href{https://bioconductor.org/packages/release/bioc/html/CFAssay.html}{CFAssay},
another R package that can be used for Clonogenic Survival Analysis analysis
in R. However, CellSurvAssay has it’s own purposes and advantages:
}
\section{Purpose}{
 \itemize{ \item it makes performing Clonogenic Survival
  Analysis in R ncredibly user-friendly and efficient, even for beginner R
  users who don't have the luxury of time to dig deeper into R, \item it
  arranges all the commonly used steps of clonogenic assay analysis in one
  location and automates the data wrangling steps to the extent that only
  single lines of ode suffice for each step of the analysis, \item it
  utilizes \code{\link[ggplot2]{ggplot}} to plot the cell survival curves,
  and builds better quality figures than other available R packages, \item it
  is less time consuming and more convenient for the user, as it accepts the
  raw data for the analysis and calculates the plating efficiencies by
  itself, unlike many automated software commonly used, \item it offers
  various method options for parameter estimation and calculating plating
  efficiencies, unlike most other available software tools, and \item as R is
  being utilized, the methodology stays open and the results reproducible. }
}

\section{Functions in the package}{
 \itemize{ \item \code{\link{importData}}
  imports the data set in R \item \code{\link{lqmodelFit}} fits the linear
  quadratic model \item \code{\link{plotCSCurve}} plots a regular cell
  survival curve \item \code{\link{ggplotCSCurve}} plots the cell survival
  curves using \code{ggplot()} \item \code{\link{compareCurves}}
  statistically compares two cell survival curves \item
  \code{\link{calculateDER}} calculates the Dose Enhancement Ratio }
}

\seealso{
Please refer to the
  \href{https://bioconductor.org/packages/release/bioc/html/CFAssay.html}{CFAssay}
   R package for the statistics behind this package.
}
