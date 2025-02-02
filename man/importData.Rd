% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importData.R
\name{importData}
\alias{importData}
\title{Imports the data}
\usage{
importData(filepath, filetype = "excel", separator = NULL, ...)
}
\arguments{
\item{filepath}{Path of the file to be imported.}

\item{filetype}{The type of file to be imported. By default, it's
\code{"excel"}, but also accepts \code{"tsv"}, \code{"csv"}, or
\code{"txt"}.}

\item{separator}{The delimiter in your data set, if it's a \code{"txt"} file.}

\item{...}{Any other argument that can be passed in the
\code{\link[readr]{read_csv}}, \code{\link[readr]{read_tsv}},
\code{\link[readr]{read_delim}}, or \code{\link[readxl]{read_excel}}
functions, if your data set is a csv, tsv, txt, or an excel file,
respectively.}
}
\value{
A data frame
}
\description{
It helps import the data set, whether it's in a tsv, csv, txt, or excel
format.
}
\examples{
\dontrun{
# If it's an excel file
importData("path/to/file")

# If it's a csv file
importData("path/to/file", filetype = "csv")

# If it's a txt file with each observation separated by a "|"
importData("path/to/file", filetype = "txt", separator = "|")
}
}
