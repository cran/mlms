% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlsx2json.R
\name{xlsx2json}
\alias{xlsx2json}
\title{Convert Field Data from XLSX to JSON}
\usage{
xlsx2json(path, destdir)
}
\arguments{
\item{path}{'character' string.
Either the path to the Excel workbook file (XLSX) to read,
or the directory containing XLSX files.}

\item{destdir}{'character' string.
Destination directory to write JSON file(s).
Defaults to the \code{path} directory.}
}
\value{
File path(s) of the JSON formatted data.
}
\description{
Convert the file format of field data from XLSX to JSON.
}
\examples{
path <- system.file("extdata/ex-field.xlsx", package = "mlms") |>
  xlsx2json(destdir = tempdir())

unlink(path)
}
\seealso{
\code{\link{read_field_xlsx}} function for reading field data from a XLSX file.

\code{\link{write_field_json}} function for writing field data to a JSON file.
}
\author{
J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
}
