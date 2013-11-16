\name{get.var.fortran}
\alias{get.var.fortran}
\title{Reads a variable from a Fortran binary file}
\description{
  Reads a single variable from a binary file created by the Fortran
  intrinsic \code{write} statement. The variable type  may be one of "int"
  (4 bytes), "float" (4 bytes), or "double" (8 bytes). 
}
\usage{
  get.var.fortran(fid, type)
}
\arguments{
  \item{fid}{An open file identifier to read from.}
  \item{type}{One of "int", "float", or "double".}
}
\value{
  A vector with the read data.
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
