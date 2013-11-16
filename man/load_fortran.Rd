\name{load.fortran}
\alias{load.fortran}
\title{Loads a Fortran binary file}
\description{
  Reads the entire contents of a binary file created with the Fortran
  intrinsic \code{write} statement. The contents of the file are
  returned as a list, where each element of the list is a single
  variable.
}
\usage{
  load.fortran(filename, type)
}
\arguments{
  \item{filename}{The name of the file to read in.}
  \item{type}{The type of data in the file. This must be one of "int",
    "float", or "double."}
}
\value{
  A list with the contents of the file.
}
\author{Benjamin Jones \email{btjones16@gmail.com}}