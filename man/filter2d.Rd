\name{filter2d}
\alias{filter2d}
\title{2D Gaussian Filter}
\description{
  Applies a Gaussian blur to a 2D matrix. The matrix is first convoluted
  with the filter along rows, then along columns.
}
\usage{
  filter2d(mat, r)
}
\arguments{
  \item{mat}{The matrix to filter.}
  \item{r}{The standard deviation of the filter in matrix cells.}
}
\value{
  The filtered matrix.
}
\author{Benjamin Jones \email{btjones16@gmail.com}}