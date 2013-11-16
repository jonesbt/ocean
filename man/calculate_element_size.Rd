\name{calculate.element.size}
\alias{calculate.element.size}
\title{Calculates the size of element in the grid}
\description{
  Given an unstructured grid, \code{calculate.element.size} calculates
  the area of each element in it. The sizes are added to the grid as an
  additional attribute \code{grid$elems$size} and the modified grid is
  returned.
}
\arguments{
  \item{grid}{The grid for which element sizes should be calculated.}
}
\value{
  A modified grid with \code{grid$elems$size} added.
}
\examples{
## Use the grid created in the att.plot example
example(att.plot)
## Calculate the size of the elements and plot them
grid <- calculate.element.size(grid)
att.plot(grid, grid$elems$size)
}
\author{Benjamin Jones \email{btjones16@gmail.com}}