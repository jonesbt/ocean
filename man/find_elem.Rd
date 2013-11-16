\name{find.elem}
\alias{find.elem}
\title{Finds the element number for a point in a grid}
\description{
  Given a grid and a point or set of points, \code{find.elem} returns
  the element number of each point in the grid. -1 is returned for any
  point that does not lie within the grid. \code{x} and \code{y} must be
  the same length.
}
\usage{
  find.elem(x, y, grid)
}
\arguments{
  \item{x}{x coordinates}
  \item{y}{y coordinates}
  \item{grid}{The grid to search for these points.}
}
\value{
  A vector with the element number for points on the grid, or -1 for
  points not on the grid.
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
  