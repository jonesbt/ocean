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
  find.elem(x, y, grid, units='ll')
}
\arguments{
  \item{x}{x coordinates}
  \item{y}{y coordinates}
  \item{grid}{The grid to search for these points.}
    \item{units}{\code{x} and \code{y} units. If 'm', the \code{x} and
    \code{y} coordinates in \code{grid} are used. If 'll', the
    \code{lat} and \code{lon} variables in \code{grid} are used.}
}
\value{
  A vector with the element number for points on the grid, or -1 for
  points not on the grid.
}
\examples{
## Use the grid created in the att.plot example
example(att.plot)
## Create a regular matrix grid
lattice.grid <- expand.grid(x=seq(min(grid$nodes$x), max(grid$nodes$x),
                            len=10), y=seq(min(grid$nodes$y),
                            max(grid$nodes$y), len=10))
elems <- find.elem(lattice.grid$x, lattice.grid$y, grid, units='m')
elems
## Plot the result, but only points in the grid
elems[elems == -1] <- NA
plot(lattice.grid$x, lattice.grid$y, pch=15,
     col=heat.colors(max(elems, na.rm=TRUE) + 2)[elems+2])
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
  