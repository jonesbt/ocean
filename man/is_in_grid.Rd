\name{is.in.grid}
\alias{is.in.grid}
\title{Checks is a point is within a grid}
\description{
  Given a grid and a point or vector of points, \code{is.in.grid}
  returns a binary vector of length \code{length(x)}. The vectors
  \code{x} and \code{y} must be the same length.
}
\usage{
  is.in.grid(x, y, grid, units='ll')
}
\arguments{
  \item{x}{x coordinates.}
  \item{y}{y coordinates.}
  \item{grid}{The grid to plot the data on.}
  \item{units}{\code{x} and \code{y} units. If 'm', the \code{x} and
    \code{y} coordinates in \code{grid} are used. If 'll', the
    \code{lat} and \code{lon} variables in \code{grid} are used.}
}
\value{
  A binary vector with TRUE if each point is in the grid and FALSE
  otherwise.
}
\examples{
## Use the grid created in the att.plot example
example(att.plot)
## Create a regular matrix grid
lattice.grid <- expand.grid(x=seq(min(grid$nodes$x), max(grid$nodes$x),
                            len=10), y=seq(min(grid$nodes$y),
                            max(grid$nodes$y), len=10))
elems <- is.in.grid(lattice.grid$x, lattice.grid$y, grid, units='m')
## Plot the result, but only points in the grid
plot(lattice.grid$x[elems], lattice.grid$y[elems])
}
\author{Benjamin Jones \email{btjones16@gmail.com}}