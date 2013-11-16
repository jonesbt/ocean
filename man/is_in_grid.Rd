\name{is.in.grid}
\alias{is.in.grid}
\title{Checks is a point is within a grid}
\description{
  Given a grid and a point or vector of points, \code{is.in.grid}
  returns a binary vector of length \code{length(x)}. The vectors
  \code{x} and \code{y} must be the same length.
}
\usage{
  is.in.grid(x, y, grid)
}
\arguments{
  \item{x}{x coordinates.}
  \item{y}{y coordinates.}
  \item{grid}{The grid to plot the data on.}
}
\value{
  A binary vector with TRUE if each point is in the grid and FALSE
  otherwise.
}
\author{Benjamin Jones \email{btjones16@gmail.com}}