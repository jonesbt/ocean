\name{lagrangian.pdd}
\alias{lagrangian.pdd}
\title{Lagrangian probability density distribution}
\description{
  Generates a Lagrangian probability density distribution and plots it
  on an unstructured grid. This function follows the method described in
  Simons et al 2013: first vertically integrating the data, then
  dividing by the number of particles spawned, and finally applying an
  Gaussian blur filter. The coordinates should be passed in as x,y in
  meters, then are projected into lat/long using the PROJ.4 library.  
}
\usage{
lagrangian.pdd(grid, x, y, proj='', npart.released=length(x), res=c(1e3,
               1e3), log=F,
               xlim=c(min(x), max(x)), ylim=c(min(y), max(y)), zlim=NA)
}
\arguments{
  \item{grid}{An unstructured grid that could be passed to
    \code{plot.att}}
  \item{x}{A vector of particle x locations.}
  \item{y}{A vector of particle y locations.}
  \item{proj}{A string to project the (x,y) coordinates to
    latitude-longitude using the PROJ.4 library. If proj is an empty
    string, the points are not projected, and units='m' is passed to
    \code{att.plot}.}
  \item{npart.released}{The number of particles released.}
  \item{res}{The size of the grid cells to bin particles into in
    meters.}
  \item{log}{Should the data be plotted on a log scale}
  \item{xlim}{x limits for plotting and binning the data.}
  \item{ylim}{y limits for plotting and binning the data.}
  \item{zlim}{z limits to be passed through to \code{image}.}
}
\examples{
# Use the grid from the att.plot example
example(att.plot)
# Create a set of particle locations
x <- rnorm(10000)
y <- rnorm(10000)
# Plot the locations
grd <- lagrangian.pdd(grid, x, y, res=c(0.03, 0.03))
}
\references{
  Simons, R.D. and Siegel, D.A. and Brown K.S. 2013 Model sensitivity
  and robustness in the estimation of larval transport: A study of
  particle tracking parameters \emph{J. Marine Systems} 119--120:
  19--29.
}
\author{Benjamin Jones \email{btjones16@gmail.com}}