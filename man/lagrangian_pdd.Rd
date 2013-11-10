\name{lagrangian.pdd}
\alias{lagrangian.pdd}
\title{Lagrangian probability density distribution}
\description{
  Generates a Lagrangian probability density distribution and plots it
  on an unstructured grid. This function follows the method described in
  Simons et al 2013, first vertically integrating the data, then
  dividing by the number of particles spawned, and finally applying an
  Gaussian blur filter. The coordinates should be passed in as x,y in
  meters, then are projected into lat/long using the PROJ.4 library.
  
}
\usage{
lagrangian.pdd(grid, x, y, proj, npart.released=length(x), res=c(1000,
1000), xlim=c(min(x), max(x)), ylim=c(min(y), max(y)))
}
\arguments{
  \item{grid}{An unstructured grid that could be passed to
    \code{plot.att}}
  \item{x}{A vector of particle x locations}
  \item{y}{A vector of particle y locations}
    \item{proj}{A string to project the (x,y) coordinates to
    latitude-longitude using the PROJ.4 library}
  \item{npart.released}{The number of particles released}
  \item{xlim.grid}{x limits for making the lpdf}
  \item{ylim.grid}{y limits for making the lpdf}
  \item{xlim}{x limits for plotting}
  \item{ylim}{y limits for plotting}
}
\references{
  Simons, Siegel, and Brown (2013) J Marine Systems. 119-120: 19-29.
}
\author{Benjamin Jones \email{btjones16@gmail.com}}