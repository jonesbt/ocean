\name{bathy.colors}
\alias{bathy.colors}
\title{Bathymetric data color scheme}
\description{
  Generates a color ramp from light grey to blue.
}
\usage{
  bathy.colors(n, alpha=1)
}
\arguments{
  \item{n}{The number of colors to generate.}
  \item{alpha}{Alpha values to be passed through to \code{rgb}.}
}
\value{
  A vector of \code{n} colors linearly spaced from light grey to pure blue.
}
\examples{
  # Plot a matrix using bathy.colors
  mat <- matrix(seq(100), 100, 1)
  image(mat, col=bathy.colors(100))

  # Plot a bowl chape using bathy.colors
  example(att.plot)
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
  