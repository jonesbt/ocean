\name{jet.colors}
\alias{jet.colors}
\title{Jet color ramp}
\description{
  Provides an implementation of a color scheme similar to the Jet
  colormap produced by matplotlib.
}
\usage{
  jet.colors(n)
}
\arguments{
  \item{n}{The number of colors to produce.}
}
\value{
  A vector of colors of length \code{n}.
}
\examples{
  # Plot a matrix using bathy.colors
  mat <- matrix(seq(100), 100, 1)
  image(mat, col=jet.colors(100))
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
  