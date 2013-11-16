\name{spaghetti}
\alias{spaghetti}
\title{Draw spaghetti lines of trajectories.}
\description{
  Using the trajectories given by \code{x} and \code{y},
  \code{spaghetti} draws lines along each trajectory. The lines are
  colored using the passed in function \code{col}. \code{x} and \code{y}
  should be 2d matrices with particles along the first axis and time
  along the second axis.
}
\usage{
  spaghetti(x, y, col=heat.colors, add=F, lwd=1)
}
\arguments{
  \item{x}{x locations.}
  \item{y}{y locations.}
  \item{col}{A function that takes 1 argument \code{n}, and returns
    \code{n} colors that will be used to color the trajectories.}
  \item{add}{Should the lines be added to the current plot.}
  \item{lwd}{Line width. See \code{par}}
}
\examples{
# Create a set of random trajectories.
x <- matrix(rnorm(10000), 100)
y <- matrix(rnorm(10000), 100)
for(i in seq(100)) {
  x[i,] <- cumsum(x[i,])
  y[i,] <- cumsum(y[i,])
}
spaghetti(x, y, col=jet.colors)
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
    
  
  