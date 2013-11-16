\name{att.plot}
\alias{att.plot}
\title{Plot an attribute on an unstructured grid}
\description{
  Given a vector of data, this function plots the data as a heatmap on
  an unstructured grid. The length of the data vector must be as long as
  either the number nodes in the grid or the number of elements in the
  grid. The grid is currently stored as a data.frame, but will be
  converted to an S4 object in the future.
}
\usage{
att.plot(grid, att, col=heat.colors, add=F, zlim=NA, units='ll',
xlim=NA, ylim=NA, legend=T, border.col=NA, cex=1)
}
\arguments{
  \item{grid}{The grid to plot the attribute on. It must be a data.frame
    with \code{grid$nodes$n}, \code{grid$nodes$x}, \code{grid$nodes$y},
    \code{grid$elems$tri}, and\code{grid$elems$n}. \code{grid$elems$tri} is
    a \code{grid$elems$n} by 3 matrix with the indices of the nodes that
    border each element.}
  \item{att}{A vector to plot as a heatmap.}
  \item{col}{A function that takes a singel arguement \code{n} and
    returns \code{n} colors.}
  \item{add}{Should this plot be added to the current plot}
  \item{zlim}{Limits for the highest and lowest color intensity colors}
  \item{units}{Either 'll' for latitude and longitude or 'm' for meters}
  \item{xlim}{x limits for the plot}
  \item{ylim}{y limits for the plot}
  \item{legend}{Should a colorbar legend be added}
  \item{border.col}{Color of the polygon border colors}
  \item{cex}{Character expansion for \code{cex} and \code{cex.*}.
    See \code{par}.}
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
