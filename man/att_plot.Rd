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
xlim=NA, ylim=NA, legend=F, border.col=NA, cex=1)
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
\examples{
## Construct points for a grid shaped like a bowl with a hole
np <- 16 # Number of points on each ring
nr <- 12 # Number of rings
theta <- seq(0, 2*pi, len=np+1)[seq(np)]
r <- sqrt(seq(0.01, 8, len=nr))
x <- rep(r, each=length(theta)) * rep(sin(theta), times=length(r))
y <- rep(r, each=length(theta)) * rep(cos(theta), times=length(r))
nn <- length(x)

## Assemble the points into a grid
tri <- matrix(0, 2 * np * (nr - 1), 3)
for(i in seq(nrow(tri))) {
    st.node <- ceiling(i / 2)
    if(i \%\% 2) {
        tri[i,] <- c(st.node, st.node + np + 1, st.node + 1)     
    } else {
        tri[i,] <- c(st.node, st.node + np, st.node + np + 1)
    }
    if(!(i \%\% (2 *np)))
        tri[i,3] <- st.node + 1 ## Next layer
    if(!((i + 1) \%\% (2 * np))) {
        tri[i,2] <- st.node + 1
        tri[i,3] <- st.node - np + 1
    }
}
ne <- nrow(tri)
grid <- list(nodes=list(x=x, y=y, lat=y, lon=x, n=nn),
             elems=list(tri=tri, n=ne))

## Plot the grid
att.plot(grid, -seq(ne), col=bathy.colors, units='m')
}
\author{Benjamin Jones \email{btjones16@gmail.com}}
