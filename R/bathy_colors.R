#' Generate a sequence of colors for plotting bathymetric data.
#' 
#' \code{bathy.colors} generates a sequence of \eqn{n} colors along a linear
#' scale from light grey to pure blue. 
#'
#' \param n the number of colors to return
#' \param alpha alpha values to be passed to rgb
#' \return a vector of blue scale colors
bathy.colors <- function(n, alpha=1)
    cols <- rgb(seq(0.9,0,len=n), seq(0.9,0,len=n), 1, alpha)
