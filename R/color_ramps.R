#' Generate a sequence of colors for plotting bathymetric data.
#' 
#' \code{bathy.colors(n)} generates a sequence of \eqn{n} colors along a
#' linear scale from light grey to pure blue. 
#'
#' @param n The number of colors to return.
#' @param alpha Alpha values to be passed to \code{rgb()}.
#' @return A vector of blue scale colors.
#'
#' @examples {
#' # Plot a colorbar using bathy.colors
#' image(matrix(seq(100), 100), col=bathy.colors(100))
#' }
bathy.colors <- function(n, alpha=1)
    return(rgb(seq(0.9,0,len=n), seq(0.9,0,len=n), seq(0.9, 1, len=n), alpha))

#' Generate an alternative sequence of colors for plotting bathymetric data.
#'
#' \code{bathy.colors(n) generates a sequence of \eqn{n} colors that begins with
#' tan, then green, and ends with increasingly dark hues of blue.
#'
#' @param n The number of colors to return.
#' @param alpha. The transparency value of the colors. See \code{?rgb} for
#'               details.
#' @return A vector of colors.
carib.colors <- function(n, alpha=1) {
    if(n < 0) {
        stop('The number of colors requested must be positive.')
    } else if(n == 0) {
        ## Return an empty character string if they requested nothing.
        character()
    } else {
        if(length(alpha) != 1 & length(alpha) != n) {
            print('Warning: using only first alpha value')
            alpha <- alpha[1]
        }
        if(sum(alpha < 0 | alpha > 1) > 0)
            stop('alphas must be between 0 and 1.')
        col = colorRampPalette(c('#F5EBCC', '#7FCDBB', '#003399', '#000066'))(n)
        col = col2rgb(col) / 255
        lum = col[1,] * 0.299 + col[2,] * 0.587 + col[3,] * 0.114
        target_lum = seq(0.95, 0.05, len=n)
        lum_adj = target_lum / lum - 1
        col[1,] = col[1,] + col[1,] * lum_adj
        col[2,] = col[2,] + col[2,] * lum_adj
        col[3,] = col[3,] + col[3,] * lum_adj
        col[col < 0] = 0
        col[col > 1] = 1
        return(rgb(col[1,], col[2,], col[3,], alpha=alpha))
    }
}

#' Generate a sequence of colors alog the jet colormap.
#'
#' \code{jet.colors(n)} generates a sequence of \eqn{n} colors from dark blue
#' to cyan to yellow to dark red. It is similar to the default color schemes
#' in Python's matplotlib or MATLAB.
#'
#' @param n The number of colors to return.
#' @param alpha The transparency value of the colors. See \code{?rgb} for
#'              details.
#' @return A vector of colors along the jet colorramp.
#'
#' @examples {
#' # Plot a colorbar with jet.colors
#' image(matrix(seq(100), 100), col=jet.colors(100))
#' }
jet.colors <- function(n, alpha=1) {
    if(n < 0) {
        stop('The number of colors requested must be positive.')
    } else if(n == 0) {
        ## Return an empty character string if they requested nothing.
        character()
    } else {
        if(length(alpha) != 1 & length(alpha) != n) {
            print('Warning: using only first alpha value')
            alpha <- alpha[1]
        }
        if(sum(alpha < 0 | alpha > 1) > 0)
            stop('alphas must be between 0 and 1.')
        col = colorRampPalette(c('#000066', 'blue', 'cyan', 'yellow',
                                  'red', '#660000'))(n)
        col = col2rgb(col) / 255
        return(rgb(col[1,], col[2,], col[3,], alpha=alpha))
    }
}

