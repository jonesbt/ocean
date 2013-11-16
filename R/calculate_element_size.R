#' Calculate the size of each triangular element in the grid
#'
#' \code{calculate.element.size} returns the grid passed in with an additional
#' attribute grid$elems$size that has the size of each element
#'
#' \param grid a grid for which to calculate the element sizes
#' \return the grid with element sizes
calculate.element.size <- function(grid) {
    grid$elems$size <- rep(NA, length(grid$elems$x))
    for(i in 1:length(grid$elems$x)) {
        x1 <- with(grid, nodes$x[elems$tri[i,1]])
        x2 <- with(grid, nodes$x[elems$tri[i,2]])
        x3 <- with(grid, nodes$x[elems$tri[i,3]])
        y1 <- with(grid, nodes$y[elems$tri[i,1]])
        y2 <- with(grid, nodes$y[elems$tri[i,2]])
        y3 <- with(grid, nodes$y[elems$tri[i,3]])
        grid$elems$size[i] <-
            det(matrix(c(x1, x2, x3,
                         y1, y2, y3,
                         1, 1, 1),
                   3, 3, byrow=T)
                ) / 2
    }
    return(grid)
}
