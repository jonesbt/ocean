calculate.element.size <- function(grid) {
    grid$elems$size <- rep(NA, length(grid$elems$x))
    for(i in seq(nrow(grid$elems$tri))) {
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
