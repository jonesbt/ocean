find.elem <- function(x, y, grid, units='ll') {
    if(length(x) != length(y)) {
        print("Error: length(x) != length(y)")
        return(-1)
    }
    if(units == 'll') {
        grid.x <- grid$nodes$lon
        grid.y <- grid$nodes$lat
    } else if(units == 'm') {
        grid.x <- grid$nodes$x
        grid.y <- grid$nodes$y
    } else {
        print("Invalid units, must be 'll' or 'm'.")
        return(-1)
    }
    elems <- rep(-1, length(x))
    elems <- .C('R_find_element', PACKAGE='ocean',
              x_pts=as.double(x), y_pts=as.double(y), n_pts=length(x),
              x=as.double(grid.x), y=as.double(grid.y),
              n_grid_pts=as.integer(grid$elems$n),
              tri1=as.integer(grid$elems$tri[,1] - 1),
              tri2=as.integer(grid$elems$tri[,2] - 1),
              tri3=as.integer(grid$elems$tri[,3] - 1),
              elems=as.integer(elems))$elems + 1
    elems[elems == 0] <- -1
    return(elems)
}

