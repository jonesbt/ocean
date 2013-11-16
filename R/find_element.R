find.elem <- function(x, y, grid) {
    if(length(x) != length(y)) {
        print("Error: length(x) != length(y)")
        return(-1)
    }
    elems <- rep(-1, length(x))
    elems <- .C('is_in_grid',
              x_pts=as.double(x), y_pts=as.double(y), n_pts=length(x),
              x=as.double(grid$nodes$lon), y=as.double(grid$nodes$lat),
              n_grid_pts=as.integer(grid$elems$n),
              tri1=as.integer(grid$elems$tri[,1] - 1),
              tri2=as.integer(grid$elems$tri[,2] - 1),
              tri3=as.integer(grid$elems$tri[,3] - 1),
              elems=as.integer(elems))$elems
    return(elems)
}

