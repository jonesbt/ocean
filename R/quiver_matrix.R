# Origin: Nemo

quiver_matrix <- function(x, y, u, v, nx=length(x), ny=length(y), add=F,
                          zlim=NA) {
    if(length(u) != (length(x) * length(y)) ||
       length(u) != (length(x) * length(y))) {
        print("u and v must be of length length(x) * length(y)")
        return()
    }
    if(length(x) < 2 || length(y) < 2) {
        print("x and y must be at least length 2")
        return()
    }

        
    ## Cannot have more x points than are available (no interpolation allowed)
    if(nx > length(x))
        nx <- length(x)
    if(ny > length(y))
        ny <- length(y)

    ## Convert u, v to matrices if necessary
    if(!is.matrix(u))
        u <-t(matrix(u, length(x), length(y)))
    if(!is.matrix(v))
        v <- t(matrix(v, length(x), length(y)))
    
    ## Reduce the x, y, u, and v variables to the specified nx, ny
    x_idx <- floor(seq(1, length(x), len=nx))
    y_idx <- floor(seq(1, length(y), len=ny))
    x <- x[x_idx]
    y <- y[y_idx]
    u <- u[x_idx, y_idx]
    v <- v[x_idx, y_idx]
    
    ## Find the length of the longest arrow
    vec_len <- abs(x[2] - x[1])
    for(i in seq(2, length(x)))
        vec_len <- min(vec_len, x[i] - x[i - 1])
    for(i in seq(2, length(y)))
        vec_len <- min(vec_len, y[i] - y[i - 1])
    vel <- sqrt(u^2 + v^2)
    u <- u / max(vel) * vec_len
    v <- v / max(vel) * vec_len
    
    ## Find the length of the arrowheads (20% of the longest vector)
    ## TODO Arrowhead length
    
    ## Do the actual plotting
    cds <- expand.grid(x=x, y=y)
    x <- cds$x
    y <- cds$y
    u <- as.vector(u)
    v <- as.vector(v)
    quiver_vector(x, y, u, v, add)
}
