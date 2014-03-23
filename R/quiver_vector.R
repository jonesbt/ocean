# Origin: Nemo

quiver_vector <- function(x, y, u, v, n=length(x), add=F, zlim=NA) {
    if(length(u) != length(x) || length(v) != length(x) ||
       length(x) != length(y)) {
        print("Unsupported dimensions for quiver_vector")
        return()
    }

    ## Cannot have more points than there are available
    if(n > length(x))
        n <- length(x)

    ## Reduce the variables
    idx <- floor(seq(1, length(x), len=n))
    x <- x[idx]
    y <- y[idx]
    u <- u[idx]
    v <- v[idx]

    ## Find the length of the longest arrow and the arrowheads
    vec_len <- 5 #TODO Use par to calculate the max length
    u <- u / max(u) * vec_len
    v <- v / max(v) * vec_len
    
    ## Do the actual plotting
    if(!add)
        plot(x, y, type='n')
    ## Suppress the warnings for 0 length vectors
    ## TODO Fix this
    suppressWarnings(arrows(x, y, x + u, y + v, len=0.01))
}
