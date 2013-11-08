## Applies a 2d Gaussian filter to mat with a standard deviation of r cells.
filter.vec <- function(vec, r, i) {
    vec.out <- vec
    for(i in seq(length(vec))) {
        filt <- dnorm(seq(length(vec)), i, r)
        filt <- filt / sum(filt)
        vec.out[i] <- sum(vec * filt)
    }
    return(vec.out)
}

filter.row <- function(mat, r, i) {
    mat[i,] <- filter.vec(mat[i,], r)
    return(mat)
}

filter.col <- function(mat, r, i)
    return(t(filter.row(t(mat), r, i)))

filter2d <- function(mat, r) {
    for(i in seq(nrow(mat)))
        mat <- filter.row(mat, r, i)
    for(i in seq(ncol(mat)))
        mat <- filter.col(mat, r, i)
    return(mat)
}
