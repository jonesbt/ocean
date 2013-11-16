spaghetti <- function(x, y, col=heat.colors, add=F, lwd=1) {
    if(!add)
        plot(x, y, type='n')
    cols <- col(nrow(x))
    for(i in seq(nrow(x)))
        lines(x[i,], y[i,], col=cols[i], lwd=lwd)
}
