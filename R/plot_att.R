#This function plots the polygons specified in x and y using colors based on
#the values of att.
#' @todo Add error checking (zlim monotonically increasing)
#' @todo Fix legend scale
#' @todo Support passing in ... to plot
plot.att <- function(grid, att, col=heat.colors, add=F, zlim=NA, units="ll") {
    #If values at nodes, use average of 3 corners to calculate element values
    if(length(att) == grid$nodes$n) {
        att.tmp <- rep(NA, grid$elems$n)
        for(i in 1:grid$elems$n)
            att.tmp[i] <- mean(c(att[kimbe.grid$elems$tri[i,1]],
                                 att[kimbe.grid$elems$tri[i,2]],
                                 att[kimbe.grid$elems$tri[i,3]]))
        att <- att.tmp
        rm(att.tmp)
    }
    if(!is.na(zlim)[1]) {
        att <- apply(matrix(c(att, rep(zlim[1], length(att))), length(att)),
                     1, max)
        att <- apply(matrix(c(att, rep(zlim[2], length(att))), length(att)),
                     1, min)
    } else {
        zlim <- c(min(att), max(att))
    }
    if(units == "ll") { # Lat/lon
        x <- grid$nodes$lon[t(kimbe.grid$elems$tri)]
        y <- grid$nodes$lat[t(grid$elems$tri)]
    } else if(units == "m") {
        x <- grid$nodes$x[t(kimbe.grid$elems$tri)]
        y <- grid$nodes$y[t(grid$elems$tri)]        
    } else {
        print("Invalid units, options are 'll' or 'm'.")
        return()
    }
    #Do the actual plotting
    n.cols <- 100
    cols.idx <- floor((att - zlim[1]) / (zlim[2] - zlim[1]) * n.cols)
    cols.idx[cols.idx <= 0] <- 1
    cols.idx[cols.idx > n.cols] <- n.cols
    cols <- col(n.cols)[cols.idx]
    cut.poly <- function(x, n.sides=3) {
        n <- length(x) %/% n.sides
        polys <- rep(NA, n*n.sides)
        polys[rep(c(rep(T, n.sides), F), n)] <- x
        return(polys)
    }
    par(mar=c(5.1, 7.1, 0, 0))
    if(!add)
        plot(0, xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),
             xlab = "Longitude",
             ylab = "Latitude",
             cex.lab=2)
    polygon(cut.poly(x), cut.poly(y), col=cols, border=NA)
    cols <- col(n.cols)
    legend(min(x), max(y),
           legend=c(round(zlim[2], 2), rep("", n.cols-2), round(zlim[1], 2)),
           bt="n",
           col=rev(cols),
           y.intersp=10/n.cols,
           pch=15,
           cex=2)
#    plot(kimbe.grid$elems$x, att, col=cols)
#    plot(att, x, type='l')
}
