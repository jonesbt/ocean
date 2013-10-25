source("~/Dropbox/clownfish/trunk/R/quiver.R")
quiver.mat <- function(x=NA, y=NA, u, v, length=0.25, scale=0.01, add=F) {
    if(is.na(x[1]))
        x <- expand.grid(1:dim(u)[1], 1:dim(u)[2])[,1]
    if(is.na(y[1]))
        y <- expand.grid(1:dim(v)[1], 1:dim(v)[2])[,2]
    quiver(as.vector(x), as.vector(y), as.vector(u), as.vector(v),
           length, scale, col=col, add)
}

