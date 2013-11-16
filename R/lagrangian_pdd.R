## Given a grid, vector of x coordinates, and vector of y coordinates, this
## function plots the Lagrangian probability density distribution as described
## in Simons et al 2013.

bin.data <- function(x, y, grid) {
    out <- matrix(.C('R_bin_data', PACKAGE='ocean',
                     as.double(x), as.double(y), as.integer(length(x)),
                     as.double(grid$x), as.double(grid$y),
                     as.integer(nrow(grid$data)), as.integer(ncol(grid$data)),
                     data=as.integer(as.vector(grid$data)))$data,
                  nrow(grid$data))
    return(out)
}

lagrangian.pdd <- function(grid, x, y, proj='', npart.released=length(x),
                           res=c(1000,1000), log=F,
                           xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),
                           zlim=NA) {
    ## Create a regular grid
    grd <- list(x=seq(xlim[1], xlim[2], by=res[1]),
                y=seq(ylim[1], ylim[2], by=res[2]))
    grd$x.cent <- rep(0, length(grd$x) - 1)
    for(i in seq(length(grd$x) - 1))
      grd$x.cent[i] <- mean(c(grd$x[i], grd$x[i + 1]))
    grd$y.cent <- rep(0, length(grd$y) - 1)
    for(i in seq(length(grd$y) - 1))
      grd$y.cent[i] <- mean(c(grd$y[i], grd$y[i + 1]))
    grd$data <- matrix(0, nrow=length(grd$x.cent), ncol=length(grd$y.cent))
    ## Bin the data into the grid
    grd$data <- bin.data(x, y, grd)
    ## Rescale by the number of particles released
    grd$data <- grd$data / npart.released
    ## Apply a 2D Gaussian filter with a 5km std dev
    ## TODO Project the data, filter, then deproject
    grd$data <- filter2d(grd$data, 5)
    
    if(log)
        grd$data <- matrix(log10(grd$data + 1e-12), nrow(grd$data))
    if(is.na(zlim[1]))
        zlim <- c(min(grd$data), max(grd$data))
    
    ## Project the grid x,y into lat/lon
    if(proj == '') {
        units <- 'm'
    } else {
        x.proj <- c(grd$x, rep(grd$x[1], length(grd$y)))
        y.proj <- c(rep(grd$y[1], length(grd$x)), grd$y)
        p <- project(data.frame(x=x.proj, y=y.proj),
                     proj=proj, inverse=T)
        grd$x <- p$x[seq(length(grd$x))]
        grd$y <- p$y[length(p$y) - rev(seq(length(grd$y))) + 1]
        
        x.proj <- c(grd$x.cent, rep(grd$x.cent[1], length(grd$y.cent)))
        y.proj <- c(rep(grd$y.cent[1], length(grd$x.cent)), grd$y.cent)
        p <- project(data.frame(x=x.proj, y=y.proj), proj=proj, inverse=T)
        grd$x.cent <- p$x[seq(length(grd$x.cent))]
        grd$y.cent <- p$y[length(p$y) - rev(seq(length(grd$y.cent))) + 1]
        units <- 'll'
    }

    ## Set up a land mask
    mask <- expand.grid(x=grd$x.cent, y=grd$y.cent)
    mask$on.grid <- is.in.grid(mask$x, mask$y, grid, units=units)
    grd$mask <- matrix(mask$on.grid, nrow(grd$data))
    grd$data[!grd$mask] <- NA
    rm(mask)
    
    ## Do the actual plotting
    image(matrix(1, 1, 1), xlim=xlim, ylim=ylim,
          col='grey', xlab='Longitude', ylab='Latitude')
    blue.colors <- function(n) return(rep('blue', n))
    att.plot(grid, att=c(1, rep(0, length(grid$nodes$x) - 1)),
                   col=blue.colors, add=T, border.col='blue', legend=F,
                   units=units)
    image(grd$data, x=grd$x.cent, y=grd$y.cent, col=jet.colors(12), add=T,
          zlim=zlim)
    return(grd)
}
