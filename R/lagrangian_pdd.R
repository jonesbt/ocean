## Given a grid, vector of x coordinates, and vector of y coordinates, this
## function plots the Lagrangian probability density distribution as described
## in Simons et al 2013.
library(proj4)

bin.data <- function(x, y, grid) {
    dyn.load('lagrangian_pdd.so')
    out <- matrix(.C('R_bin_data',
                     as.double(x), as.double(y), as.integer(length(x)),
                     as.double(grid$x), as.double(grid$y),
                     as.integer(nrow(grid$data)), as.integer(ncol(grid$data)),
                     data=as.integer(as.vector(grid$data)))$data,
                  nrow(grid$data))
    dyn.unload('lagrangian_pdd.so')
    return(out)
}

lagrangian.pdd <- function(grid, x, y, npart.released=length(x), res=c(1000,1000),
                           xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),
                           proj) {
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
    print('Binning data')
    #for(i in seq(nrow(grd$data)))
    #    for(j in seq(ncol(grd$data)))
    #        grd$data[i,j] <- sum((x > grd$x[i]) & (x < grd$x[i + 1]) &
    #                             (y > grd$y[j]) & (y < grd$y[j + 1]))
    grd$data <- bin.data(x, y, grd)
    ## Rescale by the number of particles released
    grd$data <- grd$data / npart.released
    ## Apply a 2D Gaussian filter with a 5km std dev
    ## TODO Project the data, filter, then deproject
    print('Filtering data')
    grd$data <- filter2d(grd$data, 5)

    ## Project the grid x,y into lat/lon
    print('Projecting data')
    x.proj <- c(grd$x, rep(grd$x[1], length(grd$y)))
    y.proj <- c(rep(grd$y[1], length(grd$x)), grd$y)
    proj <- project(data.frame(x=x.proj, y=y.proj),
                    proj=c("+proj=utm", "+lat_0=0", "+lon_0=144",
                    "+datum=WGS84"), inverse=T)
    grd$x <- proj$x[seq(length(grd$x))]
    grd$y <- proj$y[length(proj$y) - rev(seq(length(grd$y))) + 1]
    
    x.proj <- c(grd$x.cent, rep(grd$x.cent[1], length(grd$y.cent)))
    y.proj <- c(rep(grd$y.cent[1], length(grd$x.cent)), grd$y.cent)
    proj <- project(data.frame(x=x.proj, y=y.proj),
                    proj=c("+proj=utm", "+lat_0=0", "+lon_0=144",
                    "+datum=WGS84"), inverse=T)
    grd$x.cent <- proj$x[seq(length(grd$x.cent))]
    grd$y.cent <- proj$y[length(proj$y) - rev(seq(length(grd$y.cent))) + 1]

    ## Set up a land mask
    print('Setting up mask')
    mask <- expand.grid(x=grd$x.cent, y=grd$y.cent)
    mask$on.grid <- is.in.grid(mask$x, mask$y, grid)
    grd$mask <- matrix(mask$on.grid, nrow(grd$data))
    grd$data[!grd$mask] <- NA
    rm(mask)
    
    ## Do the actual plotting
    ## TODO Match color scheme to spaghetti plots
    print('Plotting results')
    image(matrix(1, 1, 1), xlim=c(150, 151.25), ylim=c(-5.6, -4.5),
          col='grey', xlab='Longitude', ylab='Latitude')
    blue.colors <- function(n) return(rep('blue', n))
    plot.att(grid, att=c(1, rep(0, 31288)), col=blue.colors, add=T, border.col='blue', legend=F)
    image(grd$data, x=grd$x.cent, y=grd$y.cent, col=jet.colors(12), add=T)
    #contour(grd$data, add=T, x=grd$x.cent, y=grd$y.cent)
    return(grd)
}

debug <- function() {
load('~/joint_program/kimbe_fvcom/kimbe_grid.Rda')
source('library_ocean.R')
library(MASS)
xmeans <- rnorm(10, 900e3, 3e4)
ymeans <- rnorm(10, -575e3, 3e4)
summary(xmeans)
plot.att(kimbe.grid, att=c(1, rep(0, 31288)), col=blue.colors, border.col='blue', units='m', legend=F)
points(xmeans, ymeans)
x <- y <- numeric(0)
for(i in seq(length(xmeans))) {
    samp <- mvrnorm(25000, c(xmeans[i], ymeans[i]), diag(c(1e8, 1e8)))
    x <- c(x, samp[,1])
    y <- c(y, samp[,2])
}
length(x)
points(x, y, cex=0.1)
undebug(lagrangian.pdd)
grd <- lagrangian.pdd(kimbe.grid, x, y, res=c(1000,1000))
length(grd$y.cent)
}
rm(debug)
