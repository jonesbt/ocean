## Given a grid, vector of x coordinates, and vector of y coordinates, this
## function plots the Lagrangian probability density distribution as described
## in Simons et al 2013.
library(proj4)

lagrangian.pdd <- function(grid, x, y, npart.released, res=c(1,1),
                           xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),
                           proj) {
    ## Create a regular grid
    grd <- list(x=seq(xlim[1], xlim[2], by=res[1]),
                y=seq(ylim[1], ylim[2], by=res[2]))
    grd$x.cent <- grd$y.cent <- rep(0, length(grd$x) - 1)
    for(i in seq(length(grd$x) - 1)) {
      grd$x.cent[i] <- mean(grd$x[i], grd$x[i + 1])
      grd$y.cent[i] <- mean(grd$y[i], grd$y[i + 1])
    }
    grd$data <- matrix(0, nrow=length(grd$x.cent), ncol=length(grd$y.cent))
    ## Bin the data into the grid
    for(i in seq(nrow(grd$data)))
        for(j in seq(ncol(grd$data)))
            grd$data[i,j] <- sum((x > grd$x[i]) & (x < grd$x[i + 1]) &
                                 (y > grd$y[j]) & (y < grd$y[j + 1]))
    ## Rescale by the number of particles released
    grd$data <- grd$data / npart.released
    ## Apply a 2D Gaussian filter with a 5km std dev
    ## TODO Project the data, filter, then deproject
    grd$data <- filter2d(grd$data, 1)

    ## Project the grid x,y into lat/lon
    proj <- project(cbind(grd$x, grd$y),
                    proj=c("+proj=utm", "+lat_0=0", "+lon_0=144",
                    "+datum=WGS84"), inverse=T)
    grd$x <- proj[,1]
    grd$y <- proj[,2]
    
    ## Set up a land mask
    ## TODO Use center points of cells
    mask <- expand.grid(x=grd$x.cent, y=grd$y.cent)
    mask$on.grid <- is.in.grid(mask$x, mask$y, grid)
    grd$mask <- matrix(mask$on.grid, nrow(grd$data))
    grd$data[!grd$mask] <- NA
    rm(mask)
    
    ## Do the actual plotting
    ## TODO Match color scheme to spaghetti plots
    image(matrix(1, 1, 1), xlim=c(150, 151.25), ylim=c(-5.6, -4.5),
          col='grey', xlab='Longitude', ylab='Latitude')
    white.colors <- function(n) return(rep('white', n))
    plot.att(grid, att=c(1, rep(0, 31288)), col=white.colors, add=T)
    image(grd$data, x=grd$x[-1], y=grd$y[-1], col=jet.colors(12), add=T)
    contour(grd$data, add=T, x=grd$x[-1], y=grd$y[-1])
}

debug <- function() {
load('~/joint_program/kimbe_fvcom/kimbe_grid.Rda')
source('library_ocean.R')
library(MASS)
samp <- mvrnorm(100000, c(150.6, -5.0), diag(c(0.25, 0.25)))
samp <- mvrnorm(100000, c(925, -575), diag(c(500, 500)))
x <- samp[,1]
y <- samp[,2]
plot(x,y, cex=0.25, pch=15)
debug(lagrangian.pdd)
lagrangian.pdd(kimbe.grid, x, y, length(x))
}
rm(debug)
