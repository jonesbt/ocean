## Given a grid, vector of x coordinates, and vector of y coordinates, this
## function plots the Lagrangian probability density distribution as described
## in Simons et al 2013.
library(mvtnorm)
library(mnormt)
source('library_ocean.R')

lagrangian.pdd <- function(grid, x, y, npart.released, res=c(100,100),
                           xlim=c(min(x), max(x)), ylim=c(min(y), max(y)),
                           proj) {
    ## Create a regular grid
    grd <- list(x=seq(xlim[1], xlim[2], len=res[1]),
                y=seq(ylim[1], ylim[2], len=res[2]))
    grd$data <- matrix(0, nrow=length(grd$x) - 1, ncol=length(grd$y) - 1)
    ## Bin the data into the grid
    print('About to bin')
    for(i in seq(nrow(grd$data)))
        for(j in seq(ncol(grd$data)))
            grd$data[i,j] <- sum((x > grd$x[i]) & (x < grd$x[i + 1]) &
                                 (y > grd$y[j]) & (y < grd$y[j + 1]))
    ## Rescale by the number of particles released
    grd$data <- grd$data / npart.released
    ## Apply a 2D Gaussian filter with a 5km std dev
    ## TODO Project the data, filter, then deproject
    print('About to filter')
    source('filter2d.R')
    grd.t2 <- grd
    grd <- grd.t2
    grd$data <- filter2d(grd$data, 1)
    image(grd$data, col=jet.colors(12), x=grd$x, y=grd$y)
    contour(grd$data, add=T, x=grd$x, y=grd$y)
    sum(grd$data)
    ## Do the actual plotting
    ## TODO Set up a land mask
    ## TODO Match color scheme to spaghetti plots
    plot.att(grid, att=c(1, rep(2, 31288)),
             xlim=xlim, ylim=ylim, col=bathy.colors)
    plot.att(grid, att=c(1, rep(2, 31288)),
             xlim=c(150, 151.25), ylim=c(-5.6, -4.5), col=bathy.colors)
    contour(grd$data, x=grd$x[-1], y=grd$y[-1], add=T)
}

load('~/joint_program/kimbe_fvcom/kimbe_grid.Rda')
source('library_ocean.R')
library(MASS)
plot(samp)
samp <- mvrnorm(10000, c(150.6, -5.0), diag(c(0.25, 0.25)))
x <- samp[,1]
y <- samp[,2]
plot(x,y, cex=0.25, pch=15)
debug(lagrangian.pdd)
lagrangian.pdd(kimbe.grid, x, y, 10000, res=c(25, 25),
               xlim=c(149, 152), ylim=c(-6, -4))
plot(x,y)
