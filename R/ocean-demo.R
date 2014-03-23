library(ncdf4)
library(ocean)
library(proj4)
source('fvcom_grid.R')
grid <- fvcom.grid('../../nemo/data/kimbe/kb2008_0002.nc',
                   proj='+proj=utm +lat_0=0 +lon_0=144 +datum=WGS84')

get.nnodes(grid)
get.nelems(grid)
e <- get.elems(grid)
n <- get.nodes(grid)
#interp(grid, 1)
#get.depth(grid)
image(grid, units='ll')
x <- c(rnorm(10000, 800000, 20000), rnorm(10000, 850000, 20000), rnorm(10000, 875000, 5000))
y <- c(rnorm(10000, -500000, 20000), rnorm(10000, -550000, 20000), rnorm(10000, -600000, 20000))
grid <- fvcom.grid('../../nemo/data/kimbe/kb2008_0002.nc',
                   proj='+proj=utm +lat_0=0 +lon_0=144 +datum=WGS84')
tmp <- pdd(grid, xy=data.frame(x=x, y=y), res=5000, log=F,
           xlim=c(8e5, 1e6), ylim=c(-6.5e5, -4e5))
npart = 5
ntimes = 100
x <- matrix(rnorm(npart * ntimes, 150.5, 0.25), nrow=npart, ncol=ntimes)
y <- matrix(rnorm(npart * ntimes, -5, 0.25), nrow=npart, ncol=ntimes)
for(i in seq(2, ntimes)) {
    x[,i] <- x[,i-1] + rnorm(npart, 0, 0.01)
    y[,i] <- y[,i-1] + rnorm(npart, 0, 0.01)
}
source('fvcom_grid.R')
plot(grid, xy=list(x=t(x), y=t(y)), lty=1, col=seq(5))
