#'
#' Plots etopo bathymetry as colored contour
#'
#' \param lat Length 2 vector of min/max lat
#' \param lon Length 2 vector of min/max lon
#' \param res Resolution of plot in minutes
library(marmap)
source("~/Dropbox/clownfish/trunk/R/bathy.colors.R")
plot.bathy <- function(lat, lon, res=2) {    
    lon.min <- lon[1]
    lon.max <- lon[2]
    lat.min <- lat[1]
    lat.max <- lat[2]
    
    dat <- getNOAA.bathy(lon.min, lon.max, lat.min, lat.max, res)
    udat <- unclass(dat)
    
    water <- land <- udat
    water[water > 0] <- NA
    land[land < 0] <- NA
    image(x=seq(lon.min, lon.max, len=nrow(water)),
          y=seq(lat.min, lat.max, len=ncol(water)),
          water,
          col=rev(bathy.colors(2500)), add=F,
          xlab="Longitude", ylab="Latitude")
    image(x=seq(lon.min, lon.max, len=nrow(water)),
          y=seq(lat.min, lat.max, len=ncol(water)),
          land, col=terrain.colors(1000), add=T)
    contour(x=seq(lon.min, lon.max, len=nrow(water)),
            y=seq(lat.min, lat.max, len=ncol(water)),
            water, nlevels=25, add=T)
    contour(x=seq(lon.min, lon.max, len=nrow(water)),
            y=seq(lat.min, lat.max, len=ncol(water)),
            land, nlevels=3, add=T)
}
