library(ncdf)
source("load.fortran.R")
#Load the data
dat <- load.fortran("~/Desktop/inputfiles_etopo1/20101001_20110501_hfx.dat", "float")
#De-interlace the data
times <- rep(0, length(dat))
var1 <- var2 <- matrix(NA, length(dat[[2]])/2, length(dat))
for(i in seq(2, length(dat), by=2)) {
    times[(i/2)] <- dat[[i-1]]
    var1[,i/2] <- dat[[i]][seq(1, length(dat[[i]]), by=2)]
    var2[,i/2] <- dat[[i]][seq(2, length(dat[[i]]), by=2)]
}

ncid <- open.ncdf("~/Desktop/fvcom_input/kb2011_hfx.nc", write=T)
ncid
Itime <- get.var.ncdf(ncid, "Itime")
Var1 <- get.var.ncdf(ncid, "short_wave")
Var2 <- get.var.ncdf(ncid, "net_heat_flux")


shift <- (92 * 4) # Offset from 01 Oct 2010 to 01 Jan 2011
for(i in 1:469) {
    Var1[,i] <- var2[,i + shift]
    Var2[,i] <- var1[,i + shift]
}

put.var.ncdf(ncid, "short_wave", vals=Var1)
put.var.ncdf(ncid, "net_heat_flux", vals=Var2)
close.ncdf(ncid)
