library(ncdf)
source("load.fortran.R")
#Load the data
dat <- load.fortran("~/Desktop/inputfiles_etopo1/20101001_20110501_evp.dat", "float")
#De-interlace the data
times <- rep(0, length(dat))
var1 <- var2 <- matrix(NA, length(dat[[2]])/2, length(dat))
for(i in seq(2, length(dat), by=2)) {
    times[(i/2)] <- dat[[i-1]]
    var1[,i/2] <- dat[[i]][seq(1, length(dat[[i]]), by=2)]
    var2[,i/2] <- dat[[i]][seq(2, length(dat[[i]]), by=2)]
}

ncid <- open.ncdf("~/Desktop/fvcom_input/kb2011_evp.nc", write=T)
ncid
Itime <- get.var.ncdf(ncid, "Itime")
Var1 <- get.var.ncdf(ncid, "evap")
Var2 <- get.var.ncdf(ncid, "precip")

dim(Var1)
dim(var1)

##Note: NetCDF file uses hourly resolution, Fortran binary uses 6 hour

##Interpolate evap/precip to hourly resolution
t.st <- 92 * 4
t.end <- t.st + (max(Itime) - min(Itime))
var1.interp <- matrix(0, nrow(Var1), ncol(Var1))
var2.interp <- matrix(0, nrow(Var2), ncol(Var2))
for(i in seq(nrow(var1))) {
    Var1[i,] <- approx(times, var1[i,],
                              seq(t.st, t.end, by = 1/24))$y
    Var2[i,] <- approx(times, var1[i,],
                              seq(t.st, t.end, by = 1/24))$y
}

put.var.ncdf(ncid, "evap", vals=Var1)
put.var.ncdf(ncid, "precip", vals=Var2)
close.ncdf(ncid)
