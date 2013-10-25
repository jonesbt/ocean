library(ncdf)
source("load.fortran.R")
#Load the data
dat <- load.fortran("~/Desktop/inputfiles_etopo1/20101001_20110501_wnd.dat", "float")
#De-interlace the data
times <- rep(0, length(dat))
u10 <- v10 <- matrix(NA, length(dat[[2]])/2, length(dat))
for(i in seq(2, length(dat), by=2)) {
    times[(i/2)] <- dat[[i-1]]
    u10[,i/2] <- dat[[i]][seq(1, length(dat[[i]]), by=2)]
    v10[,i/2] <- dat[[i]][seq(2, length(dat[[i]]), by=2)]
}

ncid <- open.ncdf("~/Desktop/fvcom_input/kb2011_wnd.nc", write=T)
ncid
Itime <- get.var.ncdf(ncid, "Itime")
U10 <- get.var.ncdf(ncid, "U10")
V10 <- get.var.ncdf(ncid, "V10")

summary(v10[,1:469])

shift <- (92 * 4) # Offset from 01 Oct 2010 to 01 Jan 2011
for(i in 1:469) {
    U10[,i] <- u10[,i + shift]
    V10[,i] <- v10[,i + shift]
}

put.var.ncdf(ncid, "U10", vals=U10)
put.var.ncdf(ncid, "V10", vals=V10)
close.ncdf(ncid)
