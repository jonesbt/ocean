## This file contains a script to calculate the FTLE from FISCM particle
#  tracks under the assumption that FISCM was run with 5 particles per cell.


#Load the data
library(ncdf)
ncid <- open.ncdf("~/Desktop/fiscm_group_001.nc")
x.all <- get.var.ncdf(ncid, "x")
y.all <- get.var.ncdf(ncid, "y")

toi <- 144 #hours, (6 days) (time of interest)
x <- x.all[,toi]
y <- y.all[,toi]

source("~/Dropbox/clownfish/trunk/R/plot.att.R")
source("~/Dropbox/clownfish/trunk/R/bathy.colors.R")
load("~/joint_program/kimbe_fvcom/kimbe_grid.Rda")
plot.att(kimbe.grid, y[1:60998])

max_lambda <- vector(len=60998)
for(i in 1:60998) {
	mat <- matrix(c( (x[i+60998] - x[i+121996]) / 200, (x[i+182995] - x[i+243993]) / 200,
					 (y[i+60998] - y[i+121996]) / 200, (y[i+182995] - y[i+243993]) / 200),
		2, 2, byrow=T)
	if(sum(is.na(mat)) == 0)
		max_lambda[i] <- max(Re(eigen(t(mat) %*% mat)$values))
}
ftle <- log(max_lambda + 0.0001) / 2*toi

#We need to resort the ftle to match the order of kimbe.grid
x.order <- x.all[1:60998,1]
ftle <- ftle[order(x.order)]
x.aligned <- kimbe.grid$elems$x
x.aligned <- cbind(1:60998, x.aligned)
x.aligned <- x.aligned[order(x.aligned[,2]),]
ftle <- ftle[order(x.aligned[,1])]

cols <- function(n) {return(c(heat.colors(n/2), bathy.colors(n/2)))}
plot.att(kimbe.grid, ftle, col=cols)
