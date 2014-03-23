#!/usr/local/bin/Rscript

library(compiler)
library(ncdf4)

parse_args <- function(argv) {}
create_flow_file <- function(times, x, y, fname, Gamma, R, k) {}
generate_eddy <- function(t, x, y, Gamma, R, k) {}

create_initial_locations <- function(x, y, res, fname) {
    x <- seq(min(x), max(x), by=res)
    y <- seq(min(y), max(y), by=res)
    grd <- expand.grid(x=x, y=y)
    write.table(grd, file=sprintf('%s.txt', fname), row.names=F, col.names=F)
}

create_flow_field <- function(times, x, y, Gamma, R, k, fname) {}

main <- function(argv) {
    args <- parse_args(argv)
    with(args, create_initial_locations(x, y, res, fname))
    with(args, create_flow_field())
}

# User defined parameters
xlim <- c(0, 1000) * 1000
ylim <- c(0, 500) * 1000
res <- 2 * 1000
t.res <- 1
r <- 150 * 1000
k <- 0.1 * 3600
max.eddy.vel <- 0.25 * 3600
gamma <- max.eddy.vel * 2 * pi * r

# Set up the grid
x <- seq(xlim[1], xlim[2], by=res)
y <- seq(ylim[1], ylim[2], by=res)
times <- seq(1, 24 * 60, by=t.res)

# Create the initalization file
create_initial_locations(x, y, res, sprintf('../data/rankine_%03d', res))

# Create the output file
dim.time <- ncdim_def('time', '', seq(along=times), unlim=T,
                      create_dimvar=F)
dim.x <- ncdim_def('x', '', seq(along=x), create_dimvar=F)
dim.y <- ncdim_def('y', '', seq(along=y), create_dimvar=F)
var.time <- ncvar_def('time', 'time', dim.time)
var.x <- ncvar_def('x', 'm', dim.x)
var.y <- ncvar_def('y', 'm', dim.y)
var.u <- ncvar_def('u', 'm / hr', list(dim.y, dim.x, dim.time))
var.v <- ncvar_def('v', 'm / hr', list(dim.y, dim.x, dim.time))
ncid <- nc_create(sprintf('../data/rankine_%03d.nc', res),
                  list(var.time, var.x, var.y, var.u, var.v))
ncvar_put(ncid, 'time', times, start=1, count=length(times))
ncvar_put(ncid, 'x', x)
ncvar_put(ncid, 'y', y)

### Add the time dependent data
generate.eddy <- function(x, y, t) {
    u <- matrix(0, nrow=length(x), ncol=length(y))
    v <- matrix(0, nrow=length(x), ncol=length(y))

    x.mid <- 1.1 * r + k * cur.t
    y.mid <- mean(ylim)
    for(i in seq(along=x)) {
        for(j in seq(along=y)) {
            dx <- x[i] - x.mid
            dy <- y[j] - y.mid
            rad <- sqrt(dx^2 + dy^2)
            if(rad < r) {
                vel <- rad / r * max.eddy.vel
            } else {
                vel <- gamma / (2 * pi * rad)
            }
            if(dx == 0 && dy == 0) {
                u0 <- 0
                v0 <- 0
            } else if(dx == 0) {
                u0 <- vel
                v0 <- 0
            } else if(dy == 0) {
                u0 <- 0
                v0 <- vel
            } else {
                u0 = vel * sqrt(1 / (1 + (dx / dy)^2))
                v0 = vel * sqrt(1 / (1 + (dy / dx)^2))
            }
            if(dy < 0)
                u0 <- -u0
            if(dx > 0)
                v0 <- -v0
            u[i,j] <- u0 + k
            v[i,j] <- v0
        }
    }
    return(list(u=u, v=v))
}

ge <- cmpfun(generate.eddy)
st <- c(1, 1, 1)
ct <- c(-1, -1, 1)
for(cur.t in times) {
    print(paste(cur.t, date()))
    tmp <- ge(x, y, cur.t)
    u <- t(tmp$u)
    v <- t(tmp$v)
    ncvar_put(ncid, 'u', u, st, ct)
    ncvar_put(ncid, 'v', v, st, ct)
    nc_sync(ncid)
    st[3] = st[3] + 1
}

# Close the file
nc_close(ncid)
