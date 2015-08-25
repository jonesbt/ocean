#' This file contains support functions for Nemo.

library(ncdf4)

#' Writes a data.frame to a Nemo initialization file.
#' netcdf [FILENAME] {
#' dimensions:
#' particle = 10 ;
#' variables:
#' float z(particle) ;
#' float y(particle) ;
#' float x(particle) ;
#' float release_time(particle) ;
#' }
#' 
write.nemo = function(data, filename) {
    # Create the dimensions and variables.
    dim_particle = ncdim_def('particle', '', as.integer(seq(nrow(data))),
                             create_dimvar=FALSE)
    var_x = ncvar_def('x', 'm', list(dim_particle))
    var_y = ncvar_def('y', 'm', list(dim_particle))
    var_z = ncvar_def('z', 'm', list(dim_particle))
    var_release_time = ncvar_def('release_time', '', list(dim_particle))
    # Create the file and add the data.
    ncid = nc_create(filename, list(var_x, var_y, var_z, var_release_time))
    ncvar_put(ncid, 'x', data$x)
    ncvar_put(ncid, 'y', data$y)
    ncvar_put(ncid, 'z', data$z)
    ncvar_put(ncid, 'release_time', data$time)
    nc_close(ncid)
}

#' Reads a Nemo initialization file into a data.frame.
read.nemo = function(filename) {
    ncid = nc_open(filename)
    dat = data.frame(
        x = ncvar_get(ncid, 'x'),
        y = ncvar_get(ncid, 'y'),
        z = ncvar_get(ncid, 'z'),
        time = ncvar_get(ncid, 'release_time')
    )
    nc_close(ncid)
    return(dat)
}
