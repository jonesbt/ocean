## Loads the contents of a NetCDF file into the environment passed in
#  (default global). Warning: if the name of any variable in envir matches
#  that of one in the file passed in, it will be overwritten.

load_ncdf <- function(file, out_file=NULL, envir=globalenv()) {
    require(ncdf)
    if(is.character(file)) {
        ncid <- open.ncdf(file)
    } else {
        ncid <- file
    }
    for(var in names(ncid$var))
        eval(parse(text=sprintf('assign("%s", get.var.ncdf(ncid, "%s"),
                                 envir=envir)', var, var)))
    close.ncdf(ncid)
}
